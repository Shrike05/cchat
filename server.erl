-module(server).
-export([start/1, stop/1, await_message/2, channel/2]).

-record(server_st, {
    server_atom,
    users,
    channels
}).

-record(channel_st, {
    channel,
    users
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(
        ServerAtom, #server_st{server_atom = ServerAtom, users = #{}, channels = []}, fun await_message/2
    ).

await_message(State, Data) ->
    case Data of
        {join, Pid, Channel} ->
            genserver:start(
                get_channel_atom(State#server_st.server_atom, Channel),
                #channel_st{channel = Channel, users = [Pid]},
                fun channel/2
            ),
            Channels = State#server_st.channels,
            {reply, ok, State#server_st{channels = [Channel | Channels]}};
        {get_channels} ->
            {reply, State#server_st.channels, State};
        _ ->
            {reply, ok, State}
    end.

channel(State, Data) ->
    case Data of
        {join, Pid} ->
            case contains(State#channel_st.users, Pid) of
                true ->
                    {reply, {error, user_already_joined, "You have already joined this channel"},
                        State};
                false ->
                    Users = State#channel_st.users,
                    {reply, ok, State#channel_st{users = [Pid | Users]}}
            end;
        {leave, Pid} ->
            case contains(State#channel_st.users, Pid) of
                true ->
                    NewState = remove_user(State, Pid),
                    {reply, ok, NewState};
                false ->
                    {reply, {error, user_not_joined, "Can't leave a channel you haven't joined"},
                        State}
            end;
        {message_send, Pid, Nick, Msg} ->
            case contains(State#channel_st.users, Pid) of
                true ->
                    io:format("Got A Message ~p~n", [Msg]),
                    send_message(message_send, {State, Pid, Nick, Msg}),
                    {reply, ok, State};
                false ->
                    {reply, {error, user_not_joined, "You have not joined this channel"}, State}
            end
    end.

send_message(message_send, {State, User, Nick, Msg}) ->
    Users = State#channel_st.users,
    Channel = State#channel_st.channel,
    forward_message(Users, User, {message_receive, Channel, Nick, Msg}),
    State.

remove_user(ChannelState, Pid) ->
    Pids = ChannelState#channel_st.users,
    NewPids = [X || X <- Pids, X =/= Pid],
    ChannelState#channel_st{users = NewPids}.

forward_message([User | Users], Pid, Package) ->
    case User == Pid of
        true ->
            forward_message(Users, Pid, Package);
        false ->
            genserver:request(User, Package),
            forward_message(Users, Pid, Package)
    end;
forward_message([], _, _) ->
    ok.

contains([User | Users], Target) ->
    Target == User orelse contains(Users, Target);
contains([], _) ->
    false.

get_channel_atom(ServerAtom, Channel) ->
    list_to_atom(atom_to_list(ServerAtom) ++ Channel).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    Channels = genserver:request(ServerAtom, {get_channels}),
    stop_channels(ServerAtom, Channels),
    genserver:stop(ServerAtom),
    ok.

stop_channels(ServerAtom, [Channel | Channels]) ->
    ChannelAtom = get_channel_atom(ServerAtom, Channel),
    genserver:stop(ChannelAtom),
    stop_channels(ServerAtom, Channels);
stop_channels(_, []) ->
    ok.
