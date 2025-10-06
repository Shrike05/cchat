-module(server).
-export([start/1, stop/1, server/2, channel/2]).

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
        ServerAtom, #server_st{server_atom = ServerAtom, users = [], channels = []}, fun server/2
    ).

server(State, Data) ->
    case Data of
        {join, Pid, Nick, Channel} ->
            handle_join(
                lists:member(Channel, State#server_st.channels), State, Nick, Pid, Channel
            );
        {join, Nick} ->
            Users = State#server_st.users,
            {reply, ok, State#server_st{users = [Nick | Users]}};
        {get_channels} ->
            {reply, State#server_st.channels, State};
        {new_nick, User, NewNick} ->
            Users = State#server_st.users,
            case lists:member(NewNick, Users) of
                true ->
                    {reply, {error, nick_taken, "Nickname has been taken"}, State};
                false ->
                    NewUsers = replace(Users, User, NewNick),
                    {reply, ok, State#server_st{users = NewUsers}}
            end;
        _ ->
            {reply, ok, State}
    end.

handle_join(false, State, Nick, Pid, Channel) ->
    genserver:start(
        get_channel_atom(State#server_st.server_atom, Channel),
        #channel_st{channel = Channel, users = [Pid]},
        fun channel/2
    ),
    Channels = State#server_st.channels,
    Users = State#server_st.users,
    New_State = State#server_st{channels = [Channel | Channels], users = [Nick | Users]},
    {reply, ok, New_State};
handle_join(true, State, Nick, Pid, Channel) ->
    try genserver:request(get_channel_atom(State#server_st.server_atom, Channel), {join, Pid}) of
        Reply ->
            io:format("~p~n", [Reply]),
            Users = State#server_st.users,
            New_State = State#server_st{users = [Nick | Users]},
            {reply, Reply, New_State}
    catch
        Error ->
            {reply, Error, State}
    end.

channel(State, Data) ->
    case Data of
        {join, Pid} ->
            case lists:member(Pid, State#channel_st.users) of
                true ->
                    {reply, {error, user_already_joined, "You have already joined this channel"},
                        State};
                false ->
                    Users = State#channel_st.users,
                    {reply, ok, State#channel_st{users = [Pid | Users]}}
            end;
        {leave, Pid} ->
            case lists:member(Pid, State#channel_st.users) of
                true ->
                    NewState = remove_user(State, Pid),
                    {reply, ok, NewState};
                false ->
                    {reply, {error, user_not_joined, "Can't leave a channel you haven't joined"},
                        State}
            end;
        {message_send, SendingUser, Nick, Msg} ->
            case lists:member(SendingUser, State#channel_st.users) of
                true ->
                    Users = State#channel_st.users,
                    Channel = State#channel_st.channel,
                    lists:foreach(
                        fun(Reciever) ->
                            spawn(
                                fun() -> send_request(
                                    Reciever, SendingUser, {message_receive, Channel, Nick, Msg}
                                ) end
                            )
                        end,
                        Users
                    ),
                    {reply, ok, State};
                false ->
                    {reply, {error, user_not_joined, "You have not joined this channel"}, State}
            end
    end.

send_request(Reciever, Sender, Package) ->
    case (Reciever =/= Sender) of
        true ->
            genserver:request(Reciever, Package);
        false ->
            []
    end.

remove_user(ChannelState, Pid) ->
    Pids = ChannelState#channel_st.users,
    NewPids = [X || X <- Pids, X =/= Pid],
    ChannelState#channel_st{users = NewPids}.

replace([User | Users], Target, NewValue) ->
    case User == Target of
        true ->
            [NewValue | Users];
        false ->
            [User | replace(Users, Target, NewValue)]
    end;
replace([], _, _) ->
    [].

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
