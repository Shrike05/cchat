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

%The server will create threads for channels and coordinate when a user joins a channel
%The Server will also handle nickname changes
server(State, Data) ->
    case Data of
        %In the case of a join
        {join, Pid, Nick, Channel} ->
            handle_join(
                lists:member(Channel, State#server_st.channels), State, Nick, Pid, Channel
            );
        %Return all the active channels
        {get_channels} ->
            {reply, State#server_st.channels, State};
        %Check whether a nickname is available
        {new_nick, User, NewNick} ->
            Users = State#server_st.users,
            case lists:member(NewNick, Users) of
                true ->
                    %If a username is not available then return nick_taken 
                    {reply, {error, nick_taken, "Nickname has been taken"}, State};
                false ->
                    %If a username is available then return ok and update the users list 
                    NewUsers = replace(Users, User, NewNick),
                    {reply, ok, State#server_st{users = NewUsers}}
            end;
        _ ->
            {reply, ok, State}
    end.

%In the case where the channel does not already exist, create one
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
%In the case where the channel does exist, forward the join request
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

%The channel thread will be another instance of genserver launched by the server
channel(State, Data) ->
    case Data of
        %Join request, that has been forwarded from the server
        {join, Pid} ->
            case lists:member(Pid, State#channel_st.users) of
                true ->
                    {reply, {error, user_already_joined, "You have already joined this channel"},
                        State};
                false ->
                    Users = State#channel_st.users,
                    {reply, ok, State#channel_st{users = [Pid | Users]}}
            end;
        %In case a user wants to leave the channel
        {leave, Pid} ->
            case lists:member(Pid, State#channel_st.users) of
                true ->
                    NewUsers = lists:delete(Pid, State#channel_st.users ),
                    {reply, ok, State#channel_st{users = NewUsers}};
                false ->
                    {reply, {error, user_not_joined, "Can't leave a channel you haven't joined"},
                        State}
            end;
        %Forward a message from the sending user to all other users in the channel
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

%Send the package to the reciever and filter out the sender
send_request(Reciever, Sender, Package) ->
    case (Reciever =/= Sender) of
        true ->
            genserver:request(Reciever, Package);
        false ->
            []
    end.
 
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
