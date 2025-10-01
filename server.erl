-module(server).
-export([start/1, stop/1, await_message/2]).

-record(server_st, {
    channels
}).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, #server_st{channels = #{}}, fun await_message/2).

await_message(State, Data) ->
    case Data of
        {join, Pid, Channel} ->
            case has_user_joined_channel(State, Channel, Pid) of
                true ->
                    {reply, {error, user_already_joined, "You have already joined this channel"},
                        State};
                _ ->
                    NewState = handle(join, {State, Pid, Channel}),
                    {reply, ok, NewState}
            end;
        {message_send, Pid, Channel, Nick, Msg} ->
            case has_user_joined_channel(State, Channel, Pid) of
                true ->
                    handle(message_send, {State, Pid, Channel, Nick, Msg}),
                    {reply, ok, State};
                false ->
                    {reply, {error, user_not_joined, "You have not joined this channel"}, State};
                error ->
                    {reply, {error, server_not_reached, "This channel does not exist"}, State}
            end;
        {leave, Pid, Channel} ->
            case has_user_joined_channel(State, Channel, Pid) of
                true ->
                    NewState = handle(leave, {State, Pid, Channel}),
                    {reply, ok, NewState};
                false ->
                    {reply, {error, user_not_joined, "You have not joined this channel"}, State};
                error ->
                    {reply, ok, State}
            end;
        _ ->
            {reply, ok, State}
    end.

handle(join, {State, Pid, Channel}) ->
    case maps:is_key(Channel, State#server_st.channels) of
        true ->
            Value = maps:get(Channel, State#server_st.channels),
            Channels = maps:put(Channel, [Pid | Value], State#server_st.channels),
            State#server_st{channels = Channels};
        false ->
            Channels = maps:put(Channel, [Pid], State#server_st.channels),
            State#server_st{channels = Channels}
    end;
handle(leave, {State, Pid, Channel}) ->
    Pids = maps:get(Channel, State#server_st.channels),
    NewPids = [X || X <- Pids, X =/= Pid],
    New_Channels = maps:put(Channel, NewPids, State#server_st.channels),
    State#server_st{channels = New_Channels};
handle(message_send, {State, Pid, Channel, Nick, Msg}) ->
    Users = maps:get(Channel, State#server_st.channels),
    forward_message(Users, Pid, {message_receive, Channel, Nick, Msg}),
    State.

forward_message([User | Users], Pid, Package) ->
    case User == Pid of
        true ->
            io:format("Same Reciever"),
            forward_message(Users, Pid, Package);
        false ->
            io:format("Different Reciever ~p~n ~p~n ", [User, Pid]),
            genserver:request(User, Package),
            forward_message(Users, Pid, Package)
    end;
forward_message([], _, _) ->
    ok.

has_user_joined_channel(State, Channel, User) ->
    case maps:is_key(Channel, State#server_st.channels) of
        true ->
            Users = maps:get(Channel, State#server_st.channels),
            Result = contains(Users, User),
            Result;
        false ->
            error
    end.

contains([User | Users], Target) ->
    Target == User orelse contains(Users, Target);
contains([], _) ->
    false.
% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom),
    ok.
