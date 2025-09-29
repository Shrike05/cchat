-module(server).
-export([start/1, stop/1, await_message/1]).



% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    Pid = spawn(server, await_message, [#{}]),
    register(ServerAtom, Pid),
    Pid.


await_message(State) ->
    receive
        {join, Pid, Channel} ->
            NewState = handle(join, {State, Pid, Channel}),
            await_message(NewState);

        {message_send, Pid, Channel, Nick, Msg} ->
            %io:format("Got messsage ~p~n", Msg),
            handle(message_send, {State, Pid, Channel, Nick, Msg}),
            await_message(State)
    end.


handle(join, {State, Pid, Channel}) ->    
    case maps:is_key(Channel, State) of
        true ->
            Value = maps:get(Channel, State),
            maps:put(Channel, [Pid | Value], State);
        false ->
            maps:put(Channel, [Pid], State)
    end;


handle(message_send, {State, Pid, Channel, Nick, Msg}) ->
    Users = maps:get(Channel, State),
    forward_message(Users, Pid, {message_receive, Channel, Nick, Msg}).

forward_message([], _, _) -> 
    ok;

% forward_message( [User | Users], Pid, Package) ->
%     User ! Package,
%     forward_message(Users, Pid, Package).

forward_message( [User | Users], Pid, Package) ->
    case User == Pid of
        true ->
            io:format("Same Reciever"),
            forward_message(Users, Pid, Package);
        false ->
            io:format("Different Reciever ~p~n ~p~n ", [User, Pid]),
            User ! Package,
            forward_message(Users, Pid, Package)
    end.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    unregister(ServerAtom).
