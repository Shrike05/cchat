-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    % atom of the GUI process
    gui,
    % nick/username of the client
    nick,
    % atom of the chat server
    server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

make_request(Server, Msg) ->
    make_request_with_custom_error(
        Server, Msg, {error, server_not_reached, "Server not available"}
    ).

make_request_with_custom_error(Server, Msg, NoServerError) ->
    case whereis(Server) of
        undefined ->
            NoServerError;
        _ ->
            try genserver:request(Server, Msg) of
                Reply ->
                    Reply
            catch
                _ ->
                    {error, server_not_reached, "Server not available"}
            end
    end.

% Join channel
handle(St, {join, Channel}) ->
    ChannelAtom = get_channel_atom(St#client_st.server, Channel),
    case whereis(ChannelAtom) of
        undefined ->
            Response = make_request(St#client_st.server, {join, self(), St#client_st.nick, Channel}),
            {reply, Response, St};
        _ ->
            make_request(St#client_st.server, {join, St#client_st.nick}),
            Response = make_request(ChannelAtom, {join, self()}),
            {reply, Response, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    Response = make_request_with_custom_error(
        get_channel_atom(St#client_st.server, Channel),
        {leave, self()},
        ok
    ),
    {reply, Response, St};
% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    Response = make_request(
        get_channel_atom(St#client_st.server, Channel),
        {message_send, self(), St#client_st.nick, Msg}
    ),
    {reply, Response, St};
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    Response = make_request(St#client_st.server, {new_nick, St#client_st.nick, NewNick}),
    io:fwrite("~p~n", [Response]),
    case Response of
        ok ->
            {reply, Response, St#client_st{nick = NewNick}};
        _ ->
            {reply, Response, St}
    end;
% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St};
% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick ++ "> " ++ Msg}),
    {reply, ok, St};
% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St};
% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.

get_channel_atom(ServerAtom, Channel) ->
    list_to_atom(atom_to_list(ServerAtom) ++ Channel).
