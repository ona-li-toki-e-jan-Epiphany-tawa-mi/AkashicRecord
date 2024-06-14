%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License
%% @private Module for listening for UDP connections from the network and
%% routing them to <b>poltergeist_control_server</b>.

-module(poltergeist_udp_listener).
-behaviour(gen_server).

-export([ start_link/1, init/1, handle_cast/2, handle_call/3 ]).



-record(state, { socket :: gen_udp:socket()
               }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private @param Socket the UDP socket to listen on.
-spec start_link(gen_udp:socket()) -> gen_server:start_ret().
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%% @private @param Socket the UDP socket to listen on.
-spec init(gen_udp:socket()) -> { ok, #state{} }.
init(Socket) ->
    gen_server:cast(self(), listen),
    { ok, #state{ socket = Socket } }.

%% @private listen - start listening on the UDP socket.
%% @see listen/1.
-spec handle_cast(any() | listen, #state{}) -> { noreply, #state{} }.
handle_cast(listen, State = #state{ socket = Socket }) ->
    listen(Socket),
    { noreply, State };
handle_cast(_, State) ->
    { noreply, State }.

%% @private
-spec handle_call(any(), any(), #state{}) -> { noreply, #state{} }.
handle_call(_, _, State) -> { noreply, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% @private Begins listening to the given UDP socket.
-spec listen(gen_udp:socket()) -> no_return().
listen(Socket) ->
    { ok, { _, _, Data } } = gen_udp:recv(Socket, 0),
    handle_packet(Data),
    listen(Socket).

%% @private Accepts a packet from the network, performs basic parsing, and
%% passes it off to the control server.
-spec handle_packet(string()) -> ok.
handle_packet(Data) ->
    case string:tokens(Data, " ") of
        [ Command ] ->
            io:format("INFO: recieved command ~p\n", [ Command ]),
            gen_server:cast(poltergeist_control_server, { runcommand, Command, [] });
        [ Command | Arguments ] ->
            io:format( "INFO: recieved command ~p with arguments ~p\n"
                     , [ Command, Arguments ]),
            gen_server:cast(poltergeist_control_server, { runcommand, Command, Arguments });
        _ ->
            ok
    end.
