%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License
%% @private Bare-bones remote keyboard and mouse server

-module(poltergeist).
-behaviour(application).

-export([ start/2, stop/1 ]).



-define(PASSIVE_SOCKET, { active, false }).

-record(state, { socket :: gen_udp:socket()
               }).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec start(normal, any()) -> { ok, pid(), #state{} }.
start(normal, _) ->
    { ok, Port }   = application:get_env(?MODULE, port),
    { ok, Socket } = gen_udp:open(Port, [ ?PASSIVE_SOCKET ]),
    { ok, PID }    = poltergeist_supervisor:start_link(Socket),
    io:format("INFO: initialized poltergeist\n"),
    { ok, PID, #state{ socket = Socket } }.

%% @private
-spec stop(#state{}) -> ok.
stop(#state{ socket = Socket }) ->
    gen_udp:close(Socket),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
