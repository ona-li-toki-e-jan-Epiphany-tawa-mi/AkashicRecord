%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License
%% @private Supervisor for poltergeist.

-module(poltergeist_supervisor).
-behaviour(supervisor).

-export([ start_link/1, init/1, start_udp_listener/1 ]).



-define(UDP_LISTENER_CHILD_SPEC(Socket),
        #{ id       => poltergeist_udp_listener
         , start    => { poltergeist_udp_listener, start_link, [ Socket ] }
         , restart  => permanent
         , shutdown => brutal_kill
         }).

-define(CONTROL_SERVER_CHILD_SPEC,
        #{ id       => poltergeist_control_server
         , start    => { poltergeist_control_server, start_link, [] }
         , restart  => permanent
         , shutdown => brutal_kill
         }).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private @param Socket the socket for the UDP server.
-spec start_link(gen_udp:socket()) -> supervisor:startlink_ret().
start_link(Socket) ->
    supervisor:start_link({ local, ?MODULE }, ?MODULE, Socket).

%% @private @param Socket the socket for the UDP server.
-spec init(gen_udp:socket()) -> { ok, { supervisor:sup_flags()
                                      , [ supervisor:child_spec() ]
                                      } }.
init(Socket) ->
    SupervisorFlags     = #{},
    ChildSpecifications = [ ?UDP_LISTENER_CHILD_SPEC(Socket)
                          , ?CONTROL_SERVER_CHILD_SPEC
                          ],

    spawn_link(fun start_control_server/0),
    spawn_link(?MODULE, start_udp_listener, [ Socket ]),
    { ok, { SupervisorFlags, ChildSpecifications } }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% @private Starts a UDP listener child process to listen on the given socket.
-spec start_udp_listener(gen_udp:socket()) -> supervisor:startchild_ret().
start_udp_listener(Socket) ->
    supervisor:start_child(?MODULE, ?UDP_LISTENER_CHILD_SPEC(Socket)).

%% @private Starts a control server process to execute the commands from the UDP server.
-spec start_control_server() -> supervisor:startchild_ret().
start_control_server() ->
    supervisor:start_child(?MODULE, ?CONTROL_SERVER_CHILD_SPEC).
