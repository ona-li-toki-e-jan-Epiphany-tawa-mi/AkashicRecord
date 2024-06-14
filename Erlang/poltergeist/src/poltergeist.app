%% -*- erlang -*-
%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License

{ application, poltergeist,
 [ { description,  "bare-bones remote keyboard and mouse server" }
 , { vsn,          "0.1.0" }
 , { modules,      [ poltergeist, poltergeist_supervisor, poltergeist_udp_listener
                   , poltergeist_control_server, poltergeist_controller ] }
 , { registered,   [ poltergeist_supervisor, poltergeist_control_server ] }
 , { applications, [ stdlib, kernel ] }
 , { mod,          { poltergeist, [] } }
   % sum([bytes(i, 'utf-8')[0]*10 for i in "poltergeist"])
 , { env,          [ { port, 12020 } ] }
 ] }.
