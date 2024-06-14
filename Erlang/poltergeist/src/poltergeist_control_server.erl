%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License
%% @private Module for recieving keyboard and mouse commands from the network
%% and executing them.

-feature(maybe_expr, enable).

-module(poltergeist_control_server).
-behaviour(gen_server).

-include("poltergeist_controller.hrl").

-export([ start_link/0, init/1, handle_cast/2, handle_call/3 ]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

%% @private
-spec init(any()) -> { ok, #controller{} }.
init(_) ->
    { ok, Controller } = poltergeist_controller:create_controller(),
    { ok, Controller }.

%% @private runcommand - accept a command and pass it to <b>handle_command</b>.
%% @see handle_command/3.
-spec handle_cast(any() | { runcommand, string(), [ string() ] }, #controller{}) ->
          { noreply, #controller{} }.
handle_cast({ runcommand, Command, Arguments }, State) ->
    ok = handle_command(Command, Arguments, State),
    { noreply, State };
handle_cast(_, State) ->
    { noreply, State }.

%% @private
-spec handle_call(any(), any(), #controller{}) -> { noreply, #controller{} }.
handle_call(_, _, State) -> { noreply, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CALLBACKS.                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-type handle_command_error() :: move_mouse_error() | click_mouse_error() | press_key_error()
                              | badargs            | badcommand.

%% @private Parses and runs the given command.
-spec handle_command(string(), [string()], #controller{})
                    -> ok | { error,  handle_command_error()}.
handle_command("movemouse", Arguments, Controller) ->
    io:format("INFO: executing mousemove.\n"),
    handle_move_mouse(Arguments, Controller);
handle_command("click", Arguments, Controller) ->
    io:format("INFO: executing click.\n"),
    handle_mouse_click(Arguments, Controller);
handle_command("presskey", Arguments, Controller) ->
    io:format("INFO: executing presskey.\n"),
    handle_keypress(Arguments, Controller);
handle_command(_, _, _) ->
    io:format("INFO: invalid command.\n"),
    {error, badcommand}.

%% @private Gets whether the given string represents a valid modifier key.
-spec is_modifier(string()) -> boolean().
is_modifier(Argument)->
    lists:member(Argument, [ "alt", "control", "shift", "super", "meta" ]).



%% @private Parses and runs the <b>movemouse</b> command.
-spec handle_move_mouse([ string() ], #controller{})
                       -> ok | { move_mouse_error() | badargs }.
handle_move_mouse(Arguments, #controller{ move_mouse = MoveMouse }) ->
    maybe
        [ DeltaXString, DeltaYString ] ?= Arguments,

        { DeltaX, [] } ?= string:to_integer(DeltaXString),
        { DeltaY, [] } ?= string:to_integer(DeltaYString),

        MoveMouse(DeltaX, DeltaY)
    else
        _ -> { error, badargs }
    end.



%% @private Parses and runs the <b>click</b> command.
-spec handle_mouse_click([ string() ], #controller{})
                        -> ok | { click_mouse_error() | badargs }.
handle_mouse_click(Arguments, #controller{ click_mouse = ClickMouse }) ->
    maybe
        [ ButtonArgument | ModifierArguments ] ?= Arguments,

        true      ?= is_mouse_button(ButtonArgument),
        Button    =  list_to_atom(ButtonArgument),
        true      ?= lists:all(fun is_modifier/1,  ModifierArguments),
        Modifiers =  lists:map(fun list_to_atom/1, ModifierArguments),

        ClickMouse(Button, Modifiers)
    else
        _ -> { error, badargs }
    end.

%% @private Gets whether the given string represents a valid mouse button.
-spec is_mouse_button(string()) -> boolean().
is_mouse_button(Argument) ->
    lists:member(Argument, [ "left", "middle", "right" ]).



%% @private Parses and runs the <b>presskey</b> command.
-spec handle_keypress([ string() ], #controller{}) -> ok | { press_key_error() | badargs }.
handle_keypress(Arguments, #controller{ press_key = PressKey }) ->
    maybe
        [ KeyArgument | ModifierArguments ] ?= Arguments,

        true      ?= is_key(KeyArgument),
        Key       =  list_to_atom(KeyArgument),
        true      ?= lists:all(fun is_modifier/1,  ModifierArguments),
        Modifiers =  lists:map(fun list_to_atom/1, ModifierArguments),

        PressKey(Key, Modifiers)
    else
        _ -> { error, badargs }
    end.

%% @private Gets whether the given string represents a valid key.
%% TODO: Add '~' and ''' keys.
-spec is_key(string()) -> boolean().
is_key(Command) ->
    lists:member(Command, [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"
                          , "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v"
                          , "w", "x", "y", "z"
                          , "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"
                          , "space", "backspace", "tab"
                          , "escape", "super", "home", "end", "insert", "delete"
                          , "pageup", "pagedown"
                          , "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9"
                          , "f10", "f11", "f12"
                          , "up", "down", "left", "right"
                          , ",", ".", "/", ";", "[", "]", "\\", "-", "="
                          ]).
