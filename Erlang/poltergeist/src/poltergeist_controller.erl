%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License
%% @doc Module for moving the mouse and executing virtual keypresses and mouse
%% clicks.

-feature(maybe_expr, enable).

-module(poltergeist_controller).
-export([ create_controller/0
        , move_mouse_function/0
        , click_mouse_function/0
        , press_key_function/0
        ]).

-include("poltergeist_controller.hrl").

-import(io_lib, [ format/2 ]).
-import(string, [ join/2 ]).



%% @doc Fetches all controller functions and loads them into a record.
-spec create_controller() -> { ok, #controller{} } | { error, notsupported }.
create_controller() ->
    maybe
        { ok, MoveMouse }  ?= move_mouse_function(),
        { ok, ClickMouse } ?= click_mouse_function(),
        { ok, PressKey }   ?= press_key_function(),
        { ok, #controller{ move_mouse  = MoveMouse
                         , click_mouse = ClickMouse
                         , press_key   = PressKey
                         } }
    else
        Error -> Error
    end.

%% @doc Fetches the underlying <b>move_mouse</b> function for the graphical
%% server of the current system.
%%
%% <b>move_mouse</b> accepts a x and y value (integers) representing the number
%% of pixels to move the mouse on those axes. Positive values mean up and to the
%% right, negative means down and to the left.
-spec move_mouse_function() -> { ok, move_mouse_function() } | { error, notsupported }.
move_mouse_function() ->
    X11_Supported = is_x11_supported(),
    if X11_Supported -> { ok,    fun x11_move_mouse/2 }
    ;  true          -> { error, notsupported }
    end.

%% @doc Fetches the underlying <b>click_mouse</b> function for the graphical
%% server of the current system.
%%
%% <b>click_mouse</b> accepts a mouse button (atom) and a list of modifier keys
%% (atoms) to press in combination. See the underlying functions' definitions
%% for available buttons and modifer keys.
-spec click_mouse_function() -> { ok, click_mouse_function() } | { error, notsupported }.
click_mouse_function() ->
    X11_Supported = is_x11_supported(),
    if X11_Supported -> { ok,    fun x11_click_mouse/2 }
    ;  true          -> { error, notsupported }
    end.

%% @doc Fetches the underlying <b>press_key</b> function for the graphical
%% server of the current system.
%%
%% <b>press_key</b> accepts a key (atom) and a list of modifier keys (atoms)
%% to press in combination. See the underlying functions' definitions for
%% available keys.
-spec press_key_function() -> { ok, press_key_function() } | { error, notsupported }.
press_key_function() ->
    X11_Supported = is_x11_supported(),
    if X11_Supported -> { ok,    fun x11_press_key/2 }
    ;  true          -> { error, notsupported }
    end.



%% @private Executes a shell command.
-spec cmd(string()) -> ok | { error, string() }.
cmd(Command) ->
    Result = os:cmd(Command),
    case Result of
        [] -> ok;
        _  -> { error, Result }
    end.



%% @private Checks whether the X11 functions are supported by the current
%% system.
-spec is_x11_supported() -> boolean().
is_x11_supported() -> false =/= os:find_executable("xdotool").

%% @private X11 implementation of move_mouse.
-spec x11_move_mouse(integer(), integer()) -> ok | { error, move_mouse_error() }.
x11_move_mouse(DeltaX, DeltaY) ->
    cmd(format("xdotool mousemove_relative ~w ~w", [ DeltaX, -DeltaY ])).

%% @private X11 implementation of click_mouse.
-spec x11_click_mouse(atom(), [ atom() ]) -> ok | { error, click_mouse_error() }.
x11_click_mouse(Button, Modifiers) ->
    maybe
        { ok, ButtonString } ?= x11_button_string(Button),

        MStringsWrapped =  lists:map(fun x11_modifier_string/1, Modifiers),
        true            ?= lists:all( fun(R) -> ok == R end
                                    , [ Result || { Result, _ } <- MStringsWrapped ]),
        ModifierStrings =  [ String || { _, String } <- MStringsWrapped ],

        [ cmd(format("xdotool keydown ~s",      [ MS ])) || MS <- ModifierStrings ],
        Result = cmd(format("xdotool click ~s", [ ButtonString ])),
        [ cmd(format("xdotool keyup ~s",        [ MS ])) || MS <- ModifierStrings ],
        Result
    else
        { error, Error } -> { error, Error };
        false            -> { error, unknownmodifier }
    end.

%% @private X11 implementation of press_key.
-spec x11_press_key(atom(), [ atom() ]) -> ok | { error, press_key_error() }.
x11_press_key(Key, Modifiers) ->
    maybe
        { ok, KeyString } ?= x11_key_string(Key),

        MStringsWrapped =  lists:map(fun x11_modifier_string/1, Modifiers),
        true            ?= lists:all( fun(R) -> ok == R end
                                    , [ Result || { Result, _ } <- MStringsWrapped ]),
        ModifierStrings =  [ String || { _, String } <- MStringsWrapped ],

        cmd(format("xdotool key ~s", [ join( ModifierStrings ++ [ KeyString ]
                                           , "+" )]))
    else
        { error, Error } -> { error, Error };
        false            -> { error, unknownmodifier }
    end.

%% @private Gets the text expected by xdotool for a given mouse button.
-spec x11_button_string(atom()) -> { ok, string() } | { error, unknownbutton }.
x11_button_string(left)   -> { ok,    "1" };
x11_button_string(middle) -> { ok,    "2" };
x11_button_string(right)  -> { ok,    "3" };
x11_button_string(_)      -> { error, unknownbutton }.

%% @private Gets the text expected by xdotool for a given modifier key.
-spec x11_modifier_string(atom()) -> { ok, string() } | { error, unknownmodifier }.
x11_modifier_string(alt)     -> { ok,    "alt" };
x11_modifier_string(control) -> { ok,    "ctrl" };
x11_modifier_string(shift)   -> { ok,    "shift" };
x11_modifier_string(super)   -> { ok,    "super" };
x11_modifier_string(meta)    -> { ok,    "meta" };
x11_modifier_string(_)       -> { error, unknownmodifier }.

%% @private Gets the text expected by xdotool for a given key.
-spec x11_key_string(atom()) -> { ok, string() } | { error, unknownkey }.
x11_key_string(a)         -> { ok,    "a" };
x11_key_string(b)         -> { ok,    "b" };
x11_key_string(c)         -> { ok,    "c" };
x11_key_string(d)         -> { ok,    "d" };
x11_key_string(e)         -> { ok,    "e" };
x11_key_string(f)         -> { ok,    "f" };
x11_key_string(g)         -> { ok,    "g" };
x11_key_string(h)         -> { ok,    "h" };
x11_key_string(i)         -> { ok,    "i" };
x11_key_string(j)         -> { ok,    "j" };
x11_key_string(k)         -> { ok,    "k" };
x11_key_string(l)         -> { ok,    "l" };
x11_key_string(m)         -> { ok,    "m" };
x11_key_string(n)         -> { ok,    "n" };
x11_key_string(o)         -> { ok,    "o" };
x11_key_string(p)         -> { ok,    "p" };
x11_key_string(q)         -> { ok,    "q" };
x11_key_string(r)         -> { ok,    "r" };
x11_key_string(s)         -> { ok,    "s" };
x11_key_string(t)         -> { ok,    "t" };
x11_key_string(u)         -> { ok,    "u" };
x11_key_string(v)         -> { ok,    "v" };
x11_key_string(w)         -> { ok,    "w" };
x11_key_string(x)         -> { ok,    "x" };
x11_key_string(y)         -> { ok,    "y" };
x11_key_string(z)         -> { ok,    "z" };
x11_key_string('1')       -> { ok,    "1" };
x11_key_string('2')       -> { ok,    "2" };
x11_key_string('3')       -> { ok,    "3" };
x11_key_string('4')       -> { ok,    "4" };
x11_key_string('5')       -> { ok,    "5" };
x11_key_string('6')       -> { ok,    "6" };
x11_key_string('7')       -> { ok,    "7" };
x11_key_string('8')       -> { ok,    "8" };
x11_key_string('9')       -> { ok,    "9" };
x11_key_string('0')       -> { ok,    "0" };
x11_key_string(space)     -> { ok,    "space" };
x11_key_string(backspace) -> { ok,    "BackSpace" };
x11_key_string(tab)       -> { ok,    "Tab" };
x11_key_string(escape)    -> { ok,    "Escape" };
x11_key_string(super)     -> { ok,    "super" };
x11_key_string(home)      -> { ok,    "Home" };
x11_key_string('end')     -> { ok,    "End" };
x11_key_string(insert)    -> { ok,    "Insert" };
x11_key_string(delete)    -> { ok,    "Delete" };
x11_key_string(pageup)    -> { ok,    "Page_Up" };
x11_key_string(pagedown)  -> { ok,    "Page_Down" };
x11_key_string(f1)        -> { ok,    "F1" };
x11_key_string(f2)        -> { ok,    "F2" };
x11_key_string(f3)        -> { ok,    "F3" };
x11_key_string(f4)        -> { ok,    "F4" };
x11_key_string(f5)        -> { ok,    "F5" };
x11_key_string(f6)        -> { ok,    "F6" };
x11_key_string(f7)        -> { ok,    "F7" };
x11_key_string(f8)        -> { ok,    "F8" };
x11_key_string(f9)        -> { ok,    "F9" };
x11_key_string(f10)       -> { ok,    "F10" };
x11_key_string(f11)       -> { ok,    "F11" };
x11_key_string(f12)       -> { ok,    "F12" };
x11_key_string(up)        -> { ok,    "Up" };
x11_key_string(down)      -> { ok,    "Down" };
x11_key_string(left)      -> { ok,    "Left" };
x11_key_string(right)     -> { ok,    "Right" };
x11_key_string(',')       -> { ok,    "comma" };
x11_key_string('.')       -> { ok,    "period" };
x11_key_string('/')       -> { ok,    "slash" };
x11_key_string(';')       -> { ok,    "semicolon" };
x11_key_string('[')       -> { ok,    "bracketleft" };
x11_key_string(']')       -> { ok,    "bracketright" };
x11_key_string('\\')      -> { ok,    "backslash" };
x11_key_string('-')       -> { ok,    "minus" };
x11_key_string('=')       -> { ok,    "equal" };
x11_key_string(_)         -> { error, unknownkey }.
