%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License
%% @doc Types and records for exported functions of poltergeist_controller.

-type move_mouse_error() :: string().
%% @doc <b>move_mouse</b> function signature.
-type move_mouse_function() :: fun((integer(), integer()) -> ok | { error, move_mouse_error() }).

-type click_mouse_error() :: string() | unknownbutton | unknownmodifier.
%% @doc <b>click_mouse</b> function signature.
-type click_mouse_function() :: fun((atom(), [ atom() ]) -> ok | { error, click_mouse_error() }).

-type press_key_error() :: string() | unknownkey | unknownmodifier.
%% @doc <b>press_key</b> function signature.
-type press_key_function() :: fun((atom(), [ atom() ]) -> ok | { error, press_key_error() }).

%% @doc Store for all available controller functions.
-record(controller, { move_mouse  :: move_mouse_function()
                    , click_mouse :: click_mouse_function()
                    , press_key   :: press_key_function()
                    }).
