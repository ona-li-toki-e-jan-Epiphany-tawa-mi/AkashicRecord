#!/usr/bin/env escript
%%! -sname escript

%% @copyright 2024 ona-li-toki-e-jan-Epiphany-tawa-mi | MIT License
%% @doc Super epic build script for poltergeist.

-feature(maybe_expr, enable).
-export([ main/1 ]).

-import(string, [ join/2 ]).
-import(io_lib, [ format/2 ]).



main([ "all" ])         -> all(),         log("done");
main([ "build" ])       -> build(),       log("done");
main([ "dialyzer" ])    -> dialyzer(),    log("done");
main([ "edoc" ])        -> edoc(),        log("done");
main([ "mostlyclean" ]) -> mostlyclean(), log("done");
main([ "clean" ])       -> clean(),       log("done");
main([])                -> all(),         log("done").



-define(SRC,  "src").
-define(EBIN, "ebin").
-define(DOC,  "doc").

-define(APP,       poltergeist).
-define(APP_FILE, "poltergeist.app").

-define(PLT_FILE, ".dialyzer_plt").
-define(PLT_APPS, [ "stdlib", "kernel" ]).

all() ->
    dialyzer(),
    build(),
    edoc().

build() ->
    mkdir(?EBIN),
    cp( join([ ?SRC,  ?APP_FILE ], "/")
      , join([ ?EBIN, ?APP_FILE ], "/")),
    run_emake().

dialyzer() ->
    build_plt(?PLT_FILE, ?PLT_APPS),
    run_dialyzer(?PLT_FILE, ?SRC).

edoc() ->
    mkdir(?DOC),
    cp( join([ ?SRC, "doc/overview.edoc" ], "/")
      , join([ ?DOC, "overview.edoc" ],     "/")),
    run_edoc().

mostlyclean() ->
    rm_r(?EBIN),
    rm_r(?DOC).

clean() ->
    mostlyclean(),
    rm_r(?PLT_FILE).



log(Message) ->
    io:format("~s: ~s\n", [ escript:script_name(), Message ]).

exists(Path) ->
    case file:read_file_info(Path) of
        { ok, _ } -> true;
        _         -> false
    end.

rm_r(Path) ->
    maybe
        true ?= exists(Path),
        log(format("removing file path '~s'...", [ Path ])),
        ok = file:del_dir_r(Path)
    end.

cp(Source, Destination) ->
    log(format("copying '~s' to '~s'...", [ Source, Destination ])),
    { ok, _ } = file:copy(Source, Destination).

mkdir(Path) ->
    maybe
        false ?= exists(Path),
        log(format("creating directory '~s'...", [ Path ])),
        ok = file:make_dir(Path)
    end.



run_emake() ->
    log("running emake..."),
    up_to_date = make:all().

build_plt(PltFile, Apps) ->
    maybe
        false ?= exists(PltFile),
        log(format("building PLT file '~s'...", [ PltFile ])),
        % I would have used dialyzer:run here but I couldn't get it to build the
        % PLT.
        io:format(os:cmd(format( "dialyzer --build_plt --apps ~s --output_plt ~s"
                               , [ join(Apps, " "), PltFile ]))),
        true = exists(PltFile)
    end.

run_dialyzer(PltFile, Directory) ->
    log(format("running Dialyzer on files in '~s'...", [ Directory ])),
    Warnings = dialyzer:run([ { files_rec, [ Directory ] }
                            , { from,      src_code }
                            , { init_plt,  PltFile }
                            ]),
    lists:map( fun(W) -> io:format("~s", [ dialyzer:format_warning(W) ]) end
             , Warnings),
    true = [] == Warnings.

run_edoc() ->
    log("running edoc..."),
    edoc:application(?APP, ".", [ { preprocess, true }
                                ]).
