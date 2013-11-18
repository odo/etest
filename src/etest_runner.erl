-module (etest_runner).
-compile (export_all).


% Macro printing the given message to stderr.
-define (stderr (Msg, Args),
    io:put_chars(standard_error, io_lib:format(Msg, Args))).

-define (stderr (Msg), ?stderr(Msg, [])).

auto_run(Modules) ->
    io:format("starting etest-auto-runner.\nModules: ~p\n", [Modules]),
    [load(M) || M <- Modules],
    sync:go(),
    sync:log([errors, warnings]),
    Callback = fun(Mods) ->
            TestMods = test_mods(Mods),
            io:format("running ~p ...\n", [TestMods]),
            case TestMods of
                [] -> noop;
                _ ->
                    run_all(TestMods, false)
            end
    end,
    sync:onsync(Callback).

load(Mod) ->
    try
        Mod:module_info()
    catch
        _:_ -> noop
    end.

test_mods(Mods) ->
    lists:usort([TestMod || TestMod <- [ mod_to_test_mod(M) || M <- Mods ], TestMod =/= undefined]).

mod_to_test_mod(Mod) ->
    ModString = atom_to_list(Mod),
    ModPostfix = lists:last(string:tokens(ModString, "_")),
    TestModName = case ModPostfix of
        "test" -> ModString;
        _      -> ModString ++ "_test"
    end,
    ModCandidate = list_to_atom(TestModName),
    case module_exists(ModCandidate) of
        true  -> ModCandidate;
        false -> undefined
    end.

module_exists(Module) ->
    try Module:module_info() of
        _InfoList ->
            true
    catch
        _:_ ->
            false
    end.

% The runner will be called without arguments in case no tests were found .
% Print a descriptive error message, then exit.
run_all() ->
    ?stderr("etest: No tests found~n"),
    erlang:halt().


run_all(Modules) ->
    run_all(Modules, true).

run_all(Modules, Halt) ->
    % Init statistics.
    [put(K, 0) || K <- [errors, success, tests]],

    ModulesAndOutcomes = lists:map(fun run/1, Modules),

    log_by_outcome(ModulesAndOutcomes),

    io:format("=========================================~n"
              "  Failed: ~p.  Success: ~p.  Total: ~p.~n~n", [
                get(errors),
                get(success),
                get(tests) ]),
    case Halt of
        true  -> erlang:halt(get(errors));
        false -> noop
    end.

log_by_outcome(ModulesAndOutcomes) ->
    io:format("\nFailures per Module:\n", []),
    Failures = fun(E) -> not E end,
    ModulesAndFailures = [{M, length(lists:filter(Failures, OC))} || {M, OC} <- ModulesAndOutcomes],
    OrderByFailures = fun({_, FA}, {_, FB}) ->
            FA >= FB
    end,
    [log_outcome(E) || E <- lists:sort(OrderByFailures, ModulesAndFailures)],
    io:format("\n", []).

log_outcome({_, 0}) ->
    noop;
log_outcome({Module, Failures}) ->
    io:format("~p\t~p\n", [Module, Failures]).


run(Module) ->
    Funs = testfuns(Module),
    FunsWithCallbacks = apply_callbacks(Module, Funs),

    BeforeSuite = maybe_fun(Module, before_suite),
    AfterSuite = maybe_fun(Module, after_suite),

    ToRun = lists:flatten([BeforeSuite, FunsWithCallbacks, AfterSuite]),
    TryTest = fun (Test) ->
        try
            Test(),
            io:format("Etest passed.\n"),
            true
        catch
            _:Error ->
                io:format("Etest failed.\n"),
                inc(errors),
                io:format("ETEST_FAILED::~p~n", [Error]),
                CleanTrace = clean_trace(erlang:get_stacktrace()),
                io:format("Stacktrace:~n~p~n~n", [CleanTrace]),
                false
        end
    end,
    {Module, lists:map(TryTest, ToRun)}.



testfuns(Module) ->
    Exports = try
        Module:module_info(exports)
    catch
        _:_ ->
            ?stderr("etest: ~p: No such module~n", [Module]),
            erlang:halt(1)
    end,

    IsFocus = fun({FunName, _}) ->
        nomatch =/= re:run(atom_to_list(FunName), "^focus_test_")
    end,

    TestFuns = case lists:filter(IsFocus, Exports) of
        [] ->
            IsTest = fun({FunName, _}) ->
                nomatch =/= re:run(atom_to_list(FunName), "^test_")
            end,
            lists:filter(IsTest, Exports);
        FocusTests -> FocusTests
    end,

    MakeApplicative = fun({FunName, _}) ->
        fun() ->
            inc(tests),
            Msg = lists:flatten(io_lib:format("~p:~p ", [Module, FunName])),
            io:format(
                string:left(Msg, 80, $.) ++ "\n" ++
                string:left("",  80, $=) ++ "\n"
            ),
            Module:FunName(),
            inc(success)
        end
    end,
    lists:map(MakeApplicative, TestFuns).


apply_callbacks(Module, Funs) ->
    Before = maybe_fun(Module, before_test),
    After = maybe_fun(Module, after_test),
    [
        fun() ->
            Before(),
            try Fun() after After() end
        end
        || Fun <- Funs
    ].


maybe_fun(Module, FunName) ->
    case has_fun(Module, FunName) of
        true  -> fun() -> Module:FunName() end;
        false -> fun() -> ok end
    end.


has_fun(Module, FunName) ->
    Exports = Module:module_info(exports),
    proplists:is_defined(FunName, Exports).


clean_trace(Trace0) ->
    % Remove the lower etest stack.
    {_ETestTrace, TraceR} = lists:split(5, lists:reverse(Trace0)),
    lists:reverse(TraceR).


inc(Name) ->
    put(Name, get(Name) + 1).
