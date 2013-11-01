-module (etest_runner).
-compile (export_all).


% Macro printing the given message to stderr.
-define (stderr (Msg, Args),
    io:put_chars(standard_error, io_lib:format(Msg, Args))).

-define (stderr (Msg), ?stderr(Msg, [])).


% The runner will be called without arguments in case no tests were found .
% Print a descriptive error message, then exit.
run_all() ->
    ?stderr("etest: No tests found~n"),
    erlang:halt().


run_all(Modules) ->
    % Init statistics.
    [put(K, 0) || K <- [errors, success, tests]],

    ModulesAndOutcomes = lists:map(fun run/1, Modules),

    log_by_outcome(ModulesAndOutcomes),

    io:format("=========================================~n"
              "  Failed: ~p.  Success: ~p.  Total: ~p.~n~n", [
                get(errors),
                get(success),
                get(tests) ]),

    erlang:halt(get(errors)).

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
