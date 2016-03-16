%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Helper to run common test from the Erlang shell
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(kct).

%%%_* Exports ==========================================================
-export([run/1, run/2]).

%%%_* Types ============================================================
-type suite()     :: atom() | string().
-type suites()    :: [suite()] | suite().

-type testcase()  :: atom() | string().
-type testcases() :: [testcase()] | testcase().

%%%_* API ==============================================================
-spec run(suites()) -> ok.
run(S) ->
  Result = run_test([{suite, to_suite(S)}]),
  print_result(Result).

-spec run(suites(), testcases()) -> ok.
run(S, TC) ->
  Result = run_test([ {suite, to_suite(S)}
                    , {testcase, TC}
                    ]),
  print_result(Result).

%%%_* Internal =========================================================
to_suite(Suite) when is_atom(Suite) ->
  to_suite(atom_to_list(Suite));
to_suite([Suite|_]=Suites) when is_atom(Suite); is_list(Suite) ->
  lists:map(fun to_suite/1, Suites);
to_suite([C|_]=Suite) when is_integer(C) ->
  case lists:reverse(Suite) of
    "ETIUS_" ++ _ ->
      list_to_atom(Suite);
    _ ->
      list_to_atom(Suite ++ "_SUITE")
  end.

run_test(Opts) ->
  {ok, Dir} = file:get_cwd(),
  ok = file:set_cwd(Dir ++ "/test"),
  Result = ct:run_test(Opts ++ defaults()),
  ok = file:set_cwd(Dir),
  Result.

defaults() ->
  [ {include, "../include"}
  , {abort_if_missing_suites, true}
  ].

print_result({Ok, Failed, {UserSkipped, AutoSkipped}}) ->
  {ok, Dir} = file:get_cwd(),
  io:format("~n"
            "----------------------------------------------~n"
            "passed: ~p / failed: ~p / skipped: ~p~n"
            "report: file://~s/test/index.html~n"
            "----------------------------------------------~n",
            [Ok, Failed, UserSkipped + AutoSkipped, Dir]);
print_result({error, Reason}) ->
  io:format("Failed to run tests: ~p", [Reason]).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
