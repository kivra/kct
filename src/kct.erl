%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Helper to run common test from the Erlang shell
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(kct).

%%%_* Exports ==========================================================
-export([run/0, run/1, run/2]).

%%%_* Types ============================================================
-type suite()     :: atom() | string().
-type suites()    :: [suite()] | suite().

-type testcase()  :: atom() | string().
-type testcases() :: [testcase()] | testcase().

%%%_* API ==============================================================
-spec run() -> ok.
run() ->
  Files = filelib:wildcard("test/*_SUITE.erl"),
  case [filename:basename(File, ".erl") || File <- Files] of
    [] ->
      io:format("no suites found~n");
    Suites ->
      run(Suites)
  end.

-spec run(suites()) -> ok.
run(S) ->
  Result = run_test([{suite, to_suite(S)}]),
  handle_result(Result).

-spec run(suites(), testcases()) -> ok.
run(S, TC) ->
  Result = run_test([ {suite, to_suite(S)}
                    , {testcase, TC}
                    ]),
  handle_result(Result).

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
  Defaults = defaults(Dir),
  ok = file:set_cwd(Dir ++ "/test"),
  Result = ct:run_test(Opts ++ Defaults),
  ok = file:set_cwd(Dir),
  Result.

defaults(Dir) ->
  [ {include, Dir ++ "/include"}
  , {abort_if_missing_suites, true}
  ].

handle_result({Ok, Failed, {UserSkipped, AutoSkipped}}) ->
  {ok, Dir} = file:get_cwd(),
  Uri = lists:flatten(io_lib:format("file://~s/test/index.html", [Dir])),
  case Failed + UserSkipped + AutoSkipped > 0 of
    true ->
      open(Uri);
    false ->
      ok
  end,
  io:format("~n"
            "%%%~n"
            "%%% passed: ~p / failed: ~p / skipped: ~p~n"
            "%%% report: ~s~n"
            "%%%~n",
            [Ok, Failed, UserSkipped + AutoSkipped, Uri]);
handle_result({error, Reason}) ->
  io:format("Failed to run tests: ~p~n", [Reason]).

open(Uri) ->
  open(os:type(), Uri).

open({unix, darwin},  Uri) ->
  os:cmd("open " ++ Uri);
open({unix, linux}, Uri) ->
  os:cmd("xdg-open " ++ Uri);
open(_, _) ->
  not_supported.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
