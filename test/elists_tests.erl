-module(elists_tests).

-include_lib("eunit/include/eunit.hrl").

elists_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_is_keylist()),
      ?_test(t_identical()),
      ?_test(t_merge_keylists())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_is_keylist() ->
  KL = [{a, 1}, {b, 2}],
  ?assert(true =:= elists:is_keylist(KL)),
  NKL = [{a, 1}, {b, 2, arg}],
  ?assert(false =:= elists:is_keylist(NKL)),
  NKLY = [1, {b, 2}, {c, 3}],
  ?assert(false =:= elists:is_keylist(NKLY)).

t_identical() ->
  L1a = [1, 2, 2, 3, 3, 3],
  L1b = [1, 2, 3, 2, 3, 3],
  L2 = [1, 2, 3],
  L3a = [[1,2,3], {a, 123}, "hello", <<"hello">>],
  L3b = [<<"hello">>, "hello", {a, 123}, [1,2,3]],
  ?assert(elists:identical(L1a, L1b)),
  ?assert(elists:identical(L3a, L3b)),
  ?assert(false =:= elists:identical(L1a, L2)).

t_merge_keylists() ->
  Args = [{a, 1}, {b, 2}],
  Default = [{b, 3}, {c, 4}],
  Result = elists:merge_keylists(1, Args, Default),
  io:format("~p ~n", [Result]),
  ?assert(elists:identical([{a, 1}, {b, 2}, {c, 4}], Result)).

