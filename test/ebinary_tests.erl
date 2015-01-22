-module(ebinary_tests).

-include_lib("eunit/include/eunit.hrl").

ebinary_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_common())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_common() ->
  ?assertMatch(<<"Gregoire">>, ebinary:remove_accents(<<"Grégoire">>)),
  ?assertMatch(<<"GRÉGOIRE">>, ebinary:to_upper(<<"Grégoire">>)),
  ?assertEqual(
     ebinary:to_upper(ebinary:remove_accents(<<"Grégoire">>)),
     ebinary:remove_accents(ebinary:to_upper(<<"Grégoire">>))).

