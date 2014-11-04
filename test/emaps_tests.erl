-module(emaps_tests).

-include_lib("eunit/include/eunit.hrl").

eio_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_merge())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_merge() ->
  ?assertEqual(
     #{one => 1, two => 2, three => 3},
     emaps:merge(#{one => 1, two => 2}, #{two => 4, three => 3})).

