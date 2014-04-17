-module(eio_tests).

-include_lib("eunit/include/eunit.hrl").

eio_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_readlines())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_readlines() ->
  ?assert(true).

