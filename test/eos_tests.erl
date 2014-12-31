-module(eos_tests).

-include_lib("eunit/include/eunit.hrl").

eos_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_in())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_in() ->
  {ok, Dir} = file:get_cwd(),
  eos:in("/tmp", fun() ->
                     ?assertMatch({ok, "/tmp"}, file:get_cwd()),
                     ?assertNotMatch({ok, Dir}, file:get_cwd())
                 end),
  {ok, Dir1} = file:get_cwd(),
  ?assertEqual(Dir, Dir1),
  ?assertNotMatch("/tmp", Dir1).


