-module(eapplication_tests).

-include_lib("eunit/include/eunit.hrl").

eapplication_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
      ?_test(t_set_env_from_file())
   ]}.

setup() ->
  code:add_patha("../test/apps/test/ebin"),
  application:start(test).

teardown(_) ->
  application:stop(test).

t_set_env_from_file() ->
  ?assertMatch(ok, eapplication:set_env_from_file("../test/apps/test/config/test.config")),
  ?assertMatch({ok, "toto"}, application:get_env(test, toto)),
  ?assertMatch({ok, "tata"}, application:get_env(test, tata)),
  ?assertMatch({ok, "tutu"}, application:get_env(test, tutu)).
