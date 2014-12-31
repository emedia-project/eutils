-module(eapplication_tests).

-include_lib("eunit/include/eunit.hrl").

eapplication_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
      ?_test(t_set_env_from_file()),
      ?_test(t_set_env_from_file_before())
   ]}.

setup() ->
  code:add_patha("../test/apps/test/ebin").

teardown(_) ->
  ok.

t_set_env_from_file() ->
  clean_env(),
  application:start(test),
  ?assertMatch({ok, "todo"}, application:get_env(test, tutu)),
  ?assertMatch({ok, "fixed"}, application:get_env(test, other)),
  ?assertMatch(ok, eapplication:set_env_from_file("../test/apps/test/config/test.config")),
  ?assertMatch({ok, "tutu"}, application:get_env(test, tutu)),
  ?assertMatch({ok, "fixed"}, application:get_env(test, other)),
  ?assertMatch({ok, "toto"}, application:get_env(test, toto)),
  ?assertMatch({ok, "tata"}, application:get_env(test, tata)),
  application:stop(test).

t_set_env_from_file_before() ->
  clean_env(),
  ?assertMatch(ok, eapplication:set_env_from_file("../test/apps/test/config/test.config")),
  ?assertMatch({ok, "tutu"}, application:get_env(test, tutu)),
  ?assertMatch(undefined, application:get_env(test, other)),
  ?assertMatch({ok, "toto"}, application:get_env(test, toto)),
  ?assertMatch({ok, "tata"}, application:get_env(test, tata)),
  application:start(test),
  ?assertMatch({ok, "tutu"}, application:get_env(test, tutu)),
  ?assertMatch(undefined, application:get_env(test, other)),
  ?assertMatch({ok, "toto"}, application:get_env(test, toto)),
  ?assertMatch({ok, "tata"}, application:get_env(test, tata)),
  application:stop(test).

clean_env() ->
  lists:foreach(fun(Par) ->
                    application:unset_env(test, Par)
                end, [tutu, toto, tata, other]).
