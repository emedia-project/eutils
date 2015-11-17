-module(euri_tests).

-include_lib("eunit/include/eunit.hrl").

euri_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_join())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_join() ->
  ?assertEqual("/a/b/c", euri:join("/a/b", "c")),
  ?assertEqual("/a/b/c", euri:join("/a//b", "c")),
  ?assertEqual(<<"/a/b/c">>, euri:join("/a/b", <<"c">>)),
  ?assertEqual("/a/b/c", euri:join("/a/b", "/c")),
  ?assertEqual("/a/b/c", euri:join("/a/b/", "c")),
  ?assertEqual("/a/b/c", euri:join("a/b/", "/c/")),
  ?assertEqual("/a/b/c", euri:join("/a/b/", "/c")),
  ?assertEqual(<<"/a/b/c">>, euri:join(["a", "b", <<"c">>])),
  ?assertEqual("/a/b/c", euri:join(["/a/", "/b/", "/c/"])),
  ?assertEqual("/a/b/c", euri:join(["a", "b", "c"])).

