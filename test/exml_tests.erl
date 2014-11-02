-module(exml_tests).

-include_lib("eunit/include/eunit.hrl").

eio_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_valid())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_valid() ->
  ?assertEqual(
     exml:build({<<"document">>, [{<<"hello">>, <<"world">>}], <<"Say hello">>}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\">Say hello</document>">>),
  ?assertEqual(
     exml:build({"document", [{"hello", "world"}], <<"Say hello">>}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\">Say hello</document>">>),
  ?assertEqual(
     exml:build({document, [{hello, <<"world">>}], <<"Say hello">>}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\">Say hello</document>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], <<"Say hello">>}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\">Say hello</document>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], {say}}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\"><say/></document>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], {}}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\"/>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], []}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\"/>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], [{}]}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\"/>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], {say, <<"hello">>}}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\"><say>hello</say></document>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], {say, [{lang, en}], [<<"hello">>]}}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\"><say lang=\"en\">hello</say></document>">>),
  ?assertEqual(
     exml:build({document, [{hello, "world"}], {say, [{lang, en}], [<<"hello">>, {hola}, bonjour]}}, 
                [{prolog, <<"">>}, {format, false}]), 
     <<"<document hello=\"world\"><say lang=\"en\">hello<hola/>bonjour</say></document>">>).

