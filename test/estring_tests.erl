-module(estring_tests).

-include_lib("eunit/include/eunit.hrl").

estring_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_start_with()),
      ?_test(t_to_num()),
      ?_test(t_sub()),
      ?_test(t_gsub()),
      ?_test(t_quote())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_start_with() ->
  ?assertEqual(true, estring:start_with("Hello world", "Hello")),
  ?assertEqual(false, estring:start_with("Goodbye world", "Hello")),
  ?assertEqual(true, estring:start_with("Hello world", "HELLO", true)),
  ?assertEqual(false, estring:start_with("Hello world", "HELLO", false)),
  ?assertEqual(true, estring:start_with("Hello world", "Hello", true)),
  ?assertEqual(false, estring:start_with("Goodbye world", "Hello", true)).

t_to_num() ->
  ?assertEqual({ok, 123}, estring:to_num("123")),
  ?assertEqual({ok, 12.3}, estring:to_num("12.3")),
  ?assertEqual({error, not_a_number}, estring:to_num("abc")).

t_sub() ->
  ?assertEqual("HeLlo World", estring:sub("Hello World", "l", "L")).

t_gsub() ->
  ?assertEqual("HeLLo WorLd", estring:gsub("Hello World", "l", "L")),
  ?assertEqual("HeLLo World", estring:gsub("Hello World", "ll", "LL")),
  ?assertEqual("Goodbye World", estring:gsub("Hello World", "Hello", "Goodbye")),
  ?assertEqual("Hello Monde", estring:gsub("Hello World", "World", "Monde")).

t_quote() ->
  ?assertEqual("\"toto\\\"titi\"", estring:quote("toto\"titi")).
