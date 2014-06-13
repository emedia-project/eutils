-module(estring_tests).

-include_lib("eunit/include/eunit.hrl").

eio_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_start_with()),
      ?_test(t_to_num()),
      ?_test(t_sub()),
      ?_test(t_gsub())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_start_with() ->
  ?assert(true =:= estring:start_with("Hello world", "Hello")),
  ?assert(false =:= estring:start_with("Goodbye world", "Hello")),
  ?assert(true =:= estring:start_with("Hello world", "HELLO", true)),
  ?assert(false =:= estring:start_with("Hello world", "HELLO", false)),
  ?assert(true =:= estring:start_with("Hello world", "Hello", true)),
  ?assert(false =:= estring:start_with("Goodbye world", "Hello", true)).

t_to_num() ->
  ?assert({ok, 123} =:= estring:to_num("123")),
  ?assert({ok, 12.3} =:= estring:to_num("12.3")),
  ?assert({error, not_a_number} =:= estring:to_num("abc")).

t_sub() ->
  ?assert("HeLlo World" =:= estring:sub("Hello World", "l", "L")).

t_gsub() ->
  ?assert("HeLLo WorLd" =:= estring:gsub("Hello World", "l", "L")).
