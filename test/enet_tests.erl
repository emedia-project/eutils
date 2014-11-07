-module(enet_tests).

-include_lib("eunit/include/eunit.hrl").

eio_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_ip()),
      ?_test(t_network())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_ip() ->
  ?assertMatch({127, 0, 0, 1}, enet:str_to_ip("127.0.0.1")),
  ?assertMatch({127, 0, 0, 1}, enet:str_to_ip(<<"127.0.0.1">>)),
  ?assertMatch("127.0.0.1", enet:ip_to_str({127, 0, 0, 1})).

t_network() ->
  ?assertMatch({127, 0, 0, 1}, enet:get_loopback()).
