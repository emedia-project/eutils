-module(eapplication).

-export([get_env/3]).

% @deprecated Use application:get_env/3
get_env(App, Key, Default) ->
  application:get_env(App, Key, Default).
