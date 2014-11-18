-module(eapplication).

-export([get_env/3]).

get_env(App, Key, Default) ->
  case application:get_env(App, Key) of
    {ok, Value} -> Value;
    _ -> Default
  end.
