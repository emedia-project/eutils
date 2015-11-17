-module(uuid).

-export([generate/0]).

generate() ->
  error_logger:error_msg("uuid:generate/0 is deprecated, please use euuid:generate/0~n"),
  euuid:generate().

