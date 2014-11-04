-module(emaps).

-export([
         merge/2
        ]).

merge(Map, Default) ->
  maps:fold(fun(K, V, MapAcc) ->
               case maps:get(K, MapAcc, false) of
                 false -> maps:put(K, V, MapAcc);
                 _ -> MapAcc
               end
           end, Map, Default).
