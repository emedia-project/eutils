-module(euri).

-export([join/2, join/1]).

join(A, B) ->
  join([A, B]).

join(URIs) -> 
  join(URIs, [], true).

join([], Acc, String) ->
  URI = "/" ++ string:join(Acc, "/"),
  if
    String == true -> URI;
    true -> list_to_binary(URI)
  end;
join([C|Rest], Acc, _) when is_binary(C) ->
  join([binary_to_list(C)|Rest], Acc, false);
join([C|Rest], Acc, String) when is_list(C) ->
  join(Rest, Acc ++ string:tokens(C, "/"), String).

