-module(estring).

-export([
  join/2,
  start_with/2,
  start_with/3,
  to_num/1,
  sub/3,
  gsub/3,
  split_first/2,
  repeat/2,
  quote/1,
  remove_accents/1,
  random/1
  ]).

remove_accents(S0) ->
  S1 = re:replace(S0, "[áàâãä]","a", [global, {return, list}]),
  S2 = re:replace(S1, "[éèêë]","e", [global, {return, list}]),
  S3 = re:replace(S2, "[íìîï]","i", [global, {return, list}]),
  S4 = re:replace(S3, "[óòôõö]","o", [global, {return, list}]),
  S5 = re:replace(S4, "[úùûü]","u", [global, {return, list}]),
  S6 = re:replace(S5, "ç","c", [global, {return, list}]),
  S7 = re:replace(S6, "[ÁÀÂÃÄ]","A", [global, {return, list}]),
  S8 = re:replace(S7, "[ÉÈÊË]","E", [global, {return, list}]),
  S9 = re:replace(S8, "[ÍÌÎÏ]","I", [global, {return, list}]),
  SA = re:replace(S9, "[ÓÒÔÕÖ]","O", [global, {return, list}]),
  SB = re:replace(SA, "[ÚÙÛÜ]","U", [global, {return, list}]),
  re:replace(SB, "Ç","C", [global, {return, list}]).


%% @doc
%% @end
join([First|Rest], JoinWith) ->
  lists:flatten([First] ++ [JoinWith ++ eutils:to_list(X) || X <- Rest, eutils:to_list(X) =/= ""]).

%% @doc
%% Split sthe given string at the first Token found
%%
%% Example:
%% <pre lang="erlang">
%% {"hello" " world, I love you"} = estring:split_first("hello, world, I love you", ",")
%% </pre>
%% @doc
split_first(String, Token) ->
  split_first(String, Token, {[], []}).
split_first([], _, R) -> R;
split_first([C|Rest], Token, {A, B}) ->
  case elists:include(Token, C) of
    true -> {A, Rest};
    false -> split_first(Rest, Token, {A ++ [C], B})
  end.

%% @doc
%% Return true if the first string start with the second one
%%
%% Same as estring:start(String, Start, false)
%%
%% Example :
%% <pre lang="erlang">
%% true = estring:start_with("Hello world", "Hello").
%% false = estring:start_with("Goodbye world", "Hello").
%% </pre>
%% @end
-spec start_with(string(), string()) -> boolean().
start_with(String, Substr) when is_list(String), is_list(Substr) ->
  Len = string:len(Substr),
  StartStr = string:substr(String, 1, Len),
  string:equal(StartStr, Substr).

%% @doc
%% Return true if the first string start with the second one
%%
%% Example :
%% <pre lang="erlang">
%% true = estring:start_with("Hello world", "HELLO", true).
%% false = estring:start_with("Hello world", "HELLO", false).
%% true = estring:start_with("Hello world", "Hello", true).
%% false = estring:start_with("Goodbye world", "Hello", true).
%% </pre>
%% @end
start_with(String, Substr, IgnoreCase) when is_list(String), is_list(Substr), is_boolean(IgnoreCase) ->
  Len = string:len(Substr),
  StartStr = string:substr(String, 1, Len),
  if 
    IgnoreCase -> string:equal(string:to_lower(StartStr), string:to_lower(Substr));
    true -> string:equal(StartStr, Substr)
  end.

%% @doc
%% Return the number corresponding to the given string
%%
%% Example:
%% <pre lang="erlang">
%% {ok, 123} = estring:to_num("123").
%% {ok, 12.3} = estring:to_num("12.3").
%% {error, not_a_number} = estring:to_num("abc").
%% </pre>
%% @end
-spec to_num(string()) -> {ok, number()} | {error, not_a_number}.
to_num(String) when is_list(String) ->
  case string:to_float(String) of
    {error,no_float} -> 
      try
        {ok, list_to_integer(String)}
      catch
        error:badarg -> {error, not_a_number}
      end;
    {F,_Rest} -> {ok, F}
  end.

%% @doc
%% Return an new string with the first occurance of Old substitued by New
%%
%% Example:
%% <pre lang="erlang">
%% "HeLlo World" = estring:sub("Hello World", "l", "L").
%% </pre>
%% @end
-spec sub(string(), string(), string()) -> string().
sub(Str, Old, New) ->
  FStr = lists:flatten(Str),
  FOld = lists:flatten(Old),
  FNew = lists:flatten(New),
  Lstr = length(FStr),
  Lold = length(FOld),
  Pos  = string:str(FStr, FOld),
  if
    Pos =:= 0 ->
      FStr;
    true      ->
      LeftPart = string:left(FStr, Pos-1),
      RitePart = string:right(FStr, Lstr-Lold-Pos+1),
      string:concat(string:concat(LeftPart, FNew), RitePart)
  end.

%% @doc
%% Return an new string with the all occurances of Old substitued by New
%%
%% Example:
%% <pre lang="erlang">
%% "HeLLo WorLd" = estring:gsub("Hello World", "l", "L").
%% </pre>
%% @end
-spec gsub(string(), string(), string()) -> string().
gsub(Str, Old, New) ->
  %Acc = sub(Str,Old,New),
  %subst(Acc,Old,New,Str).
  gsub(Str, Old, New, "").

gsub("", _Old, _New, Acc) -> lists:flatten(Acc);
gsub(Str, Old, New, Acc) ->
  case string:str(Str, Old) of
    0 ->
      gsub("", Old, New, Acc ++ Str);
    Pos ->
      Pre = string:left(Str, Pos - 1),
      Rest = string:right(Str, length(Str) - Pos + 1 - length(Old)),
      gsub(Rest, Old, New, Acc ++ Pre ++ New)
  end.


%% @doc
%% Create a string where X is repeated N times
%%
%% Example:
%% <pre lang="erlang">
%% "hellohellohello" = estring:repeat("hello", 3).
%% </pre>
%% @end
repeat(X, N) ->
  lists:flatten(lists:duplicate(N, X)).

%% @doc
%% @end
quote(Str) ->
  "\"" ++ gsub(Str, "\"", "\\\"") ++ "\"".

%% @doc
%% @end
random(Size) ->
  random(
    lists:flatten([X || X <- eutils:to_string(base64:encode(crypto:strong_rand_bytes(Size))), is_alphanum(X)]),
    Size).
random(Str, Size) when length(Str) >= Size -> 
  string:left(Str, Size);
random(Str, Size) ->
  random(
    lists:flatten(Str ++ [X || X <- eutils:to_string(base64:encode(crypto:strong_rand_bytes(Size - length(Str)))), is_alphanum(X)]),
    Size).

is_alphanum(C) when C >= 16#30 andalso C =< 16#39 -> true;
is_alphanum(C) when C >= 16#41 andalso C =< 16#5A -> true;
is_alphanum(C) when C >= 16#61 andalso C =< 16#7A -> true;
is_alphanum(_) -> false.
