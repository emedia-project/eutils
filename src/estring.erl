-module(estring).

-export([
  join/2,
  start_with/2,
  start_with/3,
  to_num/1,
  sub/3,
  gsub/3,
  split_first/2,
  repeat/2
  ]).

%% @doc
%% @end
join([First|Rest], JoinWith) ->
  lists:flatten([First] ++ [JoinWith ++ eutils:to_list(X) || X <- Rest, eutils:to_list(X) =/= ""]).

%% @doc
%% Split sthe given string at the first Token found
%%
%% Example:
%% <pre>
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
%% <pre>
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
%% <pre>
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
%% <pre>
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
%% <pre>
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
%% <pre>
%% "HeLLo WorLd" = estring:gsub("Hello World", "l", "L").
%% </pre>
%% @end
-spec gsub(string(), string(), string()) -> string().
gsub(Str,Old,New) ->
  Acc = sub(Str,Old,New),
  subst(Acc,Old,New,Str).

%% @doc
%% Create a string where X is repeated N times
%%
%% Example:
%% <pre>
%% "hellohellohello" = estring:repeat("hello", 3).
%% </pre>
%% @end
repeat(X, N) ->
  lists:flatten(lists:duplicate(N, X)).

%% Private

subst(Str,_Old,_New, Str) -> Str;
subst(Acc, Old, New,_Str) ->
  Acc1 = sub(Acc,Old,New),
  subst(Acc1,Old,New,Acc).
