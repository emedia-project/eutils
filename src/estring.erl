-module(estring).

-export([
  start_with/2,
  to_num/1,
  sub/3,
  gsub/3
  ]).

%% @doc
%% Return true if the first string start with the second one
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
%% Return the number corresponding to the given string
%%
%% Example:
%% <pre>
%% {ok, 123} = estring:to_num("123").
%% {ok, 12.3} = estring:to_num("12.3").
%% {error, not_a_number} = string:to_num("abc").
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
%% "HeLlo world" = estring:sub("Hello World", "l", "L").
%% </pre>
%% @end
-spec sub(string(), string(), string()) -> string().
sub(Str,Old,New) ->
    Lstr = length(Str),
    Lold = length(Old),
    Pos  = string:str(Str,Old),
    if
        Pos =:= 0 ->
            Str;
        true      ->
            LeftPart = string:left(Str,Pos-1),
            RitePart = string:right(Str,Lstr-Lold-Pos+1),
            string:concat(string:concat(LeftPart,New),RitePart)
        end.

%% @doc
%% Return an new string with the all occurances of Old substitued by New
%%
%% Example:
%% <pre>
%% "HeLLo worLd" = estring:gsub("Hello World", "l", "L").
%% </pre>
%% @end
-spec gsub(string(), string(), string()) -> string().
gsub(Str,Old,New) ->
    Acc = sub(Str,Old,New),
    subst(Acc,Old,New,Str).

%% Private

subst(Str,_Old,_New, Str) -> Str;
subst(Acc, Old, New,_Str) ->
    Acc1 = sub(Acc,Old,New),
    subst(Acc1,Old,New,Acc).
