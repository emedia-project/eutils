-module(ebinary).

-export([
  to_num/1,
  sub/3,
  gsub/3,
  concat/1,
  do_as_list/2,
  do_as_list/3
  ]).

%% @doc
%% @end
-spec to_num(binary()) -> {ok, number()} | {error, not_a_number}.
to_num(Binary) when is_binary(Binary) ->
  estring:to_num(binary_to_list(Binary)).

%% @doc
%% Return an new string with the first occurance of Old substitued by New
%%
%% Example:
%% <pre>
%% &lt;&lt;"HeLlo world"&gt;&gt; = ebinary:sub(&lt;&lt;"Hello World"&gt;&gt;, &lt;&lt;"l"&gt;&gt;, &lt;&lt;"L"&gt;&gt;).
%% </pre>
%% @end
-spec sub(binary(), binary(), binary()) -> binary().
sub(Str, Old, New) ->
  do_as_list(estring, sub, [Str, Old, New]).

%% @doc
%% Return an new string with the all occurances of Old substitued by New
%%
%% Example:
%% <pre>
%% &lt;&lt;"HeLLo worLd"&gt;&gt; = ebinary:gsub(&lt;&lt;"Hello World"&gt;&gt;, &lt;&lt;"l"&gt;&gt;, &lt;&lt;"L"&gt;&gt;).
%% </pre>
%% @end
-spec gsub(binary(), binary(), binary()) -> binary().
gsub(Str, Old, New) ->
  do_as_list(estring, gsub, [Str, Old, New]).

%% @doc
%% @end
concat(List) when is_list(List) ->
  F = fun(A, B) -> <<A/binary, B/binary>> end,
  lists:foldr(F, <<>>, List).

%% @hidden
do_as_list(Fun, Binary) when is_function(Fun), is_binary(Binary) ->
  list_to_binary(Fun(binary_to_list(Binary))).
%% @hidden
do_as_list(Module, Function, Binary) when is_atom(Module), is_atom(Function), is_binary(Binary) ->
  list_to_binary(erlang:apply(Module, Function, [binary_to_list(Binary)]));
do_as_list(Module, Function, Binaries) when is_atom(Module), is_atom(Function), is_list(Binaries) ->
  Lists = lists:map(fun binary_to_list/1, Binaries),
  list_to_binary(erlang:apply(Module, Function, Lists)).
