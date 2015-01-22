-module(ebinary).

-export([
  to_upper/1,
  to_num/1,
  sub/3,
  gsub/3,
  concat/1,
  join/2,
  do_as_list/2,
  do_as_list/3,
  repeat/2,
  remove_accents/1
  ]).

to_upper(B) ->
  eutils:to_binary(string:to_upper(eutils:to_string(B))).

remove_accents(B) ->
  eutils:to_binary(estring:remove_accents(eutils:to_string(B))).

%% @doc
%% Return the number corresponding to the given binary
%%
%% Example:
%% <pre lang="erlang">
%% {ok, 123} = estring:to_num(&lt;&lt;"123"&gt;&gt;).
%% {ok, 12.3} = estring:to_num(&lt;&lt;"12.3"&gt;&gt;).
%% {error, not_a_number} = estring:to_num(&lt;&lt;"abc"&gt;&gt;).
%% </pre>
%% @end
-spec to_num(binary()) -> {ok, number()} | {error, not_a_number}.
to_num(Binary) when is_binary(Binary) ->
  estring:to_num(binary_to_list(Binary)).

%% @doc
%% Return an new string with the first occurance of Old substitued by New
%%
%% Example:
%%
%% <pre lang="erlang">
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
%%
%% <pre lang="erlang">
%% &lt;&lt;"HeLLo worLd"&gt;&gt; = ebinary:gsub(&lt;&lt;"Hello World"&gt;&gt;, &lt;&lt;"l"&gt;&gt;, &lt;&lt;"L"&gt;&gt;).
%% </pre>
%% @end
-spec gsub(binary(), binary(), binary()) -> binary().
gsub(Str, Old, New) ->
  do_as_list(estring, gsub, [Str, Old, New]).

%% @doc
%% Concatenate a list of binaries
%%
%% Example:
%%
%% <pre lang="erlang">
%% &lt;&lt;"tototatatiti"&gt:&gt; = ebinary:concat([&lt;&lt;"toto"&gt;&gt;, &lt;&lt;"tata"&gt;&gt;, &lt;&lt;"titi"&gt;&gt;]).
%% </pre>
%% @end
concat(List) when is_list(List) ->
  join(List, <<>>).

%% @doc
%% join a list of binaries with the given separator
%%
%% Example:
%%
%% <pre lang="erlang">
%% &lt;&lt;"toto-tata-titi"&gt:&gt; = ebinary:join([&lt;&lt;"toto"&gt;&gt;, &lt;&lt;"tata"&gt;&gt;, &lt;&lt;"titi"&gt;&gt;], &lt;&lt;"-"&gt;&gt;).
%% </pre>
%% @end
join([First|Rest], Sep) ->
  F = fun(A, B) ->
          Ab = eutils:to_binary(A),
          Bb = eutils:to_binary(B),
          S = eutils:to_binary(Sep),
          <<Ab/binary, S/binary, Bb/binary>>
      end,
  lists:foldr(F, First, Rest).

%% @hidden
do_as_list(Fun, Binary) when is_function(Fun), is_binary(Binary) ->
  list_to_binary(Fun(binary_to_list(Binary))).
%% @hidden
do_as_list(Module, Function, Binary) when is_atom(Module), is_atom(Function), is_binary(Binary) ->
  list_to_binary(erlang:apply(Module, Function, [binary_to_list(Binary)]));
do_as_list(Module, Function, Binaries) when is_atom(Module), is_atom(Function), is_list(Binaries) ->
  Lists = lists:map(fun binary_to_list/1, Binaries),
  list_to_binary(erlang:apply(Module, Function, Lists)).

%% @doc
%% Create a binary where X is repeated N times
%%
%% Example:
%%
%% <pre lang="erlang">
%% <<"hellohellohello">> = ebinary:repeat(<<"hello">>, 3).
%% </pre>
%% @end
repeat(X, N) ->
  list_to_binary(estring:repeat(binary_to_list(X), N)).
