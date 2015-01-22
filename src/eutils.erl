-module(eutils).

-export([
  to_atom/1,
  to_list/1,
  to_string/1,
  to_binary/1,
  to_integer/1,
  module_exist/1,
  is_string/1
  ]).

%% @doc
%% Convert the given term to atom
%%
%% Example:
%% <pre>
%% atom = eutils:to_atom(atom).
%% atom = eutils:to_atom(&lt;&lt;"atom"&gt;&gt;).
%% atom = eutils:to_atom("atom").
%% </pre>
%% @end
to_atom(X) when is_atom(X) ->
  X;
to_atom(X) when is_binary(X); is_bitstring(X) ->
  binary_to_atom(X, utf8);
to_atom(X) when is_list(X) ->
  list_to_atom(X).

%% @doc
%% Convert the given term to list
%%
%% Example:
%% <pre>
%% "list" = eutils:to_list(list).
%% "list" = eutils:to_list("list").
%% "list" = eutils:to_list(&lt;&lt;"list"&gt;&gt;).
%% "123" = eutils:to_list(123).
%% "1.20000000000000000000e+01" = eutils:to_list(12.0).
%% "true" = eutils:to_list(true).
%% "false" = eutils:to_list(false).
%% </pre>
%% @end
to_list(V) when is_atom(V) ->
  atom_to_list(V);
to_list(V) when is_list(V) ->
  V;
to_list(V) when is_integer(V) ->
  integer_to_list(V);
to_list(V) when is_float(V) ->
  float_to_list(V);
to_list(V) when is_binary(V); is_bitstring(V) ->
  binary_to_list(V);
to_list(V) when is_tuple(V) ->
  [element(I, V) || I <- lists:seq(1, tuple_size(V))];
to_list(true) ->
  "true";
to_list(false) ->
  "false".

%% @doc
%% Convert the given term to string
%% @end
to_string(V) ->
  lists:flatten(to_list(V)).

%% @doc
%% Convert the given term to binary
%%
%% Example:
%% <pre>
%% &lt;&lt;"list"&gt;&gt; = eutils:to_binary(list).
%% &lt;&lt;"list"&gt;&gt; = eutils:to_binary("list").
%% &lt;&lt;"list"&gt;&gt; = eutils:to_binary(&lt;&lt;"list"&gt;&gt;).
%% &lt;&lt;"123"&gt;&gt; = eutils:to_binary(123).
%% &lt;&lt;"1.20000000000000000000e+01"&gt;&gt; = eutils:to_binary(12.0).
%% &lt;&lt;"true"&gt;&gt; = eutils:to_binary(true).
%% &lt;&lt;"false"&gt;&gt; = eutils:to_binary(false).
%% </pre>
%% @end
to_binary(V) when is_binary(V); is_bitstring(V) ->
  V;
to_binary(V) ->
  iolist_to_binary(to_list(V)).

%% @doc
%% Convert the given term to integer
%%
%% Example
%%<pre>
%% 123 = eutils:to_integer(123).
%% 123 = eutils:to_integer("123").
%% 123 = eutils:to_integer(<<"123">>).
%% 123 = eutils:to_integer('123').
%% 123 = eutils:to_integer(123.456).
%% </pre<
%% @end
to_integer(I) when is_integer(I) ->
  I;
to_integer(I) when is_list(I) ->
  list_to_integer(I);
to_integer(I) when is_binary(I); is_bitstring(I) ->
  binary_to_integer(I);
to_integer(I) when is_atom(I) ->
  to_integer(atom_to_list(I));
to_integer(I) when is_float(I) ->
  to_integer(float_to_list(I, [{decimals, 0}])).

%% @doc
%% Check if the given module exist
%% @end
module_exist(Module) ->
  case is_atom(Module) of
    true ->
      try Module:module_info() of
        _InfoList ->
          true
      catch
        _:_ ->
          false
      end;
    false ->
      false
  end.

is_string(V) when is_list(V) ->
  lists:all(fun is_integer/1, V);
is_string(_) -> false.
