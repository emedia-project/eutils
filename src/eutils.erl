-module(eutils).

-export([
  to_atom/1,
  to_list/1
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
to_list(true) ->
  "true";
to_list(false) ->
  "false".
