-module(elists).

-export([
  is_keylist/1,
  merge_keylists/3,
  identical/2,
  include/2
  ]).

%% @doc
%% Return true if the given list is a keylist
%% @end
is_keylist(L) when is_list(L) ->
  lists:all(fun(E) -> is_tuple(E) andalso tuple_size(E) =:= 2 end, L).

%% @doc
%% Merge the two keylists.
%%
%% Example:
%% <pre>
%% Args = [{a, 1}, {b, 2}],
%% Default = [{b, 3}, {c, 4}],
%% elists:merge_keylists(1, Args, Default),
%%   #=> [{c, 4}, {a, 1}, {b, 2}]
%% </pre>
%% @end
merge_keylists(_, [], TupleList2) ->
  TupleList2;
merge_keylists(N, [Tuple|Rest], TupleList2) when 
    is_integer(N), is_list(TupleList2), is_tuple(Tuple), is_list(Rest) ->
  Key = element(N, Tuple),
  TupleList3 = case lists:keysearch(Key, N, TupleList2) of
    {value, _} -> lists:keydelete(Key, N, TupleList2);
    false -> TupleList2
  end,
  merge_keylists(N, Rest, TupleList3 ++ [Tuple]).

%% @doc
%% Return true if the two given lists are identical
%%
%% Two lists are considered identical if they have exactly the same 
%% number of same elements.
%% @end
identical(List1, List2) when is_list(List1), is_list(List2) ->
  lists:sort(List1) =:= lists:sort(List2).

%% @doc
%% Return true if E is in List
%% @end
include(List, E) when is_list(List) ->
  lists:any(fun(Elem) -> Elem =:= E end, List).
