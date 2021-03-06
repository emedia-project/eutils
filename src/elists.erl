-module(elists).

-export([
  is_keylist/1,
  merge_keylists/3,
  keyfind/3,
  keyfind/4,
  keyfindm/2,
  keyfindm/3,
  keysearch/2,
  keysearch/3,
  keymatch/3,
  keylistmap/2,
  keyfindlast/3,
  keyfindlast/4,
  keyfindfirst/3,
  keyfindfirst/4,
  keysdelete/3,
  identical/2,
  include/2,
  include_list/2,
  delete_if/2,
  fmax/2,
  index_of/2
  ]).

%% @doc
%% @end
fmax(_, []) -> error;
fmax(Fun, List) ->
  [First|Rest] = List,
  lists:foldl(fun(Element, Max) ->
        case Fun(Element) > Fun(Max) of
          true -> Element;
          false -> Max
        end
    end, First, Rest).

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
  merge_keylists(N, Rest, TupleList3 ++ [Tuple]);
merge_keylists(N, [Tuple|Rest], TupleList2) when 
    is_integer(N), is_list(TupleList2), is_list(Rest)->
  merge_keylists(N, Rest, TupleList2 ++ [Tuple]).

%% @hidden
keyfind(Key, N, List) ->
  keyfind(Key, N, List, false).

%% @doc
%% @end
keyfindm(Keys, List) ->
  keyfindm(Keys, List, false).

%% @doc
%% @end
keyfindm(_, [], Default) -> Default;
keyfindm(Keys, [Tuple|Rest], Default) ->
  case lists:all(fun({Key, N}) ->
                     if
                       size(Tuple) >= N, element(N, Tuple) =:= Key ->
                         true;
                       true -> 
                         false
                     end
                 end, Keys) of
    true -> Tuple;
    false -> keyfindm(Keys, Rest, Default)
  end.

%% @doc
%% Extended keylistmap for tuple list
%%
%% Example:
%% <pre>
%% Args = [{<<toto>>, world, 2}, {<<titi>>, hello}],
%% Funs = [
%%     {1, erlang:binary_to_list/1},
%%     {3, fun(X) -> X * 2 end}
%%   ],
%% elists:keylistmap(Funs, Args).
%%   # => [{"toto", world, 4}, {"titi", hello}]
%% </pre>
%% @end
keylistmap(Funs, TupleList) when is_list(Funs), is_list(TupleList) ->
  keylistmap(Funs, TupleList, []).

%% @hidden
keylistmap(_, [], Result) -> lists:reverse(Result);
keylistmap(Funs, [Tuple|Rest], Result) ->
  keylistmap(Funs, Rest, 
             [list_to_tuple(
                lists:map(fun({N, Data}) ->
                              case lists:keyfind(N, 1, Funs) of
                                {N, Fun} -> Fun(Data);
                                false -> Data
                              end
                          end, 
                          [{I, element(I, Tuple)} || 
                           I <- lists:seq(1, tuple_size(Tuple))]))
              | Result]).

%% @doc
%% Save as lists:keyfind/3 but with a default value
%%
%% Example:
%% <pre>
%% Result = elists:keyfind(a, 1, [{b, 2}, {c, 3}], 1).
%%   #=> 1
%% </pre>
%% @end
keyfind(Key, N, List, Default) ->
  case lists:keyfind(Key, N, List) of
    {Key, Result} -> Result;
    _ -> Default
  end.

%% @doc
%% Same as elists:keyfindfirst/4 where Default = undefined
%% @end
keyfindfirst(Keys, N, List) when is_list(Keys) ->
  keyfindfirst(Keys, N, List, undefined).

%% @doc
%% @end
keyfindfirst([], _, _, Default) ->
  Default;
keyfindfirst([Key|Keys], N, List, Default) ->
  case lists:keyfind(Key, N, List) of
    {Key, Value} -> {Key, Value};
    _ -> keyfindfirst(Keys, N, List, Default)
  end.

%% @doc
%% @end
keysdelete(Keys, N, List) ->
  lists:foldl(fun(Key, Acc) ->
                  lists:keydelete(Key, N, Acc)
              end, List, Keys).

%% @doc
%% @end
keysearch(Fun, List) -> 
  keysearch(Fun, List, undefined).

%% @doc
%% @end
keysearch(_, [], Default) -> Default;
keysearch(Fun, [E|List], Default) ->
  case Fun(E) of
    true -> E;
    false -> keysearch(Fun, List, Default)
  end.


%% @doc
%% Same as elists:keyfindlast/4 where Default = undefined
%% @end
keyfindlast(Keys, N, List) when is_list(Keys) ->
  keyfindlast(Keys, N, List, undefined).

%% @doc
%% @end
keyfindlast(Keys, N, List, Default) when is_list(Keys) ->
  keyfindfirst(lists:reverse(Keys), N, List, Default).

%% @doc
%% @end
keymatch(Tuple, N, List) when is_tuple(Tuple), is_tuple(N), is_list(List) ->
  keymatch(
    fun(E, T) ->
        {_, Result} = lists:foldl(
                        fun(I, {I1, Res}) ->
                            Res1 = case tuple_size(E) >= tuple_size(T) of
                                     true ->
                                       Res andalso element(I1, T) =:= element(I, E);
                                     false -> Res and false
                                   end,
                            {I1 + 1, Res1} 
                        end, {1, true}, eutils:to_list(N)),
        Result
    end, Tuple, List);
keymatch(Fun, Tuple, List) when is_tuple(Tuple), is_function(Fun), is_list(List) ->
  lists:foldl(fun(Element, Acc) ->
                  case Fun(Element, Tuple) of
                    true -> [Element|Acc];
                    false -> Acc
                  end
              end, [], List).


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
  lists:member(E, List).

%% @doc
%% Return true if all element in IncList are in List
%% @end
include_list(List, IncList) when is_list(List), is_list(IncList) ->
  lists:all(fun(Inc) ->
                include(List, Inc)
            end, IncList).


%% @doc
%% @end
delete_if(Fun, List) ->
  lists:reverse(lists:foldl(fun(E, Acc) ->
                                case Fun(E) of
                                  true -> Acc;
                                  false -> [E|Acc]
                                end
                            end, [], List)).

%% @doc
%% Return the index of the item in list, or not_found
%% @end
index_of(List, Item) when is_list(List) -> 
  index_of(Item, List, 1).

%% @nodoc
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
