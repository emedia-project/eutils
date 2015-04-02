-module(ecsv).

-export([read/1, read/2]).

-define(ITEM_SEPARATOR, $,).
-define(STRING_DELIMITER, $").

read(String) ->
  read(String, []).
read(String, Options) -> 
  read(
    to_map(
      options(
        options(Options, 
                separator, 
                ?ITEM_SEPARATOR), 
        string_delimiter, 
        ?STRING_DELIMITER)), 
    data(String), 
    []).

%% Private

read(_, [], Acc) ->
  lists:reverse(Acc);
read(Options, String, []) ->
  {Line, Rest} = read_line(Options, String),
  read(Options, Rest, [Line]);
read(Options, [10|String], Acc) ->
  {Line, Rest} = read_line(Options, String),
  read(Options, Rest, [Line|Acc]);
read(Options, [13,10|String], Acc) ->
  {Line, Rest} = read_line(Options, String),
  read(Options, Rest, [Line|Acc]).

add_spaces(0, String) -> 
  String;
add_spaces(Count, String) -> 
  add_spaces(Count-1, [$ |String]).

read_item(#{string_delimiter := StringDelimiter} = Options, [StringDelimiter|T]) -> 
  read_item_quoted(Options, T, []);
read_item(Options, Other) -> 
  read_item(Options, Other, 0, []).

read_item(Options, [32|T], 0, []) -> 
  read_item(Options, T, 0, []);
read_item(Options, [9|T], 0, []) -> 
  read_item(Options, T, 0, []);
read_item(_, [10|T], _SpaceCount, Acc) -> 
  {lists:reverse(Acc), [10|T]};
read_item(_, [13,10|T], _SpaceCount, Acc) -> 
  {lists:reverse(Acc), [13,10|T]};
read_item(#{separator := ItemSeparator}, [ItemSeparator|T], _SpaceCount, Acc) -> 
  {lists:reverse(Acc), [ItemSeparator|T]};
read_item(_, [], _SpaceCount, Acc) -> 
  {lists:reverse(Acc), []};
read_item(Options, [9|T], SpaceCount, Acc) -> 
  read_item(Options, T, SpaceCount+1, Acc);
read_item(Options, [32|T], SpaceCount, Acc) -> 
  read_item(Options, T, SpaceCount+1, Acc);
read_item(Options, [C|T], SpaceCount, Acc) -> 
  read_item(Options, T, 0, [C|add_spaces(SpaceCount, Acc)]).

read_item_quoted(#{string_delimiter := StringDelimiter} = Options, [StringDelimiter,StringDelimiter|T], Acc) -> 
  read_item_quoted(Options, T, [?STRING_DELIMITER|Acc]);
read_item_quoted(#{string_delimiter := StringDelimiter}, [StringDelimiter|T], Acc) -> 
  {lists:reverse(Acc), T};
read_item_quoted(Options, [C|T], Acc) -> 
  read_item_quoted(Options, T, [C|Acc]).

read_line(Options, String) -> 
  read_line(Options, String,[]).

read_line(_, [10|T], Acc) -> 
  {lists:reverse(Acc), [10|T]};
read_line(_, [13,10|T], Acc) -> 
  {lists:reverse(Acc), [13|T]};
read_line(_, [], Acc) -> 
  {lists:reverse(Acc), []};
read_line(Options, String, []) -> 
  {Item, Rest} = read_item(Options, String), 
  read_line(Options, Rest, [Item]);
read_line(#{separator := ItemSeparator} = Options, [ItemSeparator|String], Acc) -> 
  {Item, Rest} = read_item(Options, String), 
  read_line(Options, Rest, [Item|Acc]).

data(String) ->
  case filelib:is_regular(String) of
    true -> 
      {ok, Data} = file:read_file(String),
      binary_to_list(Data);
    _ -> String
  end.

options(Options, Name, Default) ->
  case lists:keyfind(Name, 1, Options) of
    false ->
      [{Name, Default}|Options];
    _ ->
      Options
  end.

to_map(Options) when is_list(Options) ->
  lists:foldl(fun({Key, Value}, Acc) ->
                  maps:put(Key, Value, Acc)
              end, #{}, Options).

