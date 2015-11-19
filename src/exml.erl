-module(exml).
-export([
         export/1,
         export/2,
         import/1
        ]).

-define(XML_PROLOG, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>).
-define(DEFAULT_OPTIONS, 
        [
         {space, 2},
         {format, true},
         {prolog, ?XML_PROLOG},
         {level, 0}
        ]).

export(X) ->
  export(X, ?DEFAULT_OPTIONS).
export(X, Options) when is_map(Options) ->
  export(X, maps:to_list(Options));
export(X, Options) when is_tuple(X), is_list(Options) ->
  FinalOptions = maps:from_list(
    elists:merge_keylists(1, Options, ?DEFAULT_OPTIONS)
   ),
  XML = xml_node(X, FinalOptions),
  case FinalOptions of
    #{format := true, prolog := Prolog} -> <<Prolog/binary, "\n", XML/binary>>;
    #{prolog := Prolog} -> <<Prolog/binary, XML/binary>>
  end.

import(X) when is_list(X) ->
  import(eutils:to_binary(X));
import(X) when is_binary(X) ->
  tokenize(X).

%% Private (for import)

-define(m(X,B),<<X,B/binary>>). % match
-define(d(X,B),<<X:8/integer,B/binary>>). % decompose
-define(M(X,B),<<X,_/binary>> = B). % match
-define(D(X,B),<<X:8/integer,_/binary>> = B). % decompose

-define(ok(X),$a=<X,X=<$z;$A=<X,X=<$Z;$0=<X,X=<$9;X==$_;X==$-;X==$:).
-define(ws(X),X==$\s;X==$\r;X==$\n;X==$\t).
-define(dq(X),X==$").
-define(sq(X),X==$').

-define(ev(X), ?ws(X); X==$>; X==$=).

tokenize(X) -> tokenize(X, []).
tokenize(<<>>, Acc) -> Acc;
%tokenize({tag, Rest}, Acc) ->

tokenize(X, Acc) ->
  tokenize(token(X), Acc).

token(?m("<", Str)) -> {tag, word(Str)};
token(?d(_,Str)) -> Str.

word({Type, ?d(X, Str)}) when ?ws(X) -> word({Type, Str});
word(X) -> X.


%% Private (for export)

xml_node({}, _) -> <<"">>;
xml_node({Tag}, Options) ->
  xml_node({Tag, [], <<"">>}, Options);
xml_node({Tag, Content}, Options) -> 
  xml_node({Tag, [], Content}, Options);
xml_node({Tag, Attrs, Content}, Options) when is_tuple(Content) -> 
  xml_node({Tag, Attrs, [Content]}, Options);
xml_node({Tag, Attrs, [Content]}, Options) when not is_tuple(Content) -> 
  xml_node({Tag, Attrs, eutils:to_binary(Content)}, Options);
xml_node({Tag, Attrs, Content}, 
         #{space := Space, format := Format, level := Level} = Options
        ) when is_list(Content) -> 
  TagBin = eutils:to_binary(Tag),
  AttrsBin = attrs(Attrs),
  SpaceBin = ebinary:repeat(<<" ">>, Level * Space),
  case lists:foldl(fun(Node, Acc) ->
                       NodeBin = xml_node(Node, Options#{level := Level + 1}),
                       <<Acc/binary, NodeBin/binary>>
                   end, <<"">>, Content) of
    <<"">> -> 
      case Format of
        true -> <<SpaceBin/binary, "<", TagBin/binary, AttrsBin/binary, "/>\n">>;
        false -> <<"<", TagBin/binary, AttrsBin/binary, "/>">>
      end;
    ContentBin -> 
      case Format of
        true ->
          <<SpaceBin/binary, "<", TagBin/binary, AttrsBin/binary, ">\n", 
            ContentBin/binary, 
            SpaceBin/binary, "</", TagBin/binary, ">\n">>;
        false ->
          <<"<", TagBin/binary, AttrsBin/binary, ">", 
            ContentBin/binary, 
            "</", TagBin/binary, ">">>
      end
  end;
xml_node({Tag, Attrs, Content}, 
         #{space := Space, format := Format, level := Level}) -> 
  TagBin = eutils:to_binary(Tag),
  AttrsBin = attrs(Attrs),
  Node = case eutils:to_binary(Content) of
           <<"">> -> <<"<", TagBin/binary, AttrsBin/binary, "/>">>;
           ContentBin -> <<"<", TagBin/binary, AttrsBin/binary, ">", 
                           ContentBin/binary, 
                           "</", TagBin/binary, ">">>
         end,
  case Format of
    true -> 
      SpaceBin = ebinary:repeat(<<" ">>, Level * Space),
      <<SpaceBin/binary, Node/binary, "\n">>;
    false -> Node
  end;
xml_node(Content, #{space := Space, format := Format, level := Level}) -> 
  ContentBin = eutils:to_binary(Content),
  case Format of
    true -> 
      SpaceBin = ebinary:repeat(<<" ">>, Level * Space),
      <<SpaceBin/binary, ContentBin/binary, "\n">>;
    false -> ContentBin
  end.

attrs(Attrs) when is_list(Attrs) ->
  attrs(Attrs, <<"">>).
attrs([], Acc) ->
  Acc;
attrs([{Attr, Value}|Rest], Acc) ->
  AttrBin = eutils:to_binary(Attr),
  ValueBin = eutils:to_binary(Value),
  attrs(Rest, <<Acc/binary, " ", AttrBin/binary, "=\"", ValueBin/binary, "\"">>).

