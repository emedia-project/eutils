-module(exml).
-export([
         build/1,
         build/2
        ]).

-define(XML_PROLOG, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>).
-define(DEFAULT_OPTIONS, 
        [
         {space, 2},
         {format, true},
         {prolog, ?XML_PROLOG},
         {level, 0}
        ]).

build(X) ->
  build(X, ?DEFAULT_OPTIONS).
build(X, Options) when is_map(Options) ->
  build(X, maps:to_list(Options));
build(X, Options) when is_tuple(X), is_list(Options) ->
  FinalOptions = maps:from_list(
    elists:merge_keylists(1, Options, ?DEFAULT_OPTIONS)
   ),
  XML = xml_node(X, FinalOptions),
  case FinalOptions of
    #{format := true, prolog := Prolog} -> <<Prolog/binary, "\n", XML/binary>>;
    #{prolog := Prolog} -> <<Prolog/binary, XML/binary>>
  end.

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


% build({}) -> "";
% 
% build({Tag,Value}) ->
%   build({Tag,[],Value});
% build({Tag,AttrList,ContentList}) when is_list(ContentList) ->
%   Content = list_to_atom(lists:flatmap(fun build/1, ContentList)),
%   build({Tag,AttrList,Content});
% build({Tag,AttrList,Content}) when is_tuple(Content) ->
%   build({Tag,AttrList,list_to_atom(build(Content))});
% build({TAG,AttrList,Content}) ->
%   Tag = atom_to_list(TAG),
%   "<"++Tag++attrs(AttrList,"")++">"++atom_to_list(Content)++"</"++Tag++">".
% 
