-module(enet).

-export([
         str_to_ip/1,
         ip_to_str/1,
         get_active_ip/0,
         get_loopback/0
        ]).

str_to_ip(IP) when is_binary(IP) ->
  str_to_ip(binary_to_list(IP));
str_to_ip(IP) when is_list(IP) ->
  [A, B, C, D] = [list_to_integer(X) || X <- string:tokens(IP, ".")],
  {A, B, C, D}.

ip_to_str({A, B, C, D}) ->
  lists:flatten(io_lib:format("~B.~B.~B.~B", [A, B, C, D])).

get_active_ip() ->
  get_active_ip(get_iflist()).

get_loopback() ->
  get_loopback(get_iflist()).

% privates

get_active_ip(If_list) ->
  get_ip([A || A <- If_list, inet:ifget(A,[addr]) /= {ok,[{addr,{127,0,0,1}}]}, filter_networkcard(list_to_binary(A))]).

get_iflist() ->
  {ok, IfList} = inet:getiflist(),
  IfList.

filter_networkcard(<<"vnic", _R/binary>>) ->
  false;
filter_networkcard(<<"vmnet", _R/binary>>) ->
  false;
filter_networkcard(_) ->
  true.

get_ip([]) ->
  get_loopback();
get_ip([If]) ->
  case inet:ifget(If, [addr]) of
    {ok, []} -> get_loopback();
    {_, [{_, Ip}]} -> Ip
  end.

get_loopback(If_list) ->
  get_ip([A || A <- If_list, inet:ifget(A,[addr]) == {ok,[{addr,{127,0,0,1}}]}]).
