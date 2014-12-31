-module(eos).

-export([
         in/2,
         in/3
        ]).

%% @doc
%% Execute the given function function in the given path.
%%
%% Example :
%% 
%% <pre lang="erlang">
%% eos:in("/tmp", fun() ->
%%   ?assertMatch({ok, "/tmp"}, file:get_cwd())
%%   end).
%% </pre>
%% @end
in(Path, Fun, Args) when is_function(Fun) ->
  case file:get_cwd() of
    {ok, Dir} ->
      case file:set_cwd(Path) of
        ok -> 
          Result = apply(Fun, Args),
          case file:set_cwd(Dir) of
            ok -> Result;
            E -> E
          end;
        E -> E
      end;
    E -> E
  end.

%% @equiv in(Path, Fun, [])
in(Path, Fun) when is_function(Fun) ->
  in(Path, Fun, []).
