-module(eos).

-export([
         in/2
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
in(Path, Fun) when is_function(Fun) ->
  case file:get_cwd() of
    {ok, Dir} ->
      case file:set_cwd(Path) of
        ok -> 
          Result = Fun(),
          case file:set_cwd(Dir) of
            ok -> Result;
            E -> E
          end;
        E -> E
      end;
    E -> E
  end.
