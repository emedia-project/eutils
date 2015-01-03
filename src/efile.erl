-module(efile).

-export([
  expand_path/1,
  normalize_path/1,
  user_home/0,
  make_dir/1,
  remove_recursive/1,
  copy_recursive/2,
  relative_from/2,
  realpath/1
  ]).

%% @doc
%% Expand the given path
%%
%% Example:
%%
%% <pre lang="erlang">
%% "/home/user" = efile:expand_path("~").
%% &lt;&lt;"/home/user"&gt;&gt; = efile:expand_path(&lt;&lt;"~"&gt;&gt;).
%% </pre>
%% @end
-spec expand_path(string() | binary()) -> binary() | list().
expand_path(Path) when is_binary(Path) ->
  ebinary:do_as_list(?MODULE, expand_path, Path);
expand_path(Path) when is_list(Path) ->
  normalize_path(filename:absname(expand_home(Path))).

%% @doc
%% Normalize the given path
%%
%% Example:
%%
%% <pre lang="erlang">
%% "/" = efile:normalize_path("/toto/tutu/../../../../../..").
%% &lt;&lt;"/"&gt;&gt; = efile:normalize_path(&lt;&lt;"/toto/tutu/../../../../../.."&gt;&gt;).
%% "/toto/titi" = efile:normalize_path("/toto/tata/../titi").
%% </pre>
%% @end
-spec normalize_path(string() | binary()) -> string() | binary().
normalize_path(Path) when is_binary(Path) ->
  ebinary:do_as_list(?MODULE, normalize_path, Path);
normalize_path(Path) when is_list(Path) ->
  normalize_path(filename:split(Path), []).
normalize_path([".."|T], []) ->
  normalize_path(T, []);
normalize_path([".."|T], [_|Acc]) ->
  normalize_path(T, Acc);
normalize_path(["."|T], Acc) ->
  normalize_path(T, Acc);
normalize_path([H|T], Acc) ->
  normalize_path(T, [H|Acc]);
normalize_path([], Acc) ->
  case length(Acc) of
    0 -> "/";
    _ -> filename:join(lists:reverse(Acc))
  end.

%% @doc
%% Return the HOME directory
%%
%% Example:
%%
%% <pre lang="erlang">
%% "/home/user" = efile:user_home().
%% </pre>
%% @end
-spec user_home() -> string().
user_home() ->
  case os:type() of
    {win32, _} -> get_windows_home();
    _ -> get_unix_home()
  end.

%% @doc
%% Create the given directory if it not exist
%% @end
make_dir(Path) ->
  filelib:ensure_dir(filename:join([Path, "."])).

%% @doc
%% Remove, recursively the given path
%% @end
remove_recursive(Path) ->
  case filelib:is_dir(Path) of
    false ->
      file:delete(Path);
    true ->
      lists:foreach(fun remove_recursive/1, sub_files(Path)),
      file:del_dir(Path)
  end.

%% @doc
%% @end
copy_recursive(Source, Destination) ->
  Base = filename:basename(Source),
  Dest = filename:join(Destination, Base),
  case filelib:is_dir(Source) of
    false -> 
      case file:copy(Source, Dest) of
        {error, Reason} ->
          error(Reason);
        _ -> ok
      end;
    true ->
      case make_dir(Dest) of
        ok ->
          lists:foreach(fun(File) ->
                            copy_recursive(File, Dest)
                        end, sub_files(Source));
        {error, Reason} ->
          error(Reason)
      end
  end.

%% @doc
%% @end
relative_from(FilePath, FromPath) ->
  case get_real_path(FilePath) of
    {ok, FilePath1} ->
      case get_real_path(FromPath) of
        {ok, FromPath1} ->
          realpath(
            filename:join(
              relative_from1(
                filename:split(FilePath1), 
                filename:split(FromPath1))));
        E -> E
      end;
    E -> E
  end.

%% @doc
%% Return the realpath of the given path
%% @end
realpath(Path) ->
  filename:join(
    realpath(
      filename:split(Path), 
      []
    )
  ).

% private

get_real_path(Path) ->
  case filename:split(Path) of
    ["/"|_] -> {ok, Path};
    FilePath3 -> 
      case file:get_cwd() of
        {ok, Dir} -> {ok, realpath(filename:join(filename:split(Dir) ++ FilePath3))};
        E -> E
      end
  end.

relative_from1([C|File], [C|From]) ->
  relative_from1(File, From);
relative_from1(File, From) ->
  [".." || X <- From, X =/= "/"] ++ File.

realpath([], Result) ->
  Result;
realpath([Current|List], Result) when Current =:= "..", length(Result) > 0 ->
  case lists:reverse(Result) of
    [".."|_] -> 
      realpath(List, Result ++ [Current]);
    _ ->
      case re:run(Result, "^.*/$") of
        {match, _} -> 
          realpath(List, Result);
        nomatch ->
          realpath(List, lists:reverse(tl(lists:reverse(Result))))
      end
  end;
realpath([Current|List], Result) when Current =:= "." ->
  realpath(List, Result);
realpath([Current|List], Result) ->
  realpath(List, Result ++ [Current]).

sub_files(From) ->
  {ok, SubFiles} = file:list_dir(From),
  [filename:join(From, SubFile) || SubFile <- SubFiles].

expand_home([$~|Rest]) ->
  user_home() ++ Rest;
expand_home(Path) -> Path.

get_unix_home() ->
  os:getenv("HOME").

get_windows_home() ->
  filename:absname(
    case os:getenv("USERPROFILE") of
      false ->
        get_windows_home(os:getenv("HOMEDRIVE"));
      Path -> Path
    end
    ).
get_windows_home(false) -> false;
get_windows_home(HomeDrive) -> get_windows_home(HomeDrive, os:getenv("HOMEPATH")).
get_windows_home(_, false) -> false;
get_windows_home(HomeDrive, HomePath) -> HomeDrive ++ HomePath.
