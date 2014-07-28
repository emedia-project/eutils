-module(efile).

-export([
  expand_path/1,
  normalize_path/1,
  user_home/0,
  make_dir/1,
  remove_recursive/1
  ]).

%% @doc
%% Expand the given path
%%
%% Example:
%% <pre>
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
%% <pre>
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
%% <pre>
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

%% Private

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
