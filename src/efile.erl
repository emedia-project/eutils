-module(efile).

-export([
  expand_path/1,
  normalize_path/1,
  user_home/0,
  make_dir/1,
  remove_recursive/1,
  copy/3,
  copyfile/2,
  relative_from/2,
  realpath/1,
  wildcard/2
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
copy(Source, Destination, Options) ->
  case elists:include(Options, recursive) of
    true ->
      Base = filename:basename(Source),
      Dest = filename:join(Destination, Base),
      case filelib:is_dir(Source) of
        false -> 
          copyfile(Source, Dest);
        true ->
          case file:read_file_info(Source) of
            {ok, FileInfo} ->
              case make_dir(Dest) of
                ok ->
                  case file:write_file_info(Dest, FileInfo) of
                    ok -> ok;
                    {error, Reason} ->
                      error(Reason)
                  end,
                  SubFiles = sub_files(Source),
                  SubFiles1 = case lists:keyfind(exclude, 1, Options) of
                                {exclude, []} -> SubFiles;
                                {exclude, ExcludedFiles} ->
                                  elists:delete_if(
                                    fun(File) ->
                                        lists:any(
                                          fun(Exclude) ->
                                              string:str(expand_path(File), Exclude) =/= 0
                                          end, ExcludedFiles)
                                    end, SubFiles)
                              end,
                  SubFiles2 = case lists:keyfind(only, 1, Options) of
                                {only, []} ->
                                  SubFiles1;
                                {only, OnlyFiles} ->
                                  elists:delete_if(
                                    fun(File) ->
                                        lists:all(
                                          fun(Only) ->
                                              string:str(expand_path(File), Only) =:= 0
                                          end, OnlyFiles)
                                    end, SubFiles1)
                              end,
                  lists:foreach(fun(File) ->
                                    copy(File, Dest, Options)
                                end, SubFiles2);
                {error, Reason} ->
                  error(Reason)
              end;
            {error, Reason} ->
              error(Reason)
          end
      end;
    false ->
      copyfile(Source, Destination)
  end.

copyfile(Source, Destination) ->
  case file:read_file_info(Source) of
    {ok, FileInfo} ->
      case file:copy(Source, Destination) of
        {error, Reason} ->
          error(Reason);
        _ -> 
          case file:write_file_info(Destination, FileInfo) of
            ok -> ok;
            {error, Reason} ->
              error(Reason)
          end
      end;
    {error, Reason} ->
      error(Reason)
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

%% @doc
%% @end
wildcard(Path, Exclude) ->
  elists:delete_if(fun(P) ->
                       lists:any(fun(E) ->
                                     string:str(expand_path(P), E) > 0
                                 end, Exclude)
                   end, filelib:wildcard(Path)).

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
