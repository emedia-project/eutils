-module(eapplication).

-export([
	get_env/3,
	set_env_from_file/1,
  set_env_from_config/1,
  set_env/2
       ]).

% @deprecated Use application:get_env/3
get_env(App, Key, Default) ->
  application:get_env(App, Key, Default).

%% @doc
%% @end
-spec set_env_from_file(file:filename()) -> ok | {error, any()}.
set_env_from_file(File) ->
  case file:consult(File) of
    {ok, [Terms]} ->
      set_env_from_config(Terms);
    E ->
      E
  end.

%% @doc
%% @end
-spec set_env_from_config([term()]) -> ok | {error, any()}.
set_env_from_config([]) -> ok;
set_env_from_config([{AppName, AppConfig}|Rest]) ->
  case set_env(AppName, AppConfig) of
    ok -> set_env_from_config(Rest);
    E -> E
  end.

%% @doc
%% @end
-spec set_env(atom(), [term()]) -> ok | {error, any()}.
set_env(_, []) -> ok;
set_env(AppName, [{Key, Value}|Config]) ->
  _ = application:set_env(AppName, Key, Value),
  set_env(AppName, Config).