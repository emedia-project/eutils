-module(ecode).

-export([priv_dir/1]).

priv_dir(App) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      EbinDir = filename:dirname(code:which(App)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Path ->
      Path
  end.

