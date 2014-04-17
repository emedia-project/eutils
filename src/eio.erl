-module(eio).

-export([
  readlines/1,
  cmd/1
  ]).

%% @doc
%% Read a complete file in memory
%%
%% Example:
%% <pre>
%% Data1 = eio:readlines("/my/file.txt").
%% Data2 = eio:readlines(&lt;&lt;"/my/file.txt"&gt;&gt;).
%% </pre>
%% @end
-spec readlines(binary() | string()) -> list().
readlines(FileName) when is_binary(FileName) ->
  readlines(binary_to_list(FileName));
readlines(FileName) when is_list(FileName) ->
  case file:open(FileName, [read]) of
    {ok, Device} ->
      try get_all_lines(Device)
      after file:close(Device)
      end;
    _ -> error
  end.
get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.

%% @doc
%% Execute the given shell command
%%
%% Example:
%% <pre>
%% {RC, Data} = ios:cmd("ls -l").
%% </pre>
%% @end
-spec cmd(string()) -> {integer(), list()}.
cmd(Cmd) ->
  Opt = [stream, exit_status, use_stdio,
         stderr_to_stdout, in, eof],
  P = open_port({spawn, Cmd}, Opt),
  get_cmd_data(P, []).
get_cmd_data(P, D) ->
  receive
    {P, {data, D1}} ->
      get_cmd_data(P, [D|D1]);
    {P, eof} ->
      port_close(P),
      receive
        {P, {exit_status, N}} ->
          {N, lists:reverse(D)}
      end
  end.
