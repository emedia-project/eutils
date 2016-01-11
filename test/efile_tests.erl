-module(efile_tests).

-include_lib("eunit/include/eunit.hrl").

efile_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(t_realpath())
      , ?_test(t_realtive_from())
      , ?_test(t_match())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_realpath() ->
  case os:type() of
    win32 ->
      ?assertEqual("/", efile:realpath("/../../../../..")),
      ?assertEqual("c:\\toto\\titi", efile:realpath("c:\\toto\\titi\\tutu\\tata\\..\\..")),
      ?assertEqual("c:\\toto\\tutu", efile:realpath("c:\\toto\\titi\\..\\tutu\\tata\\..")),
      ?assertEqual("c:\\tutu\\tata", efile:realpath("c:\\toto\\titi\\..\\..\\tutu\\tata")),
      ?assertEqual("c:\\toto", efile:realpath("c:\\..\\toto")),
      ?assertEqual("c:\\", efile:realpath("c:\\toto\\..")),
      ?assertEqual("c:\\", efile:realpath("c:\\toto\\..\\..")),
      ?assertEqual("c:\\toto", efile:realpath("c:\\toto\\.")),
      ?assertEqual("c:\\toto\\titi", efile:realpath("c:\\toto\\.\\titi\\.")),
      ?assertEqual("c:\\toto", efile:realpath("c:\\..\\toto\\titi\\..\\.\\.\\.")),
      ?assertEqual("c:\\", efile:realpath("c:\\..\\..\\..\\..\\.."));
    _ ->
      ?assertEqual("/toto/titi", efile:realpath("/toto/titi/tutu/tata/../..")),
      ?assertEqual("/toto/tutu", efile:realpath("/toto/titi/../tutu/tata/..")),
      ?assertEqual("/tutu/tata", efile:realpath("/toto/titi/../../tutu/tata")),
      ?assertEqual("/toto", efile:realpath("/../toto")),
      ?assertEqual("/", efile:realpath("/toto/..")),
      ?assertEqual("/", efile:realpath("/toto/../..")),
      ?assertEqual("/toto", efile:realpath("/toto/.")),
      ?assertEqual("/toto/titi", efile:realpath("/toto/./titi/.")),
      ?assertEqual("/toto", efile:realpath("/../toto/titi/../././.")),
      ?assertEqual("/", efile:realpath("/../../../../.."))
  end,
  ?assertEqual("../titi", efile:realpath("../titi")),
  ?assertEqual("../toto", efile:realpath("../toto/titi/../././.")),
  ?assertEqual("../../../toto", efile:realpath("../../../toto/titi/../././.")).

t_realtive_from() ->
  ?assertEqual("file.txt", efile:relative_from("/toto/titi/file.txt", "/toto/titi")),
  ?assertEqual("titi/file.txt" , efile:relative_from("/toto/titi/file.txt", "/toto")),
  ?assertEqual("../titi/file.txt" , efile:relative_from("/toto/titi/file.txt", "/toto/tutu")),
  ?assertEqual("../titi/file.txt" , efile:relative_from("toto/titi/file.txt", "toto/tutu")),
  ?assertEqual("../../toto/titi/file.txt" , efile:relative_from("/toto/titi/file.txt", "/tata/tutu")).

t_match() ->
  ?assert(efile:match("a/b/c", "**/b/**")),
  ?assert(efile:match("a/b/c", "*/b/*")),
  ?assert(efile:match("a/b/c", "**/b/*")),
  ?assert(efile:match("a/b/c", "*/b/**")),
  ?assert(efile:match("a/b/c/x", "**/b/**")),
  ?assert(efile:match("a/b/c/x", "*/b/**")),
  ?assert(efile:match("a/b/c/x", "*/b/**")),
  ?assertEqual(false, efile:match("a/b/c/x", "*/b/*")),
  ?assertEqual(false, efile:match("a/b/c/x", "**/b/*")),
  ?assertEqual(false, efile:match("a/b/c", "b")),
  ?assert(efile:match("a/.b/c", "**/.b/**")),
  ?assertEqual(false, efile:match("a/xb/c", "**/.b/**")).

