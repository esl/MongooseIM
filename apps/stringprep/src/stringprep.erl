%%%----------------------------------------------------------------------
%%% File    : stringprep.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to stringprep
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@proces-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(stringprep).

-author('alexey@process-one.net').

-export([start/0, load_nif/0, load_nif/1, tolower/1, nameprep/1,
	 nodeprep/1, resourceprep/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start() ->
    application:start(p1_stringprep).

load_nif() ->
    load_nif(get_so_path()).

load_nif(LibDir) ->
    SOPath = filename:join(LibDir, "stringprep"),
    case catch erlang:load_nif(SOPath, 0) of
        ok ->
            ok;
        Err ->
            error_logger:warning_msg("unable to load stringprep NIF: ~p~n", [Err]),
            Err
    end.

-spec tolower(iodata()) -> binary() | error.
tolower(_String) ->
    erlang:nif_error(nif_not_loaded).

-spec nameprep(iodata()) -> binary() | error.
nameprep(_String) ->
    erlang:nif_error(nif_not_loaded).

-spec nodeprep(iodata()) -> binary() | error.
nodeprep(_String) ->
    erlang:nif_error(nif_not_loaded).

-spec resourceprep(iodata()) -> binary() | error.
resourceprep(_String) ->
    erlang:nif_error(nif_not_loaded).

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_so_path() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    filename:join([AppDir, "priv", "lib"]).

%%%===================================================================
%%% Unit tests
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

load_nif_test() ->
    ?assertEqual(ok, load_nif(filename:join(["..", "priv", "lib"]))).

badarg_test() ->
    ?assertError(badarg, ?MODULE:nodeprep(foo)),
    ?assertError(badarg, ?MODULE:nameprep(123)),
    ?assertError(badarg, ?MODULE:resourceprep({foo, bar})),
    ?assertError(badarg, ?MODULE:tolower(fun() -> ok end)).

empty_string_test() ->
    ?assertEqual(<<>>, ?MODULE:nodeprep(<<>>)),
    ?assertEqual(<<>>, ?MODULE:nameprep(<<>>)),
    ?assertEqual(<<>>, ?MODULE:resourceprep(<<>>)),
    ?assertEqual(<<>>, ?MODULE:tolower(<<>>)).

'@_nodeprep_test'() ->
    ?assertEqual(error, ?MODULE:nodeprep(<<"@">>)).

tolower_test() ->
    ?assertEqual(<<"abcd">>, ?MODULE:tolower(<<"AbCd">>)).

resourceprep_test() ->
    ?assertEqual(
       <<95,194,183,194,176,226,137,136,88,46,209,130,208,189,206,
         181,32,208,188,97,206,183,32,195,143,197,139,32,196,174,
         209,143,207,131,206,174,32,208,188,97,115,208,186,46,88,
         226,137,136,194,176,194,183,95>>,
       ?MODULE:resourceprep(
          <<95,194,183,194,176,226,137,136,88,46,209,130,208,189,
            206,181,32,208,188,194,170,206,183,32,195,143,197,139,
            32,196,174,209,143,207,131,206,174,32,208,188,194,170,
            115,208,186,46,88,226,137,136,194,176,194,183,95>>)).

nameprep_fail_test() ->
    ?assertEqual(error,
                 ?MODULE:nameprep(<<217,173,65,112,107,97,119,97,217,173>>)).

-endif.
