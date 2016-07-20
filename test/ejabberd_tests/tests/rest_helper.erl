%%%-------------------------------------------------------------------
%%% @author bartekgorny
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2016 17:29
%%%-------------------------------------------------------------------
-module(rest_helper).
-author("bartekgorny").

%% API
-export([
    assert_inlist/2,
    assert_notinlist/2,
    decode_maplist/1,
    gett/1,
    post/2,
    putt/2,
    delete/1
]).

-define(PATHPREFIX, <<"/api">>).


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

assert_inlist(Pattern, L) when is_map(Pattern) ->
    assert_inmaplist(maps:keys(Pattern), Pattern, L, L);
assert_inlist(Pattern, L) ->
    [H|_] = L,
    Fl = lists:filter(fun(X) -> case X of Pattern -> true; _ -> false end end, L),
    case Fl of
        [] ->
            ct:fail(io_lib:format("Fail: ~p not in [~p...]", [Pattern, H]));
        _ ->
            ok
    end.

assert_notinlist(Pattern, L) when is_map(Pattern) ->
    assert_notinmaplist(maps:keys(Pattern), Pattern, L, L);
assert_notinlist(Pattern, L) ->
    [H|_] = L,
    Fl = lists:filter(fun(X) -> case X of Pattern -> true; _ -> false end end, L),
    case Fl of
        [] ->
            ok;
        _ ->
            ct:fail(io_lib:format("Fail: ~p in [~p...]", [Pattern, H]))
    end.

assert_inmaplist([], Map, L, [H|_]) ->
    case L of
        [] ->
            ct:fail(io_lib:format("Fail: ~p not in [~p...]", [Map, H]));
        _ ->
            ok
    end;
assert_inmaplist([K|Keys], Map, L, Orig) ->
    V = maps:get(K, Map),
    Nl = lists:filter(fun(M) -> maps:get(K, M, niema) =:= V end, L),
    assert_inmaplist(Keys, Map, Nl, Orig).


assert_notinmaplist([], Map, L, [H|_]) ->
    case L of
        [] ->
            ok;
        _ ->
            ct:fail(io_lib:format("Fail: ~p in [~p...]", [Map, H]))
    end;
assert_notinmaplist([K|Keys], Map, L, Orig) ->
    V = maps:get(K, Map),
    Nl = lists:filter(fun(M) -> maps:get(K, M, niema) =:= V end, L),
    assert_notinmaplist(Keys, Map, Nl, Orig).


gett(Path) ->
    make_request(<<"GET">>, Path).

post(Path, Body) ->
    make_request(<<"POST">>, Path, Body).

putt(Path, Body) ->
    make_request(<<"PUT">>, Path, Body).

delete(Path) ->
    make_request(<<"DELETE">>, Path).

make_request(Method, Path) ->
    make_request(Method, Path, <<"">>).

make_request(Method, Path, ReqBody) when is_map(ReqBody) ->
    make_request(Method, Path, jiffy:encode(ReqBody));
make_request(Method, Path, ReqBody) when not is_binary(Path) ->
    make_request(Method, list_to_binary(Path), ReqBody);
make_request(Method, Path, ReqBody) ->
    CPath = <<?PATHPREFIX/binary, Path/binary>>,
    {Code, RespBody} = case fusco_request(Method, CPath, ReqBody) of
                           {RCode, _, Body, _, _} ->
                               {RCode, Body};
                           {RCode, _, Body, _, _, _} ->
                               {RCode, Body}
                       end,
    {Code, decode(RespBody)}.

decode(<<>>) ->
    <<"">>;
decode(RespBody) ->
    jiffy:decode(RespBody).

fusco_request(Method, Path, Body) ->
    fusco_request(Method, Path, Body, []).

fusco_request(Method, Path, Body, HeadersIn) ->
    {ok, Client} = fusco_cp:start_link({"localhost", 5288, false}, [], 1),
    Headers = [{<<"Content-Type">>, <<"application/json">>} | HeadersIn],
    {ok, Result} = fusco_cp:request(Client, Path, Method, Headers, Body, 2, 5000),
    Result.

mapfromlist(L) ->
    Nl = lists:keymap(fun(B) -> binary_to_existing_atom(B, utf8) end, 1, L),
    maps:from_list(Nl).

decode_maplist(Ml) ->
    Pl = [C || {C} <- Ml],
    [mapfromlist(L) || L <- Pl].
