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
    delete/1,
    gett/2,
    post/3,
    putt/3,
    delete/2
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

-spec gett(Path :: string()|binary(), Cred :: {Username :: binary(), Password :: binary()}) -> term().
gett(Path, Cred) ->
    make_request({<<"GET">>, Cred}, Path).

post(Path, Body, Cred) ->
    make_request({<<"POST">>, Cred}, Path, Body).

putt(Path, Body, Cred) ->
    make_request({<<"PUT">>, Cred}, Path, Body).

delete(Path, Cred) ->
    make_request({<<"DELETE">>, Cred}, Path).


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
    try
        jiffy:decode(RespBody)
    catch
        throw:_ ->
            RespBody
    end.

%% a request specyfying credentials is directed to client http listener
fusco_request({Method, {User, Password}}, Path, Body) ->
    Basic = list_to_binary("basic " ++ base64:encode_to_string(to_list(User) ++ ":"++ to_list(Password))),
    Headers = [{<<"authorization">>, Basic}],
    fusco_request(Method, Path, Body, Headers, 8089);
%% without them it is for admin (secure) interface
fusco_request(Method, Path, Body) ->
    fusco_request(Method, Path, Body, [], 8088).

fusco_request(Method, Path, Body, HeadersIn, Port) ->
    {ok, Client} = fusco_cp:start_link({"localhost", Port, false}, [], 1),
    Headers = [{<<"Content-Type">>, <<"application/json">>} | HeadersIn],
    {ok, Result} = fusco_cp:request(Client, Path, Method, Headers, Body, 2, 10000),
    Result.

mapfromlist(L) ->
    Nl = lists:keymap(fun(B) -> binary_to_existing_atom(B, utf8) end, 1, L),
    maps:from_list(Nl).

decode_maplist(Ml) ->
    Pl = [C || {C} <- Ml],
    [mapfromlist(L) || L <- Pl].


to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_list(V) ->
    V.
