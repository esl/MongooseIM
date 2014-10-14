%%%===================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc HTTP(S) reverse proxy for MongooseIM's Cowboy listener
%%% @end
%%%===================================================================
-module(mod_revproxy).
-behaviour(gen_mod).
-behaviour(cowboy_http_handler).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

-export([compile/1,
         match/4,
         rebuild_req/1]).

-record(state, {}).
-type state() :: #state{}.
-type option() :: {atom(), any()}.

-define(CRLF, "\n\r").
%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
-spec start(ejabberd:host(), [option()]) -> any().
start(_Host, Opts) ->
    Routes = gen_mod:get_opt(routes, Opts, []),
    ModSrc = mod_revproxy_dynamic_src(Routes),
    {Mod, Code} = dynamic_compile:from_string(ModSrc),
    code:load_binary(Mod, "mod_revproxy_dynamic.erl", Code),
    ok.

stop(_Host) ->
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_handler callbacks
%%--------------------------------------------------------------------
%% @todo specs

init(_Transport, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {Host, Req1} = cowboy_req:header(<<"host">>, Req),
    {Path, Req2} = cowboy_req:path(Req1),
    {Method, Req3} = cowboy_req:method(Req2),
    Match = match(mod_revproxy_dynamic:rules(), Host, Path, Method),
    handle_match(Match, Req3, State).

terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

handle_match({_, _}=Upstream, Req, State) ->
    io:format("Match for ~p~n", [Upstream]),
    {ok, Req, State};
handle_match(false, Req, State) ->
    {ok, Req1} = cowboy_req:reply(404, Req),
    {ok, Req1, State}.

mod_revproxy_dynamic_src(Routes) ->
    Rules = compile(Routes),
    lists:flatten(
        ["-module(mod_revproxy_dynamic).
         -export([rules/0]).

         rules() ->
             ", io_lib:format("~p", [Rules]), ".\n"]).

%% HTTP request rebuilder
rebuild_req(Req) ->
    {Line, Req1} = request_line(Req),
    {Headers, Req2} = request_headers(Req1),
    {Body, Req3} = request_body(Req2),
    Result = << Line/binary, Headers/binary, ?CRLF, Body/binary >>,
    {Result, Req3}.

request_line(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    {Path, Req2} = cowboy_req:path(Req1),
    {Version, Req3} = cowboy_req:version(Req2),
    VersionBin = atom_to_binary(Version, utf8),
    Line = << Method/binary," ",Path/binary," ",VersionBin/binary,?CRLF >>,
    {Line, Req3}.

request_headers(Req) ->
    {Headers, Req1} = cowboy_req:headers(Req),
    Result = rebuild_headers(Headers, <<>>),
    {Result, Req1}.

rebuild_headers([], Acc) ->
    Acc;
rebuild_headers([{Name, Value}|Tail], Acc) ->
    Acc1 = << Acc/binary, Name/binary, ": ", Value/binary, ?CRLF >>,
    rebuild_headers(Tail, Acc1).

%% this will only handle bodies that cowboy can read at once
%% (by default bodies up to 8 mb) tbd if chunks should be streamed or rejected
request_body(Req) ->
    case cowboy_req:has_body(Req) of
        false ->
            {<<>>, Req};
        true ->
            {ok, Data, Req1} = cowboy_req:body(Req),
            {Data, Req1}
    end.

%% Cowboy-like routing functions
upstream_uri({uri, URI}, _Path) ->
    URI;
upstream_uri({host, Host}, Path) ->
    << Host/binary, Path/binary >>.

match(Rules, Host, Path, Method) when is_list(Host), is_list(Path) ->
    match_rules(Rules, Host, Path, Method);
match(Rules, Host, Path, Method) ->
    match(Rules, split_host(Host), split_path(Path), Method).

match_rules([], _, _, _) ->
    false;
match_rules([Rule|Tail], Host, Path, Method) ->
    case match_method(Rule, Host, Path, Method) of
        {Type, Upstream} ->
            {Type, Upstream};
        _ ->
            match_rules(Tail, Host, Path, Method)
    end.

match_method({_, _, '_', _}=Rule, Host, Path, _Method) ->
    match_path(Rule, Host, Path);
match_method({_, _, Method, _}=Rule, Host, Path, Method) ->
    match_path(Rule, Host, Path);
match_method(_, _, _, _) ->
    false.

match_path({_, '_', _, _}=Rule, Host, _Path) ->
    match_host(Rule, Host);
match_path({_, Path, _, _}=Rule, Host, Path) ->
    match_host(Rule, Host);
match_path(_, _, _) ->
    false.

match_host({'_', _, _, Upstream}, _Host) ->
    Upstream;
match_host({Host, _, _, Upstream}, Host) ->
    Upstream;
match_host({_, _, _, _}, _) ->
    false.

compile(Routes) ->
    compile(Routes, []).

compile([], Acc) ->
    lists:reverse(Acc);
compile([{Host, Method, Upstream}|Tail], Acc) ->
    compile([{Host, '_', Method, Upstream}|Tail], Acc);
compile([{HostMatch,PathMatch,MethodMatch,UpstreamMatch}|Tail], Acc) ->
    HostRule = compile_host(HostMatch),
    Method = compile_method(MethodMatch),
    Upstream = compile_upstream(UpstreamMatch),
    PathRule = compile_path(PathMatch),
    Host = {HostRule, PathRule, Method, Upstream},
    compile(Tail, [Host|Acc]).

compile_host('_') ->
    '_';
compile_host("_") ->
    '_';
compile_host(HostMatch) when is_list(HostMatch) ->
    compile_host(list_to_binary(HostMatch));
compile_host(HostMatch) when is_binary(HostMatch) ->
    split_host(HostMatch).

compile_path('_') ->
    '_';
compile_path("_") ->
    '_';
compile_path(PathMatch) when is_list(PathMatch) ->
    compile_path(iolist_to_binary(PathMatch));
compile_path(PathMatch) when is_binary(PathMatch) ->
    split_path(PathMatch).

compile_method('_') ->
    '_';
compile_method("_") ->
    '_';
compile_method(Bin) when is_binary(Bin) ->
    cowboy_bstr:to_upper(Bin);
compile_method(List) when is_list(List) ->
    compile_method(list_to_binary(List));
compile_method(Atom) when is_atom(Atom) ->
    compile_method(atom_to_binary(Atom, utf8)).

compile_upstream(<< "http://", Rest/binary >> = Bin) ->
    uri_or_host(Rest, Bin);
compile_upstream(<< "https://", Rest/binary >> = Bin) ->
    uri_or_host(Rest, Bin);
compile_upstream(List) when is_list(List) ->
    compile_upstream(list_to_binary(List));
compile_upstream(_) ->
    erlang:error(badarg).

uri_or_host(Url, Upstream) ->
    case binary:match(Url, <<"/">>) of
        nomatch -> {host, Upstream};
        _       -> {uri, Upstream}
    end. 

split_host(Host) ->
    binary:split(Host, <<".">>, [global, trim]).

split_path(<<$/, Path/bits>>) ->
    binary:split(Path, <<"/">>, [global, trim]);
split_path(_) ->
    erlang:error(badarg).

%%--------------------------------------------------------------------
%% Unit tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

example_routes() ->
    [{"domain.com", "/abc", "_", "http://localhost:8080/"},
     {"domain.com", get, "http://localhost:1234"},
     {"static.domain.com", get, "http://localhost:9999/"},
     {"abc.domain.com", "/", "_", "http://localhost:8888"}]. 

compile_test() ->
    Compiled = [{[<<"domain">>,<<"com">>],
                 [<<"abc">>],
                 '_',
                 {uri,<<"http://localhost:8080/">>}},
                {[<<"domain">>,<<"com">>],
                 '_',
                 <<"GET">>,
                 {host,<<"http://localhost:1234">>}},
                {[<<"static">>,<<"domain">>,<<"com">>],
                 '_',
                 <<"GET">>,
                 {uri,<<"http://localhost:9999/">>}},
                {[<<"abc">>,<<"domain">>,<<"com">>],
                 [],
                 '_',
                 {host,<<"http://localhost:8888">>}}],
    Compiled = compile(example_routes()).

match_test_() ->
    Rules = compile(example_routes()),
    Tests = [{"domain.com/abc GET",
              {<<"domain.com">>, <<"/abc">>, <<"GET">>},
              {uri, <<"http://localhost:8080/">>}},
             {"domain.com/abc/def GET",
              {<<"domain.com">>, <<"/abc/def">>, <<"GET">>},
              {host, <<"http://localhost:1234">>}},
             {"unknown.com/somepath GET",
              {<<"unknown.com">>, <<"/somepath">>, <<"GET">>},
              false},
             {"static.domain.com/file.html GET",
              {<<"static.domain.com">>, <<"/file.html">>, <<"GET">>},
              {uri, <<"http://localhost:9999/">>}},
             {"static.domain.com/file.html PUT",
              {<<"static.domain.com">>, <<"/file.html">>, <<"PUT">>},
              false},
             {"domain.com/def POST",
              {<<"domain.com">>, <<"/def">>, <<"POST">>},
              false},
             {"abc.domain.com/ OPTIONS",
              {<<"abc.domain.com">>, <<"/">>, <<"OPTIONS">>},
              {host, <<"http://localhost:8888">>}},
             {"abc.domain.com/a DELETE",
              {<<"abc.domain.com">>, <<"/a">>, <<"DELETE">>},
              false}],
    [{Title, fun() ->
                Upstream = match(Rules, Host, Path, Method)
             end} || {Title, {Host, Path, Method}, Upstream} <- Tests].

url_test_() ->
    Tests = [{{host, <<"http://localhost:9999">>},
              <<"/def/ghi/jkl/index.html">>,
              <<"http://localhost:9999/def/ghi/jkl/index.html">>},
             {{uri, <<"http://localhost:8888/">>},
              <<"/abc">>,
              <<"http://localhost:8888/">>},
             {{host, <<"https://localhost:1234">>},
              <<"/">>,
              <<"https://localhost:1234/">>}],
    [fun() ->
        URL = upstream_uri(Upstream, Path)
     end || {Upstream, Path, URL} <- Tests].

-endif.
