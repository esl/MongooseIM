%%%===================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc HTTP(S) reverse proxy for MongooseIM's Cowboy listener
%%% @end
%%%===================================================================
-module(mod_revproxy).
-behaviour(gen_mod).
-behaviour(cowboy_http_handler).

%% API
-export([compile/1]).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

-record(state, {timeout, length}).

-type option() :: {atom(), any()}.
-type state() :: #state{}.

-type host()     :: '_' | string() | binary().
-type path()     :: '_' | string() | binary().
-type method()   :: '_' | atom() | string() | binary().
-type upstream() :: string() | binary().
-type route()    :: {host(), method(), upstream()} |
                    {host(), path(), method(), upstream()}.

-define(CRLF, "\r\n").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec compile([route()]) -> ok.
compile(Routes) ->
    Source = mod_revproxy_dynamic_src(Routes),
    {Module, Code} = dynamic_compile:from_string(Source),
    code:load_binary(Module, "mod_revproxy_dynamic.erl", Code),
    ok.

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
-spec start(ejabberd:host(), [option()]) -> ok.
start(_Host, Opts) ->
    Routes = gen_mod:get_opt(routes, Opts, []),
    compile(Routes).

-spec stop(ejabberd:host()) -> ok.
stop(_Host) ->
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_handler callbacks
%%--------------------------------------------------------------------
-spec init({atom(), http}, cowboy_req:req(), [option()])
    -> {ok, cowboy_req:req(), state()}.
init(_Transport, Req, Opts) ->
    Timeout = gen_mod:get_opt(timeout, Opts, 5000),
    Length = gen_mod:get_opt(body_length, Opts, 8000000),
    {ok, Req, #state{timeout=Timeout,
                     length=Length}}.

-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
    {Host, Req1} = cowboy_req:header(<<"host">>, Req),
    {Path, Req2} = cowboy_req:path(Req1),
    {Method, Req3} = cowboy_req:method(Req2),
    Match = match(mod_revproxy_dynamic:rules(), Host, Path, Method),
    handle_match(Match, Method, Path, Req3, State).

-spec terminate(any(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Passing and receiving request via fusco
handle_match({_, _}=Upstream, Method, Path, Req, State) ->
    {UpstreamHost, UpstreamPath} = upstream_uri(Upstream, Path),
    pass_request(UpstreamHost, UpstreamPath, Method, Req, State);
handle_match(false, _, _, Req, State) ->
    {ok, Req1} = cowboy_req:reply(404, Req),
    {ok, Req1, State}.

pass_request(Host, Path, Method, Req, #state{timeout=Timeout}=State) ->
    {ok, Pid} = fusco:start_link(Host, [{connect_timeout, Timeout}]),
    {Headers, Req1} = cowboy_req:headers(Req),
    {Body, Req2} = request_body(Req1, State),
    Response = fusco:request(Pid, Path, Method, Headers, Body, Timeout),
    return_response(Response, Req2, State).

return_response({ok, {{Status, _}, Headers, Body, _, _}}, Req, State) ->
    StatusI = binary_to_integer(Status),
    {ok, Req1} = cowboy_req:reply(StatusI, Headers, Body, Req),
    {ok, Req1, State};
return_response({error, connect_timeout}, Req, State) ->
    {ok, Req1} = cowboy_req:reply(504, Req),
    {ok, Req1, State};
return_response({error, timeout}, Req, State) ->
    {ok, Req1} = cowboy_req:reply(504, Req),
    {ok, Req1, State};
return_response({error, _Other}, Req, State) ->
    {ok, Req1} = cowboy_req:reply(502, Req),
    {ok, Req1, State}.

%% this will only handle bodies that cowboy can read at once
%% (by default bodies up to 8 mb) tbd if chunks should be streamed or rejected
request_body(Req, #state{length=Length}) ->
    case cowboy_req:has_body(Req) of
        false ->
            {<<>>, Req};
        true ->
            {ok, Data, Req1} = cowboy_req:body(Req, [{length, Length}]),
            {Data, Req1}
    end.

%% Cowboy-like routing functions
upstream_uri({uri, URI}, _) ->
    Size = byte_size(URI),
    [Host, Path] = binary:split(URI, <<"/">>, [{scope,{8,Size-8}}]),
    {binary_to_list(Host), << "/", Path/binary >>};
upstream_uri({host, Host}, Path) ->
    {binary_to_list(Host), Path}.

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

compile_routes(Routes) ->
    compile_routes(Routes, []).

compile_routes([], Acc) ->
    lists:reverse(Acc);
compile_routes([{Host, Method, Upstream}|Tail], Acc) ->
    compile_routes([{Host, '_', Method, Upstream}|Tail], Acc);
compile_routes([{HostMatch,PathMatch,MethodMatch,UpstreamMatch}|Tail], Acc) ->
    HostRule = compile_host(HostMatch),
    Method = compile_method(MethodMatch),
    Upstream = compile_upstream(UpstreamMatch),
    PathRule = compile_path(PathMatch),
    Host = {HostRule, PathRule, Method, Upstream},
    compile_routes(Tail, [Host|Acc]).

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

%% Dynamically compiled configuration module
mod_revproxy_dynamic_src(Routes) ->
    Rules = compile_routes(Routes),
    lists:flatten(
        ["-module(mod_revproxy_dynamic).
         -export([rules/0]).

         rules() ->
             ", io_lib:format("~p", [Rules]), ".\n"]).

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
              {"http://localhost:9999", <<"/def/ghi/jkl/index.html">>}},
             {{uri, <<"http://localhost:8888/">>},
              <<"/abc">>,
              {"http://localhost:8888", <<"/">>}},
             {{host, <<"https://localhost:1234">>},
              <<"/">>,
              {"https://localhost:1234", <<"/">>}}],
    [fun() ->
        URL = upstream_uri(Upstream, Path)
     end || {Upstream, Path, URL} <- Tests].

-endif.
