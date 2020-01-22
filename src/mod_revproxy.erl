%%%===================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc HTTP(S) reverse proxy for MongooseIM's Cowboy listener
%%% @end
%%%===================================================================
-module(mod_revproxy).
-behaviour(gen_mod).
-behaviour(cowboy_handler).
-behaviour(mongoose_module_metrics).

%% API
-export([compile/1]).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_http_handler callbacks
-export([init/2,
         terminate/3]).

%% to be used by tests only
-export([compile_routes/1,
         match/4,
         upstream_uri/1,
         split/4]).

-include("mod_revproxy.hrl").

-record(state, {timeout, length, custom_headers}).

-type option() :: {atom(), any()}.
-type state() :: #state{}.

-type host()     :: '_' | string() | binary().
-type path()     :: '_' | string() | binary().
-type method()   :: '_' | atom() | string() | binary().
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
-spec start(jid:server(), [option()]) -> ok.
start(_Host, Opts) ->
    Routes = gen_mod:get_opt(routes, Opts, []),
    compile(Routes).

-spec stop(jid:server()) -> ok.
stop(_Host) ->
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_handler callbacks
%%--------------------------------------------------------------------
-spec init(cowboy_req:req(), [option()])
    -> {ok, cowboy_req:req(), state()}.
init(Req, Opts) ->
    Timeout = gen_mod:get_opt(timeout, Opts, 5000),
    Length = gen_mod:get_opt(body_length, Opts, 8000000),
    Headers = gen_mod:get_opt(custom_headers, Opts, []),
    State = #state{timeout=Timeout,
                   length=Length,
                   custom_headers=Headers},
    handle(Req, State).

-spec handle(cowboy_req:req(), state()) -> {ok, cowboy_req:req(), state()}.
handle(Req, State) ->
    Host = cowboy_req:header(<<"host">>, Req),
    Path = cowboy_req:path(Req),
    QS = cowboy_req:qs(Req),
    PathQS = case QS of
        <<>> ->
            Path;
        _ ->
            <<Path/binary, "?", QS/binary>>
    end,
    Method = cowboy_req:method(Req),
    Match = match(mod_revproxy_dynamic:rules(), Host, PathQS, Method),
    handle_match(Match, Method, Req, State).

-spec terminate(any(), cowboy_req:req(), state()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Passing and receiving request via fusco
handle_match(#match{}=Match, Method, Req, State) ->
    {Host, Path} = upstream_uri(Match),
    pass_request(Host, Path, Method, Req, State);
handle_match(false, _, Req, State) ->
    Req1 = cowboy_req:reply(404, Req),
    {ok, Req1, State}.

pass_request(Host, Path, Method, Req,
             #state{timeout=Timeout, custom_headers=CustomHeaders}=State) ->
    {ok, Pid} = fusco:start_link(Host, [{connect_timeout, Timeout}]),
    Headers = maps:to_list(cowboy_req:headers(Req)),
    {Body, Req1} = request_body(Req, State),
    Headers1 = Headers ++ CustomHeaders,
    Response = fusco:request(Pid, Path, Method, Headers1, Body, Timeout),
    fusco:disconnect(Pid),
    return_response(Response, Req1, State).

return_response({ok, {{Status, _}, Headers, Body, _, _}}, Req, State) ->
    StatusI = binary_to_integer(Status),
    Headers1 = remove_confusing_headers(Headers),
    Req1 = cowboy_req:reply(StatusI, maps:from_list(Headers1), Body, Req),
    {ok, Req1, State};
return_response({error, connect_timeout}, Req, State) ->
    Req1 = cowboy_req:reply(504, Req),
    {ok, Req1, State};
return_response({error, timeout}, Req, State) ->
    Req1 = cowboy_req:reply(504, Req),
    {ok, Req1, State};
return_response({error, _Other}, Req, State) ->
    Req1 = cowboy_req:reply(502, Req),
    {ok, Req1, State}.

request_body(Req, #state{length=Length}) ->
    case cowboy_req:has_body(Req) of
        false ->
            {<<>>, Req};
        true ->
            {ok, Data, Req1} = cowboy_req:read_body(Req, #{length => Length}),
            {Data, Req1}
    end.

remove_confusing_headers(List) ->
    [Header || {Field, _}=Header <- List,
               not is_header_confusing(cowboy_bstr:to_lower(Field))].

is_header_confusing(<<"transfer-encoding">>) -> true;
is_header_confusing(_)                       -> false.

%% Cowboy-like routing functions
upstream_uri(#match{upstream=Upstream, remainder=Remainder,
                    bindings=Bindings, path=Path}) ->
    #upstream{type=Type,
              protocol=Protocol,
              host=UpHost,
              path=UpPath} = Upstream,
    BoundHost = upstream_bindings(UpHost, $., Bindings, <<>>),
    PathSegments = case {Type, Path} of
        {uri, _} -> UpPath ++ Remainder;
        {_, '_'} -> UpPath ++ Remainder;
        _        -> UpPath ++ Path ++ Remainder
    end,
    BoundPath = upstream_bindings(PathSegments, $/, Bindings, <<>>),
    FullHost = <<Protocol/binary, BoundHost/binary>>,
    FullPath = <<"/", BoundPath/binary>>,
    {binary_to_list(FullHost), FullPath}.

upstream_bindings([], _, _,  Acc) ->
    Acc;
upstream_bindings([<<>>|Tail], S, Bindings, Acc) when Tail =/= [] ->
    upstream_bindings(Tail, S, Bindings, Acc);
upstream_bindings([Binding|Tail], S, Bindings, Acc) when is_atom(Binding) ->
    {Binding, Value} = lists:keyfind(Binding, 1, Bindings),
    upstream_bindings(Tail, S, Bindings, upstream_append(Value, S, Acc));
upstream_bindings([Head|Tail], S, Bindings, Acc) ->
    upstream_bindings(Tail, S, Bindings, upstream_append(Head, S, Acc)).

upstream_append(Value, _, <<>>) ->
    Value;
upstream_append(Value, S, Acc) ->
    <<Acc/binary, S, Value/binary>>.

%% Matching request to the upstream
match(Rules, Host, Path, Method) when is_list(Host), is_list(Path) ->
    match_rules(Rules, Host, Path, Method);
match(Rules, Host, Path, Method) ->
    match(Rules, split_host(Host), split_path(Path), Method).

match_rules([], _, _, _) ->
    false;
match_rules([Rule|Tail], Host, Path, Method) ->
    case match_method(Rule, Host, Path, Method) of
        false ->
            match_rules(Tail, Host, Path, Method);
        Result ->
            Result
    end.

match_method({_, _, '_', _}=Rule, Host, Path, _Method) ->
    match_path(Rule, Host, Path);
match_method({_, _, Method, _}=Rule, Host, Path, Method) ->
    match_path(Rule, Host, Path);
match_method(_, _, _, _) ->
    false.

match_path({_, '_', _, _}=Rule, Host, Path) ->
    match_host(Rule, Host, Path, []);
match_path({_, RulePath, _, _}=Rule, Host, Path) ->
    match_path_segments(RulePath, Path, Rule, Host, []).

match_path_segments([], Remainder, Rule, Host, Bindings) ->
    match_host(Rule, Host, Remainder, Bindings);
match_path_segments([<<>>|T], Remainder, Rule, Host, Bindings) ->
    match_path_segments(T, Remainder, Rule, Host, Bindings);
match_path_segments([H|T1], [H|T2], Rule, Host, Bindings) ->
    match_path_segments(T1, T2, Rule, Host, Bindings);
match_path_segments([Binding|T1], [H|T2], Rule, Host, Bindings)
        when is_atom(Binding) ->
    case match_bindings(Binding, H, Bindings) of
        false ->
            false;
        Bindings1 ->
            match_path_segments(T1, T2, Rule, Host, Bindings1)
    end;
match_path_segments(_, _, _, _, _) ->
    false.

match_host({'_', RulePath, _, Upstream}, _Host, Remainder, Bindings) ->
    #match{upstream = Upstream,
           path = RulePath,
           remainder = Remainder,
           bindings = Bindings};
match_host({RuleHost, Path, _, Upstream}, Host, Remainder, Bindings) ->
    match_host_segments(RuleHost, Host, Upstream, Remainder, Path, Bindings).

match_host_segments([], [], Upstream, Remainder, Path, Bindings) ->
    #match{upstream = Upstream,
           path = Path,
           remainder = Remainder,
           bindings = Bindings};
match_host_segments([H|T1], [H|T2], Upstream, Remainder, Path, Bindings) ->
    match_host_segments(T1, T2, Upstream, Remainder, Path, Bindings);
match_host_segments([Binding|T1], [H|T2], Upstream, Remainder, Path, Bindings)
        when is_atom(Binding) ->
    case match_bindings(Binding, H, Bindings) of
        false ->
            false;
        Bindings1 ->
            match_host_segments(T1, T2, Upstream, Remainder, Path, Bindings1)
    end;
match_host_segments(_, _, _, _, _, _) ->
    false.

match_bindings(Binding, Value, Bindings) ->
    case lists:keyfind(Binding, 1, Bindings) of
        {Binding, Value} ->
            Bindings;
        {Binding, _} ->
            false;
        false ->
            lists:keystore(Binding, 1, Bindings, {Binding, Value})
    end.

%% Rules compilation
compile_routes(Routes) ->
    compile_routes(Routes, []).

compile_routes([], Acc) ->
    lists:reverse(Acc);
compile_routes([{Host, Method, Upstream}|Tail], Acc) ->
    compile_routes([{Host, '_', Method, Upstream}|Tail], Acc);
compile_routes([{HostMatch, PathMatch, MethodMatch, UpstreamMatch}|Tail], Acc) ->
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

compile_upstream(Bin) when is_binary(Bin) ->
    split_upstream(Bin);
compile_upstream(List) when is_list(List) ->
    compile_upstream(list_to_binary(List));
compile_upstream(Atom) when is_atom(Atom) ->
    Atom;
compile_upstream(_) ->
    erlang:error(badarg).

split_host(Host) ->
    split(Host, $., [], <<>>).

split_path(Path) ->
    Split = split(Path, $/, [], <<>>),
    Trailing = include_trailing(Path, $/, Split),
    lists:reverse(Trailing).

split_upstream(<<"http://", Rest/binary>>) ->
    split_upstream(Rest, <<"http://">>);
split_upstream(<<"https://", Rest/binary>>) ->
    split_upstream(Rest, <<"https://">>).

split_upstream(URI, Protocol) ->
    {Host, Path, Type} = case binary:split(URI, <<"/">>) of
        [HostSeg] ->
            {HostSeg, <<>>, host};
        [HostSeg, PathSeg] ->
            {HostSeg, PathSeg, uri}
    end,
    HostSegments = split_host(Host),
    PathSegments = split_path(Path),
    #upstream{type = Type,
              protocol = Protocol,
              host = lists:reverse(HostSegments),
              path = PathSegments}.

include_trailing(<<>>, _, Segments) ->
    Segments;
include_trailing(<<Separator>>, Separator, Segments) ->
    Segments;
include_trailing(Bin, Separator, Segments) ->
    case binary:at(Bin, byte_size(Bin)-1) of
        Separator -> [<<>>|Segments];
        _         -> Segments
    end.

split(<<>>, _S, Segments, <<>>) ->
    Segments;
split(<<>>, _S, Segments, Acc) ->
    [Acc|Segments];
split(<<S, Rest/binary>>, S, Segments, <<>>) ->
    split(Rest, S, Segments, <<>>);
split(<<S, Rest/binary>>, S, Segments, Acc) ->
    split(Rest, S, [Acc|Segments], <<>>);
split(<<$:, Rest/binary>>, S, Segments, <<>>) ->
    {BindingBin, Rest1} = compile_binding(Rest, S, <<>>),
    Binding = binary_to_atom(BindingBin, utf8),
    split(Rest1, S, [Binding|Segments], <<>>);
split(<<$:, D, Rest/binary>>, S, Segments, Acc)
        when D >= $0, D =< $9 ->
    split(Rest, S, Segments, <<Acc/binary, $:, D>>);
split(<<$:, _Rest/binary>>, _S, _Segments, _Acc) ->
    erlang:error(badarg);
split(<<C, Rest/binary>>, S, Segments, Acc) ->
    split(Rest, S, Segments, <<Acc/binary, C>>).

compile_binding(<<>>, _S, <<>>) ->
    erlang:error(badarg);
compile_binding(<<>>, _S, Acc) ->
    {Acc, <<>>};
compile_binding(<<S, Rest/binary>>, S, Acc) ->
    {Acc, Rest};
compile_binding(<<C, Rest/binary>>, S, Acc) ->
    compile_binding(Rest, S, <<Acc/binary, C>>).

%% Dynamically compiled configuration module
mod_revproxy_dynamic_src(Routes) ->
    Rules = compile_routes(Routes),
    lists:flatten(
        ["-module(mod_revproxy_dynamic).
         -export([rules/0]).

         rules() ->
             ", io_lib:format("~p", [Rules]), ".\n"]).
