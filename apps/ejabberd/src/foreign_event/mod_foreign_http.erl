%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_foreign_http).
-author('szymon.mentel@erlang-solutions.com').
-behaviour(mod_foreign).

-include("jlib.hrl").
-include("ejabberd.hrl").

-export([make_request/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

make_request(Request, OnResponse) ->
    {Host, Path, Method, Headers, Body} = parse(Request),
    Response = do_make_request(Host, Path, Method, Headers, Body),
    respond(encode(Response), OnResponse),
    ok.

%%--------------------------------------------------------------------
%% Internals: parsing
%%--------------------------------------------------------------------

parse(Request) ->
    {Host, Path} = parse_url(Request),
    {Host, Path, parse_method(Request), parse_headers(Request), parse_body(Request)}.

parse_url(#xmlel{} = Request) ->
    [Url] = exml_query:paths(Request, [{attr, <<"url">>}]),
    do_parse_url(Url).

do_parse_url(<<"http://", Rest/binary>>) ->
    do_parse_url(<<"http://">>, Rest);
do_parse_url(<<"https://", Rest/binary>>) ->
    do_parse_url(<<"https://">>, Rest);
do_parse_url(_) ->
    {error, malformed_url}.

do_parse_url(Scheme, Rest) ->
    [Host, Path] = case binary:split(Rest, [<<"/">>]) of
                       [_H, _P] = R ->
                           R;
                       [H] ->
                           [H, <<"/">>]
                   end,
    {binary_to_list(<<Scheme/binary, Host/binary>>), Path}.

parse_method(#xmlel{} = Request) ->
    [Method] = exml_query:paths(Request, [{attr, <<"method">>}]),
    do_parse_method(Method).

do_parse_method(<<"get">>) ->
    <<"GET">>;
do_parse_method(<<"post">>) ->
    <<"POST">>.

parse_headers(#xmlel{} = Request) ->
    Headers = exml_query:paths(Request, [{element, <<"header">>}]),
    lists:foldl(fun(#xmlel{attrs = [{<<"name">>, Name}],
                           children = [#xmlcdata{content = Value}]},
                    Acc) ->
                        [{Name, Value} | Acc]
                end, [], Headers).

parse_body(#xmlel{} = Request) ->
    exml_query:path(Request, [{element, <<"payload">>}, cdata]).

%%--------------------------------------------------------------------
%% Internals: encoding
%%--------------------------------------------------------------------

encode({{StatusCode, _ReasonPhare}, Headers, Body, _Size, _Time}) ->
    #xmlel{name = <<"response">>, 
           attrs = [
                    {<<"xmlns">>, ?NS_FOREIGN_EVENT_HTTP},
                    {<<"type">>, <<"http">>},
                    {<<"status">>, exml:escape_attr(StatusCode)}
                   ],
           children = [
                       #xmlel{name = <<"payload">>, children = exml:escape_cdata(Body)}
                       | lists:foldl(fun({Name, Value}, Acc) -> 
                                             [#xmlel{name = <<"header">>,
                                                     attrs = [{<<"name">>, exml:escape_attr(Name)}],
                                                     children = [exml:escape_cdata(Value)]}
                                              | Acc ]
                                     end, [], Headers)
                      ]
          }.

%%--------------------------------------------------------------------
%% Internals: request
%%--------------------------------------------------------------------

do_make_request(Host, Path, Method, Headers, Body) ->
    {ok, Client} = fusco:start(Host, []),
    {ok, Response} = fusco:request(Client, Path, Method, Headers, Body, 5000),
    Response.

respond(Response, noreply) ->
    do_nothing;
respond(Response, OnResponse) ->
    OnResponse(Response).
