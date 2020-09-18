%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
%%%-------------------------------------------------------------------
%%% @doc
%% options and defaults:
%%     * server - (required)
%%     * path_prefix - ""
%%     * request_timeout - 2000,
%%     * http_opts - [] % passed to fusco
%%%
%%% @end
%%% Created : 26. Jun 2018 13:07
%%%-------------------------------------------------------------------
-module(mongoose_http_client).
-author("bartlomiej.gorny@erlang-solutions.com").
-include("mongoose.hrl").

%% API
-export([get/4, post/5]).

-spec get(Host :: jid:lserver() | global,
          PoolTag :: atom(),
          Path :: binary(),
          Headers :: list()) ->
    {ok, {binary(), binary()}} | {error, any()}.
get(Host, PoolTag, Path, Headers) ->
    make_request(Host, PoolTag, Path, <<"GET">>, Headers, <<>>).

-spec post(Host :: jid:lserver() | global,
           PoolTag :: atom(),
           Path :: binary(),
           Headers :: list(),
           Query :: binary()) ->
    {ok, {binary(), binary()}} | {error, any()}.
post(Host, PoolTag, Path, Headers, Query) ->
    make_request(Host, PoolTag, Path, <<"POST">>, Headers, Query).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_request(Host, PoolTag, Path, Method, Headers, Query) ->
    case mongoose_wpool_http:get_params(Host, PoolTag) of
        {ok, PathPrefix, RequestTimeout} ->
            make_request(Host, PoolTag, PathPrefix, RequestTimeout, Path, Method, Headers, Query);
        Err ->
            Err
    end.

make_request(Host, PoolTag, PathPrefix, RequestTimeout, Path, Method, Headers, Query) ->
    FullPath = <<PathPrefix/binary, Path/binary>>,
    Req = {request, FullPath, Method, Headers, Query, 2, RequestTimeout},
    try mongoose_wpool:call(http, Host, PoolTag, Req) of
        {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} ->
            {ok, {Code, RespBody}};
        {error, timeout} ->
            {error, request_timeout};
        {'EXIT', Reason} ->
            {error, {'EXIT', Reason}};
        {error, Reason} ->
            {error, Reason}
    catch
        exit:timeout ->
            {error, pool_timeout};
        exit:no_workers ->
            {error, pool_down};
        Type:Error ->
            {error, {Type, Error}}
    end.
