%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
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
-module(mod_private_riak).

-behaviour(mod_private).

%% API
-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-export([get_all_nss/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

-spec init(jid:lserver(), list()) -> ok.
init(_Host, _Opts) ->
    ok.

-spec multi_set_data(jid:luser(), jid:lserver(), [{binary(), exml:element()}]) ->
    ok | {error, term()}.
multi_set_data(LUser, LServer, NS2XML) ->
    R = [set_private_data(LUser, LServer, NS, XML) || {NS, XML} <- NS2XML],
    %% check if something returned with error msg
    case lists:keyfind(error, 1, R) of
        {error, Reason} -> {error, Reason};
        false -> ok
    end.

-spec multi_get_data(jid:luser(), jid:lserver(), [{binary(), term()}]) -> [any()].
multi_get_data(LUser, LServer, NS2Def) ->
    [get_private_data(LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

-spec remove_user(jid:luser(), jid:lserver()) -> ok.
remove_user(LUser, LServer) ->
    Bucket = bucket_type(LServer),
    [mongoose_riak:delete(Bucket, key(LUser, NS)) || NS <- get_all_nss(LUser, LServer)],
    ok.

set_private_data(LUser, LServer, NS, XML) ->
    Obj = riakc_obj:new(bucket_type(LServer), key(LUser, NS), exml:to_binary(XML)),
    mongoose_riak:put(Obj).

get_all_nss(LUser, LServer) ->
    {ok, KeysWithUsername} = mongoose_riak:list_keys(bucket_type(LServer)),
    lists:foldl(
        fun(Key, Acc) ->
            case binary:split(Key, <<"/">>) of
                [LUser, ResultKey] -> [ResultKey | Acc];
                _ -> Acc
            end
        end,
        [], KeysWithUsername
    ).

get_private_data(LUser, LServer, NS, Default) ->
    case mongoose_riak:get(bucket_type(LServer), key(LUser, NS)) of
        {ok, Obj} ->
            Value = riakc_obj:get_value(Obj),
            {ok, #xmlel{} = DecodedXML} = exml:parse(Value),
            DecodedXML;
        _ ->
            Default
    end.

bucket_type(LServer) ->
    {gen_mod:get_module_opt(LServer, mod_private, bucket_type, <<"private">>), LServer}.

key(LUser, NS) ->
    <<LUser/binary, "/", NS/binary>>.
