%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
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
%%
%% @author Michal Piotrowski <michal.piotrowski@erlang-solutions.com>
-module(jingle_sip_helper).

-export([jingle_element/3]).
-export([jingle_iq/3]).
-export([maybe_rewrite_to_phone/1]).
-export([maybe_rewrite_from_phone/2]).

-include("jlib.hrl").

-spec jingle_element(CallID :: binary(), Action :: binary(), [exml:element()]) ->
    exml:element().
jingle_element(CallID, Action, Children) ->
    #xmlel{name = <<"jingle">>,
           attrs = [{<<"xmlns">>, ?JINGLE_NS},
                    {<<"action">>, Action},
                    {<<"sid">>, CallID}],
           children = Children}.

-spec jingle_iq(ToBinary :: jid:literal_jid(), FromBinary :: jid:literal_jid(), exml:element()) ->
    exml:element().
jingle_iq(ToBinary, FromBinary, JingleEl) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"from">>, FromBinary},
                    {<<"to">>, ToBinary},
                    {<<"id">>, uuid:uuid_to_string(uuid:get_v4(), binary_standard)},
                    {<<"type">>, <<"set">>}],
           children = [JingleEl]}.

-spec maybe_rewrite_to_phone(mongoose_acc:t()) -> jid:jid().
maybe_rewrite_to_phone(Acc) ->
    Server = mongoose_acc:lserver(Acc),
    #jid{luser = ToUser} = JID = mongoose_acc:to_jid(Acc),
    ToRewrite = gen_mod:get_module_opt(Server, mod_jingle_sip, username_to_phone, []),
    case lists:keyfind(ToUser, 1, ToRewrite) of
        {ToUser, PhoneNumber} ->
            JID#jid{user = PhoneNumber, luser = PhoneNumber};
        _ ->
            JID
    end.

-spec maybe_rewrite_from_phone(jid:lserver(), binary()) -> jid:luser().
maybe_rewrite_from_phone(Server, <<"+", _/binary>> = PhoneNumber) ->
    try_to_rewrite_from_phone(Server, PhoneNumber);
maybe_rewrite_from_phone(Server, <<"*", _/binary>> = PhoneNumber) ->
    try_to_rewrite_from_phone(Server, PhoneNumber);
maybe_rewrite_from_phone(_, Username) ->
    Username.

try_to_rewrite_from_phone(Server, PhoneNumber) ->
    ToRewrite = gen_mod:get_module_opt(Server, mod_jingle_sip, username_to_phone, []),
    case lists:keyfind(PhoneNumber, 2, ToRewrite) of
        {ToUser, PhoneNumber} ->
            ToUser;
        _ ->
            PhoneNumber
    end.

