-module(jingle_sip_helper).

-export([maybe_rewrite_to_phone/1]).
-export([maybe_rewrite_from_phone/2]).

-include("jlib.hrl").

-spec maybe_rewrite_to_phone(mongoose_acc:t()) -> jid:jid().
maybe_rewrite_to_phone(Acc) ->
    Server = mongoose_acc:get(server, Acc),
    #jid{luser = ToUser} = JID = mongoose_acc:get(to_jid, Acc),
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

