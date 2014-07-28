-module(vcard_update).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

-type escalus_client() :: #client{}.

-spec discard_vcard_update(User) -> NDiscarded when
      User :: escalus_client(),
      NDiscarded :: non_neg_integer().
discard_vcard_update(User) ->
    discard_vcard_update(User,
                         {mod_vcard_xupdate, has_mod_vcard_xupdate()}).

discard_vcard_update(User, {mod_vcard_xupdate, true}) ->
    do_discard_vcard_update(User),
    1;
discard_vcard_update(_, _) ->
    0.

do_discard_vcard_update(Client) ->
    Presence = escalus_connection:get_stanza(Client, discard_vcard_update),
    escalus:assert(fun is_vcard_update/1, Presence).

is_vcard_update(#xmlel{name = <<"presence">>} = Stanza) ->
    case exml_query:subelement(Stanza, <<"x">>) of
        undefined -> false;
        X ->
            escalus_pred:has_ns(?NS_VCARD_UPDATE, X)
    end;
is_vcard_update(_) ->
    false.

has_mod_vcard_xupdate() ->
    Server = escalus_ct:get_config(ejabberd_domain),
    escalus_ejabberd:rpc(gen_mod, is_loaded,
                         [server_string(Server), mod_vcard_xupdate]).

server_string(BString) when is_binary(BString) ->
    case server_string_type() of
        binary -> BString;
        list -> binary_to_list(BString)
    end;
server_string(String) when is_list(String) ->
    case server_string_type() of
        list -> String;
        binary -> list_to_binary(String)
    end.

-spec server_string_type() -> list | binary | no_return().
server_string_type() ->
    try
        is_ejabberd_community() andalso throw(binary),
        is_mongooseim() andalso throw(binary),
        try_ejabberd2(),
        error(server_string_type_unknown)
    catch
        throw:binary -> binary;
        throw:list -> list
    end.

-spec is_ejabberd_community() -> boolean().
is_ejabberd_community() ->
    Apps = escalus_ejabberd:rpc(application, which_applications, []),
    case lists:keyfind(ejabberd, 1, Apps) of
        {ejabberd, "ejabberd", "community" ++ _} ->
            true;
        _ ->
            false
    end.

-spec is_mongooseim() -> boolean().
is_mongooseim() ->
    Apps = escalus_ejabberd:rpc(application, which_applications, []),
    case lists:keyfind(ejabberd, 1, Apps) of
        {ejabberd, "ejabberd", "2.1.8+mim" ++ _} ->
            true;
        _ ->
            false
    end.

-spec try_ejabberd2() -> no_return().
try_ejabberd2() ->
    [{config, hosts,
      [XMPPDomain | _]}] = escalus_ejabberd:rpc(ets, lookup,
                                                [config, hosts]),
    case XMPPDomain of
        BString when is_binary(BString) ->
            throw(binary);
        String when is_list(String) ->
            throw(list)
    end.
