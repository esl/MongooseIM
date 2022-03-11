-module(mongoose_graphql_roster_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

execute(_Ctx, _Obj, <<"listContacts">>, Args) ->
    list_contacts(Args);
execute(_Ctx, _Obj, <<"getContact">>, Args) ->
    get_contact(Args).

-spec list_contacts(mongoose_graphql:args()) ->
    {ok, mongoose_graphql_roster:contact_list()} | {error, resolver_error()}.
list_contacts(#{<<"user">> := UserJID}) ->
    case mod_roster_api:list_contacts(UserJID) of
        {ok, Contacts} ->
            {ok, [mongoose_graphql_roster:make_ok_roster(C) || C <- Contacts]};
        Error ->
            make_error(Error, #{user => jid:to_binary(UserJID)})
    end.

-spec get_contact(mongoose_graphql:args()) ->
    {ok, mongoose_graphql_roster:contact()} | {error, resolver_error()}.
get_contact(#{<<"user">> := UserJID, <<"contact">> := ContactJID}) ->
    case mod_roster_api:get_contact(UserJID, ContactJID) of
        {ok, Contact} ->
            mongoose_graphql_roster:make_ok_roster(Contact);
        Error ->
            make_error(Error, #{contact => jid:to_binary(ContactJID),
                                user => jid:to_binary(UserJID)})
    end.
