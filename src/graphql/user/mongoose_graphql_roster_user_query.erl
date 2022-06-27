-module(mongoose_graphql_roster_user_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

execute(Ctx, _Obj, <<"listContacts">>, Args) ->
    list_contacts(Ctx, Args);
execute(Ctx, _Obj, <<"getContact">>, Args) ->
    get_contact(Ctx, Args).

-spec list_contacts(mongoose_graphql:context(), mongoose_graphql:args()) ->
    {ok, mongoose_graphql_roster:contact_list()} | {error, resolver_error()}.
list_contacts(#{user := UserJID}, #{}) ->
    {ok, Contacts} = mod_roster_api:list_contacts(UserJID),
    {ok, [mongoose_graphql_roster:make_ok_roster(C) || C <- Contacts]}.

-spec get_contact(mongoose_graphql:context(), mongoose_graphql:args()) ->
    {ok, mongoose_graphql_roster:contact()} | {error, resolver_error()}.
get_contact(#{user := UserJID}, #{<<"contact">> := ContactJID}) ->
    case mod_roster_api:get_contact(UserJID, ContactJID) of
        {ok, Contact} ->
            mongoose_graphql_roster:make_ok_roster(Contact);
        Error ->
            make_error(Error, #{contact => jid:to_binary(ContactJID)})
    end.
