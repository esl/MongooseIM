-module(mongoose_graphql_roster_user_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).

execute(Ctx, _Obj, <<"addContact">>, Args) ->
    add_contact(Ctx, Args);
execute(Ctx, _Obj, <<"addContacts">>, Args) ->
    add_contacts(Ctx, Args);
execute(Ctx, _Obj, <<"subscription">>, Args) ->
    subscription(Ctx, Args);
execute(Ctx, _Obj, <<"deleteContact">>, Args) ->
    delete_contact(Ctx, Args);
execute(Ctx, _Obj, <<"deleteContacts">>, Args) ->
    delete_contacts(Ctx, Args).

-spec add_contact(mongoose_graphql:context(), mongoose_graphql:args()) ->
    mongoose_graphql_roster:binary_result().
add_contact(#{user := UserJID}, #{<<"contact">> := ContactJID,
                                  <<"name">> := Name,
                                  <<"groups">> := Groups}) ->
    mongoose_graphql_roster:add_contact(UserJID, ContactJID, Name, Groups).

-spec add_contacts(mongoose_graphql:context(), mongoose_graphql:args()) ->
    mongoose_graphql_roster:list_binary_result().
add_contacts(#{user := UserJID}, #{<<"contacts">> := Contacts}) ->
    {ok, [mongoose_graphql_roster:add_contact(UserJID, ContactJID, Name, Groups)
          || #{<<"jid">> := ContactJID, <<"name">> := Name, <<"groups">> := Groups} <- Contacts]}.

-spec subscription(mongoose_graphql:context(), mongoose_graphql:args()) ->
    mongoose_graphql_roster:binary_result().
subscription(#{user := UserJID}, #{<<"contact">> := ContactJID, <<"action">> := Action}) ->
    Type = mongoose_graphql_roster:translate_sub_action(Action),
    Res = mod_roster_api:subscription(UserJID, ContactJID, Type),
    format_result(Res, #{contact => jid:to_binary(ContactJID)}).

-spec delete_contact(mongoose_graphql:context(), mongoose_graphql:args()) ->
    mongoose_graphql_roster:binary_result().
delete_contact(#{user := UserJID}, #{<<"contact">> := ContactJID}) ->
    mongoose_graphql_roster:delete_contact(UserJID, ContactJID).

-spec delete_contacts(mongoose_graphql:context(), mongoose_graphql:args()) ->
    mongoose_graphql_roster:list_binary_result().
delete_contacts(#{user := UserJID}, #{<<"contacts">> := ContactJIDs}) ->
    {ok, [mongoose_graphql_roster:delete_contact(UserJID, ContactJID)
          || ContactJID <- ContactJIDs]}.
