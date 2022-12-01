-module(mongoose_graphql_roster_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2, null_to_default/2]).

execute(_Ctx, _Obj, <<"addContact">>, Args) ->
    add_contact(Args);
execute(_Ctx, _Obj, <<"addContacts">>, Args) ->
    add_contacts(Args);
execute(_Ctx, _Obj, <<"subscription">>, Args) ->
    subscription(Args);
execute(_Ctx, _Obj, <<"setMutualSubscription">>, Args) ->
    set_mutual_subscription(Args);
execute(_Ctx, _Obj, <<"deleteContact">>, Args) ->
    delete_contact(Args);
execute(_Ctx, _Obj, <<"deleteContacts">>, Args) ->
    delete_contacts(Args);
execute(_Ctx, _Obj, <<"subscribeToAll">>, Args) ->
    subscribe_to_all(Args);
execute(_Ctx, _Obj, <<"subscribeAllToAll">>, Args) ->
    subscribe_all_to_all(Args).

-spec add_contact(mongoose_graphql:args()) -> mongoose_graphql_roster:binary_result().
add_contact(#{<<"user">> := UserJID, <<"contact">> := ContactJID,
              <<"name">> := Name, <<"groups">> := Groups}) ->
    mongoose_graphql_roster:add_contact(UserJID, ContactJID, Name, Groups).

-spec add_contacts(mongoose_graphql:args()) -> mongoose_graphql_roster:list_binary_result().
add_contacts(#{<<"user">> := UserJID, <<"contacts">> := Contacts}) ->
    {ok, [mongoose_graphql_roster:add_contact(UserJID, ContactJID, Name, Groups)
          || #{<<"jid">> := ContactJID, <<"name">> := Name, <<"groups">> := Groups} <- Contacts]}.

-spec subscription(mongoose_graphql:args()) -> mongoose_graphql_roster:binary_result().
subscription(#{<<"user">> := UserJID, <<"contact">> := ContactJID, <<"action">> := Action}) ->
    Type = mongoose_graphql_roster:translate_sub_action(Action),
    Res = mod_roster_api:subscription(UserJID, ContactJID, Type),
    format_result(Res, #{user => jid:to_binary(UserJID),
                         contact => jid:to_binary(ContactJID)}).

-spec set_mutual_subscription(mongoose_graphql:args()) -> mongoose_graphql_roster:binary_result().
set_mutual_subscription(#{<<"userA">> := UserAJID, <<"userB">> := UserBJID,
                          <<"action">> := Action}) ->
    Res = mod_roster_api:set_mutual_subscription(UserAJID, UserBJID, Action),
    format_result(Res, #{userA => jid:to_binary(UserAJID),
                         userB => jid:to_binary(UserBJID)}).

-spec delete_contact(mongoose_graphql:args()) -> mongoose_graphql_roster:binary_result().
delete_contact(#{<<"user">> := UserJID, <<"contact">> := ContactJID}) ->
    mongoose_graphql_roster:delete_contact(UserJID, ContactJID).

-spec delete_contacts(mongoose_graphql:args()) -> mongoose_graphql_roster:list_binary_result().
delete_contacts(#{<<"user">> := UserJID, <<"contacts">> := ContactJIDs}) ->
    {ok, [mongoose_graphql_roster:delete_contact(UserJID, ContactJID)
          || ContactJID <- ContactJIDs]}.

-spec subscribe_to_all(mongoose_graphql:args()) -> mongoose_graphql_roster:list_binary_result().
subscribe_to_all(#{<<"user">> := User, <<"contacts">> := Contacts}) ->
    {ok, do_subscribe_to_all(User, Contacts)}.

-spec subscribe_all_to_all(mongoose_graphql:args()) -> mongoose_graphql_roster:list_binary_result().
subscribe_all_to_all(#{<<"contacts">> := Contacts}) ->
    {ok, do_subscribe_all_to_all(Contacts)}.

-spec do_subscribe_to_all(mongoose_graphql_roster:contact_input(),
                          [mongoose_graphql_roster:contact_input()]) ->
    [mongoose_graphql_roster:binary_result()].
do_subscribe_to_all(User, Contacts) ->
    UserTuple = contact_input_map_to_tuple(User),
    lists:map(fun(Contact) ->
                      ContactTuple = contact_input_map_to_tuple(Contact),
                      Res = mod_roster_api:subscribe_both(UserTuple, ContactTuple),
                      format_result(Res, #{user => jid:to_binary(element(1, UserTuple)),
                                           contact => jid:to_binary(element(1, ContactTuple))})
              end, Contacts).

-spec do_subscribe_all_to_all([mongoose_graphql_roster:contact_input()]) ->
    [mongoose_graphql_roster:binary_result()].
do_subscribe_all_to_all([]) ->
    [];
do_subscribe_all_to_all([_]) ->
    [];
do_subscribe_all_to_all([User | Contacts]) ->
    do_subscribe_to_all(User, Contacts) ++ do_subscribe_all_to_all(Contacts).

contact_input_map_to_tuple(#{<<"jid">> := JID, <<"name">> := Name, <<"groups">> := Groups}) ->
    Groups1 = null_to_default(Groups, []),
    {JID, Name, Groups1}.
