-module(mongoose_graphql_roster).

-export([make_ok_roster/1, translate_sub_action/1]).

-export([add_contact/4, delete_contact/2]).

-include("mod_roster.hrl").
-include("mongoose_graphql_types.hrl").

-type binary_result() :: {ok, binary()} | {error, resolver_error()}.
-type list_binary_result() :: {ok, [{ok, binary()} | {error, resolver_error()}]}.
-type contact_input() :: map().
-type contact() :: map().
-type contact_list() :: [{ok, contact()}].

-export_type([binary_result/0, list_binary_result/0, contact_input/0,
              contact/0, contact_list/0]).

-spec add_contact(jid:jid(), jid:jid(), binary(), [binary()]) -> binary_result().
add_contact(UserJID, ContactJID, Name, Groups) ->
    DefaultName =  mongoose_graphql_helper:null_to_default(Name, <<>>),
    DefaultGroups = mongoose_graphql_helper:null_to_default(Groups, []),
    Res = mod_roster_api:add_contact(UserJID, ContactJID, DefaultName, DefaultGroups),
    mongoose_graphql_helper:format_result(Res, #{user => jid:to_binary(UserJID),
                                                 contact => jid:to_binary(ContactJID)}).

-spec delete_contact(jid:jid(), jid:jid()) -> binary_result().
delete_contact(UserJID, ContactJID) ->
    Res = mod_roster_api:delete_contact(UserJID, ContactJID),
    mongoose_graphql_helper:format_result(Res, #{user => jid:to_binary(UserJID),
                                                 contact => jid:to_binary(ContactJID)}).

-spec make_ok_roster(mod_roster:roster()) -> {ok, map()}.
make_ok_roster(#roster{subscription = Sub, ask = Ask, jid = JID, name = Name, groups = Groups}) ->
    {ok, #{<<"jid">> => JID, <<"subscription">> => Sub, <<"ask">> => Ask,
           <<"name">> => Name, <<"groups">> => [{ok, Group} || Group <- Groups]}}.

translate_sub_action(invite) -> subscribe;
translate_sub_action(accept) -> subscribed;
translate_sub_action(cancel) -> unsubscribe;
translate_sub_action(decline) -> unsubscribed.
