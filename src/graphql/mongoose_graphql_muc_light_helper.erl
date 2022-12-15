-module(mongoose_graphql_muc_light_helper).

-export([make_room/1, make_ok_user/1, blocking_item_to_map/1, prepare_blocking_items/1,
         page_size_or_max_limit/2, null_to_default/2, config_to_map/3]).

-spec page_size_or_max_limit(null | integer(), integer()) -> integer().
page_size_or_max_limit(null, MaxLimit) ->
    MaxLimit;
page_size_or_max_limit(PageSize, MaxLimit) when PageSize > MaxLimit ->
    MaxLimit;
page_size_or_max_limit(PageSize, _MaxLimit) ->
    PageSize.

-spec make_room(mod_muc_light_api:room()) -> map().
make_room(#{jid := JID, aff_users := Users, options := Options}) ->
    Participants = lists:map(fun make_ok_user/1, Users),
    Name = maps:get(<<"roomname">>, Options, null),
    Subject = maps:get(<<"subject">>, Options, null),
    #{<<"jid">> => jid:to_binary(JID), <<"name">> => Name, <<"subject">> => Subject,
      <<"participants">> => Participants, <<"options">> => make_options(Options)}.

make_ok_user({JID, Aff}) ->
    {ok, #{<<"jid">> => JID, <<"affiliation">> => Aff}}.

prepare_blocking_items(Items) ->
    [{What, Action, jid:to_lus(Who)} || #{<<"entity">> := Who, <<"entityType">> := What,
                                          <<"action">> := Action} <- Items].

blocking_item_to_map({What, Action, Who}) ->
    {ok, #{<<"entityType">> => What, <<"action">> => Action, <<"entity">> => Who}}.

make_options(Options) ->
    [{ok, #{<<"key">> => K, <<"value">> => V}} || {K, V} <- lists:sort(maps:to_list(Options))].

null_to_default(null, Default) ->
    Default;
null_to_default(Value, _Default) ->
    Value.

-spec config_to_map(null | binary(), null | binary(), null | [map()]) -> map().
config_to_map(Name, Subject, Options) ->
    NS = [{K, V} || {K, V} <- [{<<"roomname">>, Name}, {<<"subject">>, Subject}], V =/= null],
    maps:merge(options_to_map(Options), maps:from_list(NS)).

options_to_map(null) ->
    #{};
options_to_map(Options) ->
    maps:from_list([{K, V} || #{<<"key">> := K, <<"value">> := V} <- Options]).
