-module(mongoose_graphql_muc_light_helper).

-export([make_room/1, make_ok_user/1]).

-spec make_room(mod_muc_light_api:room()) -> map().
make_room(#{jid := JID, name := Name, subject := Subject, aff_users := Users}) ->
    Participants = lists:map(fun make_ok_user/1, Users),
    #{<<"jid">> => JID, <<"name">> => Name, <<"subject">> => Subject,
      <<"participants">> => Participants}.

make_ok_user({JID, Aff}) ->
    {ok, #{<<"jid">> => JID, <<"affiliation">> => Aff}}.
