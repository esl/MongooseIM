%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc A backend for storing MAM preferencies using Mnesia.
%%%
%%% All preferencies of each user are stored inside a single row.
%%% All operations are dirty.
%%% @end
%%%-------------------------------------------------------------------
-module(mod_mam_muc_mnesia_dirty_invitation).
-export([start/2,
         save_invitation_time/5,
         invitation_sent_timestamp/5]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

-record(mam_invitation, {room_user, invitation_time}).

start(_Host, _Mod) ->
    mnesia:create_table(mam_invitation,
            [{disc_copies, [node()]},
             {attributes, record_info(fields, mam_invitation)}]),
    ok.

save_invitation_time(_Host, _Mod, ArcID, ArcJID, UserJID) ->
    Key = room_user(ArcID, ArcJID, UserJID),
    case mnesia:dirty_read(mam_invitation, Key) of
        [] -> undefined;
        [#mam_invitation{invitation_time=InvitationTime}] -> InvitationTime
    end.


invitation_sent_timestamp(_Host, _Mod, ArcID, ArcJID, UserJID) ->
    Key = room_user(ArcID, ArcJID, UserJID),
    Invitation = #mam_invitation{
            room_user=Key,
            invitation_time=mod_mam_utils:now_to_microseconds(now())
        },
    mnesia:sync_dirty(fun() ->
            case mnesia:read(mam_invitation, Key) of
                [] -> mnesia:write(Invitation);
                %% Ignore invitation
                [_] -> ok
            end
        end).

remove_archive(_Host, _Mod, ArcID, _ArcJID) ->
    mnesia:sync_dirty(fun() ->
            delete_local_participants(ArcID, {ArcID, x}),
            delete_remote_participants(ArcID, {ArcID, x, x})
        end).

delete_local_participants(ArcID, PrevKey) ->
    case mnesia:next(mam_invitation, PrevKey) of
        '$end_of_table' ->
            ok;
        {ArcID, _} = Key ->
            mnesia:delete(mam_invitation, Key),
            delete_local_participants(ArcID, Key);
        _ ->
            ok
    end.

delete_remote_participants(ArcID, PrevKey) ->
    case mnesia:next(mam_invitation, PrevKey) of
        '$end_of_table' ->
            ok;
        {ArcID, _, _} = Key ->
            mnesia:delete(mam_invitation, Key),
            delete_remote_participants(ArcID, Key);
        _ ->
            ok
    end.

%% ----------------------------------------------------------------------
%% Helpers

room_user(ArcID, #jid{lserver=LServer}, #jid{lserver=LServer, luser=UserLUser}) ->
    {ArcID, UserLUser};
room_user(ArcID, #jid{}, #jid{lserver=UserLServer, luser=UserLUser}) ->
    {ArcID, UserLServer, UserLUser}.

