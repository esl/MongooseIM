%%%-------------------------------------------------------------------
%%% @author Ludwik Bukowski <ludwik.bukowski@erlang-solutions.com>
%%% @copyright (C) 2015, Ludwik Bukowski
%%% @doc XEP-0202: Entity Time
%%% @end
%%%-------------------------------------------------------------------
-module(mod_block).
-author('ludwik.bukowski@erlang-solutions.com').
-behaviour(gen_mod).
-export([process_iq/3, process_iq_get/5, process_iq_set/5]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").
-define(MOD_PRIVACY, mod_privacy).
-define(BACKEND, mod_privacy_backend).

process_iq(_From, _To, IQ) ->
    SubEl = IQ#iq.sub_el,
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

process_iq_get(_, From, _To,
                #iq{xmlns = ?NS_BLOCKING,
                   sub_el = #xmlel{name = <<"blocklist">>}},
               _) ->
    #jid{luser = LUser, lserver = LServer} = From,
    #userlist{name = _, list = List} = get_blocklist(LUser, LServer),
    Items = lists:map(fun parse_listitem_to_jid/1, List),
    Res = #xmlel{name = <<"blocklist">>,
                 attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
                 children = Items},
    {stop, {result, [Res]}};

process_iq_get(Acc, _, _, _, _) -> Acc.

process_iq_set(_, From, _To,
               #iq{xmlns = ?NS_BLOCKING,
                   sub_el =
                   #xmlel{name = SubElName, children = SubEls}}, PrivacyLists) ->
    #jid{luser = LUser, lserver = LServer} = From,
    case {SubElName, xml:remove_cdata(SubEls)} of
        {<<"block">>, []} -> {error, ?ERR_BAD_REQUEST};
        {<<"block">>, Els} ->
            JIDs = parse_blocklist_items(Els,[]),
            case ?BACKEND:block_user(LUser,LServer, JIDs) of
                {ok,  {ListName, UniqueJIDsList}} ->
                    BlockedBefore = PrivacyLists#userlist.list,
                    UpdatedBlocked = lists:umerge(lists:usort(BlockedBefore), lists:usort(UniqueJIDsList)),           %% Numbers are not fixed
                    RefreshedLists = PrivacyLists#userlist{name=ListName, list=UpdatedBlocked},
                    broadcast_blocking_list(LUser, LServer, block, JIDs),
                    {result, [], RefreshedLists};                                   %% Refreshing state of ejabberd_c2s
                {error, _} ->
                    {error, ?ERR_INTERNAL_SERVER_ERROR};
                _ ->
                    {error, ?ERR_INTERNAL_SERVER_ERROR}
            end;
        {<<"unblock">>, []} ->
            case ?BACKEND:unblock_all(LUser, LServer) of
                {ok, Name} ->
                    Res = #userlist{name=Name, list=[], needdb=false},
                    broadcast_blocking_list(LUser, LServer, unblock, []),
                    {result, [], Res};
                {error, no_default_list} ->
                    broadcast_blocking_list(LUser, LServer, unblock, []),
                    {result, [], PrivacyLists};
                {error, Reason} ->
                    {error, ?ERR_INTERNAL_SERVER_ERROR};
                _ -> {error, ?ERR_INTERNAL_SERVER_ERROR}
            end;
        {<<"unblock">>, Els} ->
            JIDs = parse_blocklist_items(Els,[]),
            case ?BACKEND:unblock_user(LUser,LServer, JIDs) of
                {ok, UniqueJIDs} ->
                    BlockedBefore = PrivacyLists#userlist.list,
                    BlockedBeforeJIDs1 = lists:map(fun(#listitem{value = V}) -> V end, BlockedBefore),
                    BlockedBeforeJIDs2 = lists:map(fun jlib:jid_to_binary/1, BlockedBeforeJIDs1),
                    BlockedBeforeZipped = lists:zip(BlockedBefore, BlockedBeforeJIDs2),
                    Filtered = lists:filter(fun({_, JIDFromItem}) -> not lists:member(JIDFromItem, UniqueJIDs) end,  BlockedBeforeZipped),
                    {ReturnListItem, _} = lists:unzip(Filtered),
                    RefreshedLists = PrivacyLists#userlist{list=ReturnListItem},
                    broadcast_blocking_list(LUser, LServer, unblock, JIDs),
                    {result, [], RefreshedLists};                                    %% Refreshing state of ejabberd_c2s
                {error, no_default_list} ->                                 %% User unblocks contacts but he has not default list.
                    broadcast_blocking_list(LUser, LServer, unblock, JIDs),
                    {result, [], PrivacyLists};
                {error, _} ->
                    {error, ?ERR_INTERNAL_SERVER_ERROR};
                _ -> {error, ?ERR_INTERNAL_SERVER_ERROR}
            end;
        _ -> {error, ?ERR_BAD_REQUEST}
    end;
process_iq_set(Acc, _, _, _, _) -> Acc.



%% Helpers
get_blocklist(LUser, LServer) ->
    case ?BACKEND:get_default_list(LUser, LServer) of
        {ok, {Default, List}} ->
            NeedDb = is_list_needdb(List),
            #userlist{name = Default, list = List, needdb = NeedDb};
        {error, _} ->
            #userlist{}
    end.

parse_listitem_to_jid(Item) ->
    #listitem{value=Value, action = deny} = Item,
    Jid = jlib:jid_to_binary(Value),
    Attrs = [{<<"jid">>, Jid}],
    #xmlel{name = <<"item">>, attrs = Attrs, children =  []}.

is_list_needdb(Items) ->
    lists:any(fun is_item_needdb/1, Items).

is_item_needdb(#listitem{type = subscription}) -> true;
is_item_needdb(#listitem{type = group})        -> true;
is_item_needdb(_)                              -> false.

parse_blocklist_items([], JIDs) -> JIDs;
parse_blocklist_items([#xmlel{name = <<"item">>,
                              attrs = Attrs}
                          | Els],
                      JIDs) ->
    case xml:get_attr(<<"jid">>, Attrs) of
        {value, JIDRaw} ->
            JID = jlib:jid_to_lower(jlib:binary_to_jid(JIDRaw)),
            parse_blocklist_items(Els, [JID | JIDs]);
        false -> parse_blocklist_items(Els, JIDs)
    end;
parse_blocklist_items([_ | Els], JIDs) ->
    parse_blocklist_items(Els, JIDs).


%% Pushes
broadcast_blocking_list(LUser, LServer, Flag, JIDs) ->
    UserJID = jlib:make_jid(LUser, LServer, <<>>),
    ejabberd_router:route(UserJID, UserJID, broadcast_blocking_list_packet(Flag, JIDs)).

%% TODO this is dirty
broadcast_blocking_list_packet(Flag, JIDs) ->
    #xmlel{
        name = <<"broadcast">>,
        children = [{blocking, {Flag, JIDs}}]}.

