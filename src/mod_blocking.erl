%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2016 18:17
%%%-------------------------------------------------------------------
-module(mod_blocking).

-xep([{xep, 191}, {version, "1.2"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2,
         process_iq_get/5,
         process_iq_set/4,
         update_blocking_list/4,
         stop/1
        ]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-type listitem() :: #listitem{}.

-type blocking_type() :: 'block' | 'unblock'.

start(Host, _Opts) ->
    mod_disco:register_feature(Host, ?NS_BLOCKING),
    ejabberd_hooks:add(privacy_iq_get, Host,
        ?MODULE, process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, Host,
        ?MODULE, process_iq_set, 50),
    ejabberd_hooks:add(c2s_remote_hook, Host,
        ?MODULE, update_blocking_list, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(privacy_iq_get, Host,
        ?MODULE, process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, Host,
        ?MODULE, process_iq_set, 50),
    ejabberd_hooks:delete(c2s_remote_hook, Host,
        ?MODULE, update_blocking_list, 50),
    ok.

-spec update_blocking_list(privacy_state(), atom(), term(), ejabberd_c2s:state()) -> privacy_state().
update_blocking_list(_HandlerState, mod_privacy, {blocking_change, UserList, Action, JIDs}, C2SState) ->
    blocking_push_to_resources(Action, JIDs, C2SState),
    blocking_presence_to_contacts(Action, JIDs, C2SState),
    #privacy_state{userlist = UserList};
update_blocking_list(HandlerState, _, _, _) ->
    HandlerState.

process_iq_get(Acc, _From = #jid{luser = LUser, lserver = LServer},
               _, #iq{xmlns = ?NS_BLOCKING}, _) ->
    Res = case mod_privacy_backend:get_privacy_list(LUser, LServer, <<"blocking">>) of
              {error, not_found} ->
                  {ok, []};
              {ok, L} ->
                  {ok, L};
              E ->
                  {error, E}
          end,
    IqRes = case Res of
                {ok, Lst} ->
                    {result, blocking_query_response(Lst)};
                {error, _} ->
                    {error, mongoose_xmpp_errors:internal_server_error()}
            end,
    mongoose_acc:set(hook, result, IqRes, Acc);
process_iq_get(Val, _, _, _, _) ->
    Val.

process_iq_set(Acc, From, _To, #iq{xmlns = ?NS_BLOCKING, sub_el = SubEl}) ->
    %% collect needed data
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{name = BType} = SubEl,
    Type = parse_command_type(BType),
    Usrs = exml_query:paths(SubEl, [{element, <<"item">>}, {attr, <<"jid">>}]),
    CurrList = case mod_privacy_backend:get_privacy_list(LUser, LServer, <<"blocking">>) of
                  {ok, List} ->
                      List;
                  {error, not_found} ->
                      [];
                  {error, Reason} ->
                      {error, Reason}
              end,
    %% process
    {Acc1, Res} = process_blocking_iq_set(Type, Acc, LUser, LServer, CurrList, Usrs),
    %% respond / notify
    {Acc2, Res1} = complete_iq_set(blocking_command, Acc1, LUser, LServer, Res),
    mongoose_acc:set(hook, result, Res1, Acc2);
process_iq_set(Val, _, _, _) ->
    Val.

parse_command_type(<<"block">>) -> block;
parse_command_type(<<"unblock">>) -> unblock.

%% @doc Set IQ must do the following:
%% * get / create a dedicated privacy list (we call it "blocking")
%% * modify the list
%% * set that list as a default (in backend)
%% * return the list so that c2s can set it as current list
%% * broadcast (push) message to all the user's resources
%% * sent 'unavailable' msg to blocked contacts, or 'available' to unblocked
%%
-spec process_blocking_iq_set(Type :: block | unblock, Acc :: mongoose_acc:t(),
                              LUser:: binary(), LServer:: binary(),
                              CurrList :: [listitem()], Users :: [binary()]) ->
    {mongoose_acc:t(), {ok, [binary()], [listitem()], block | unblock | unblock_all} |
                       {error, exml:element()}}.
%% fail if current default list could not be retrieved
process_blocking_iq_set(_, Acc, _, _, {error, _}, _) ->
    {Acc, {error, mongoose_xmpp_errors:internal_server_error()}};
%% reject block request with empty jid list
process_blocking_iq_set(block, Acc, _, _, _, []) ->
    {Acc, {error, mongoose_xmpp_errors:bad_request()}};
process_blocking_iq_set(Type, Acc, LUser, LServer, CurrList, Usrs) ->
    %% check who is being added / removed
    {NType, Changed, NewList} = blocking_list_modify(Type, Usrs, CurrList),
    case mod_privacy_backend:replace_privacy_list(LUser, LServer, <<"blocking">>, NewList) of
        {error, E} ->
            {error, E};
        ok ->
            case mod_privacy_backend:set_default_list(LUser, LServer, <<"blocking">>) of
                ok ->
                    {Acc, {ok, Changed, NewList, NType}};
                {error, not_found} ->
                    {Acc, {error, mongoose_xmpp_errors:item_not_found()}};
                {error, _Reason} ->
                {Acc, {error, mongoose_xmpp_errors:internal_server_error()}}
            end
    end.

-spec complete_iq_set(atom(), mongoose_acc:t(), term(), term(), term()) ->
    {mongoose_acc:t(), {error, term()} | {result, list() | {result, list(), term()}}}.
complete_iq_set(blocking_command, Acc, _, _, {error, Reason}) ->
    {Acc, {error, Reason}};
complete_iq_set(blocking_command, Acc, LUser, LServer, {ok, Changed, List, Type}) ->
    UserList = #userlist{name = <<"blocking">>, list = List, needdb = false},
    % send the list to all users c2s processes (resources) to make it effective immediately
    Acc1 = push_blocking_info(Acc, LUser, LServer, UserList, Changed, Type),
    % return a response here so that c2s sets the list in its state
    {Acc1, {result, [], UserList}}.

-spec blocking_list_modify(Type :: block | unblock, New :: [binary()], Old :: [listitem()]) ->
    {block|unblock|unblock_all, [binary()], [listitem()]}.
blocking_list_modify(block, Change, Old) ->
    N = make_blocking_list(Change),
    {_, O} = remove_from(Change, Old),
    %% we treat all items on the "to block" list as changed becase they might have been present
    %% on the old list with different settings
    %% and we need to set order numbers, doesn't matter how but it has to be unique
    {block, Change, set_order(N ++ O)};
blocking_list_modify(unblock, [], Old) ->
    %% unblock with empty list means unblocking all contacts
    Rem = [jid:to_binary(J#listitem.value) || J <- Old],
    {unblock_all, Rem, []};
blocking_list_modify(unblock, Change, Old) ->
    {Removed, O} = remove_from(Change, Old),
    {unblock, Removed, O}.

set_order(L) ->
    set_order(1, [], L).

set_order(_, N, []) ->
    N;
set_order(Idx, N, [H|T]) ->
    set_order(Idx + 1, [H#listitem{order = Idx}|N], T).

remove_from(ToRem, Lst) ->
    remove_from(ToRem, [], [], Lst).

remove_from(_, Removed, New, []) ->
    {Removed, New};
remove_from(ToRem, Removed, New, [H|T]) ->
    Bin = jid:to_binary(H#listitem.value),
    case lists:member(Bin, ToRem) of
        true ->
            remove_from(ToRem, [Bin|Removed], New, T);
        false ->
            remove_from(ToRem, Removed, [H|New], T)
    end.

make_blocking_list(L) ->
    make_blocking_list([], L).

make_blocking_list(New, []) ->
    New;
make_blocking_list(New, [H|T]) ->
    case make_blocking_list_entry(H) of
        false ->
            make_blocking_list(New, T);
        Entry ->
            make_blocking_list([Entry|New], T)
    end.

make_blocking_list_entry(J) ->
    case jid:from_binary(J) of
        error ->
            false;
        JID ->
            #listitem{type = jid,
                      match_all = true,
                      %% we have to use another action
                      %% because c2s has to respond differently based on why we deny
                      action = block,
                      value = jid:to_lower(JID)}
    end.

%% @doc send iq confirmation to all of the user's resources
%% if we unblock all contacts then we don't list who's been unblocked
push_blocking_info(Acc, LUser, LServer, UserList, _Changed, unblock_all) ->
    push_blocking_info(Acc, LUser, LServer, UserList, [], unblock);
push_blocking_info(Acc, LUser, LServer, UserList, Changed, Type) ->
    case jid:make(LUser, LServer, <<>>) of
        error ->
            Acc;
        UserJID ->
            ejabberd_sm:run_in_all_sessions(UserJID,
                                            mod_privacy, {blocking_change, UserList, Type, Changed}),
            Acc
    end.

blocking_query_response(Lst) ->
    #xmlel{
        name = <<"blocklist">>,
        attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
        children = [#xmlel{name= <<"item">>,
                           attrs = [{<<"jid">>, jid:to_binary(J#listitem.value)}]} || J <- Lst]}.

-spec blocking_push_to_resources(Action :: blocking_type(),
                                 JIDS :: [binary()],
                                 State :: ejabberd_c2s:state()) -> ok.
blocking_push_to_resources(Action, JIDs, StateData) ->
    SubEl = build_push_xmlel(Action, JIDs),
    PrivPushIQ = #iq{type = set, xmlns = ?NS_BLOCKING,
                     id = <<"push">>,
                     sub_el = [SubEl]},
    T = ejabberd_c2s_state:jid(StateData),
    F = jid:to_bare(T),
    PrivPushEl = jlib:replace_from_to(F, T, jlib:iq_to_xml(PrivPushIQ)),
    ejabberd_router:route(F, T, PrivPushEl),
    ok.

build_push_xmlel(block, JIDs) ->
    build_push_xmlel(<<"block">>, JIDs);
build_push_xmlel(unblock, JIDs) ->
    build_push_xmlel(<<"unblock">>, JIDs);
build_push_xmlel(Action, JIDs) ->
    #xmlel{name = Action,
           attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
           children = lists:map(
               fun(JID) ->
                   #xmlel{name = <<"item">>,
                          attrs = [{<<"jid">>, JID}]}
               end, JIDs)}.

-spec blocking_presence_to_contacts(Action :: blocking_type(),
                                    JIDs :: [binary()],
                                    State :: ejabberd_c2s:state()) -> ok.
blocking_presence_to_contacts(_Action, [], _StateData) ->
    ok;
blocking_presence_to_contacts(Action, [Jid|JIDs], StateData) ->
    Pres = case Action of
               block ->
                   #xmlel{name = <<"presence">>,
                          attrs = [{<<"xml:lang">>, <<"en">>}, {<<"type">>, <<"unavailable">>}]
                   };
               unblock ->
                   mongoose_c2s_presence:get_last_presence(StateData)
           end,
    T = jid:from_binary(Jid),
    case mongoose_c2s_presence:is_subscribed_to_my_presence(T, StateData) of
        true ->
            F = jid:to_bare(ejabberd_c2s_state:jid(StateData)),
            ejabberd_router:route(F, T, Pres);
        false ->
            ok
    end,
    blocking_presence_to_contacts(Action, JIDs, StateData).
