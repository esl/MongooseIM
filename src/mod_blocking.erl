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
-export([start/2,
         process_iq_get/5,
         process_iq_set/4,
         stop/1
        ]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-type listitem() :: #listitem{}.

start(Host, _Opts) ->
    mod_disco:register_feature(Host, ?NS_BLOCKING),
    ejabberd_hooks:add(privacy_iq_get, Host,
        ?MODULE, process_iq_get, 50),
    ejabberd_hooks:add(privacy_iq_set, Host,
        ?MODULE, process_iq_set, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(privacy_iq_get, Host,
        ?MODULE, process_iq_get, 50),
    ejabberd_hooks:delete(privacy_iq_set, Host,
        ?MODULE, process_iq_set, 50),
    ok.

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
                    {error, ?ERR_INTERNAL_SERVER_ERROR}
            end,
    mongoose_acc:put(iq_result, IqRes, Acc);
process_iq_get(Val, _, _, _, _) ->
    Val.

process_iq_set(Acc, From, _To, #iq{xmlns = ?NS_BLOCKING, sub_el = SubEl}) ->
    %% collect needed data
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{name = BType} = SubEl,
    Type = binary_to_existing_atom(BType, latin1),
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
    mongoose_acc:put(iq_result, Res1, Acc2);
process_iq_set(Val, _, _, _) ->
    Val.

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
    {ok, [binary()], [listitem()], block | unblock | unblock_all}
    | {error, jlib:xmlel()}.
%% fail if current default list could not be retrieved
process_blocking_iq_set(_, Acc, _, _, {error, _}, _) ->
    {Acc, {error, ?ERR_INTERNAL_SERVER_ERROR}};
%% reject block request with empty jid list
process_blocking_iq_set(block, Acc, _, _, _, []) ->
    {Acc, {error, ?ERR_BAD_REQUEST}};
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
                    {Acc, {error, ?ERR_ITEM_NOT_FOUND}};
                {error, _Reason} ->
                {Acc, {error, ?ERR_INTERNAL_SERVER_ERROR}}
            end
    end.

-spec complete_iq_set(atom(), mongoose_acc:t(), term(), term(), term()) ->
    {mongoose_acc:t(), {error, term()} | {result, list() | {result, list(), term()}}}.
complete_iq_set(blocking_command, Acc, _, _, {error, Reason}) ->
    {Acc, {error, Reason}};
complete_iq_set(blocking_command, Acc, LUser, LServer, {ok, Changed, List, Type}) ->
    UserList = #userlist{name = <<"blocking">>, list = List, needdb = false},
    % send the list to all users c2s processes (resources) to make it effective immediately
    Acc1 = broadcast_blocking_command(Acc, LUser, LServer, UserList, Changed, Type),
    % return a response here so that c2s sets the list in its state
    {Acc1, {result, [], UserList}}.
%%complete_iq_set(blocking_command, _, _, _) ->
%%    {result, []}.

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
broadcast_blocking_command(Acc, LUser, LServer, UserList, _Changed, unblock_all) ->
    broadcast_blocking_command(Acc, LUser, LServer, UserList, [], unblock);
broadcast_blocking_command(Acc, LUser, LServer, UserList, Changed, Type) ->
    case jid:make(LUser, LServer, <<>>) of
        error ->
            Acc;
        UserJID ->
            Bcast = {blocking, UserList, Type, Changed},
            ejabberd_sm:route(UserJID, UserJID, Acc, {broadcast, Bcast})
    end.

blocking_query_response(Lst) ->
    #xmlel{
        name = <<"blocklist">>,
        attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
        children = [#xmlel{name= <<"item">>,
                           attrs = [{<<"jid">>, jid:to_binary(J#listitem.value)}]} || J <- Lst]}.

