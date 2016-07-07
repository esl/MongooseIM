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
-define(BACKEND, mod_privacy_backend).
%% API
-export([]).


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

process_iq_get(_, _From = #jid{luser = LUser, lserver = LServer}, _, #iq{xmlns = ?NS_BLOCKING}, _) ->
    Res = case ?BACKEND:get_privacy_list(LUser, LServer, <<"blocking">>) of
              {error, not_found} ->
                  {ok, []};
              {ok, L} ->
                  {ok, L};
              E ->
                  {error, E}
          end,
    case Res of
        {ok, Lst} ->
            {result, blocking_query_response(Lst)};
        {error, _} ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end;
process_iq_get(Val, _, _, _, _) ->
    Val.

process_iq_set(_, From, _To, #iq{xmlns = ?NS_BLOCKING, sub_el = SubEl}) ->
    %% collect needed data
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{name = BType} = SubEl,
    Type = binary_to_existing_atom(BType, latin1),
    Usrs = exml_query:paths(SubEl, [{element, <<"item">>}, {attr, <<"jid">>}]),
    CurrList = case ?BACKEND:get_privacy_list(LUser, LServer, <<"blocking">>) of
                  {ok, List} ->
                      List;
                  {error, not_found} ->
                      [];
                  {error, Reason} ->
                      {error, Reason}
              end,
    %% process
    Res = process_blocking_iq_set(Type, LUser, LServer, CurrList, Usrs),
    %% respond / notify
    complete_iq_set(blocking_command, LUser, LServer, Res);
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
-spec process_blocking_iq_set(Type :: block | unblock, LUser:: binary(), LServer:: binary(),
                              CurrList :: [listitem()], Users :: [binary()]) ->
    {ok, [binary()], [listitem()], block | unblock}
    | {error, jlib:xmlel()}.
%% fail if current default list could not be retrieved
process_blocking_iq_set(_, _, _, {error, _}, _) ->
    {error, ?ERR_INTERNAL_SERVER_ERROR};
%% reject block request with empty jid list
process_blocking_iq_set(block, _, _, _, []) ->
    {error, ?ERR_BAD_REQUEST};
process_blocking_iq_set(Type, LUser, LServer, CurrList, Usrs) ->
    %% check who is being added / removed
    {Changed, NewList} = blocking_list_modify(Type, Usrs, CurrList),
    case ?BACKEND:replace_privacy_list(LUser, LServer, <<"blocking">>, NewList) of
        {error, E} ->
            {error, E};
        ok ->
            case ?BACKEND:set_default_list(LUser, LServer, <<"blocking">>) of
                ok ->
                    {ok, Changed, NewList, Type};
                {error, not_found} ->
                    {error, ?ERR_ITEM_NOT_FOUND};
                {error, _Reason} ->
                    {error, ?ERR_INTERNAL_SERVER_ERROR}
            end
    end.

-spec complete_iq_set(atom(), term(), term(), term()) -> {error, term()} | {result, list() | {result, list(), term()}}.
complete_iq_set(blocking_command, _, _, {error, Reason}) ->
    {error, Reason};
complete_iq_set(blocking_command, LUser, LServer, {ok, Changed, List, Type}) ->
    broadcast_blocking_command(LUser, LServer, Changed, Type),
    %% build a response here so that c2s sets the list in its state
    {result, [], #userlist{name = <<"blocking">>, list = List, needdb = false}}.
%%complete_iq_set(blocking_command, _, _, _) ->
%%    {result, []}.

-spec blocking_list_modify(Type :: block | unblock, New :: [binary()], Old :: [listitem()]) ->
    {[binary()], [listitem()]}.
blocking_list_modify(block, Change, Old) ->
    N = make_blocking_list(Change),
    {_, O} = remove_from(Change, Old),
    %% we treat all items on the "to block" list as changed becase they might have been present on the
    %% old list with different settings
    %% and we need to set order numbers, doesn't matter how but it has to be unique
    {Change, set_order(N ++ O)};
blocking_list_modify(unblock, [], Old) ->
    %% unblock with empty list means unblocking all contacts
    Rem = [jid:to_binary(J#listitem.value) || J <- Old],
    {Rem,[]};
blocking_list_modify(unblock, Change, Old) ->
    {Removed, O} = remove_from(Change, Old),
    {Removed, O}.

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
                      %% we have to use another action because c2s has to respond differently based on why we deny
                      action = block,
                      value = jid:to_lower(JID)}
    end.

%% @doc send iq confirmation to all of the user's resources
broadcast_blocking_command(LUser, LServer, Changed, Type) ->
    case jid:make(LUser, LServer, <<>>) of
        error ->
            ok;
        UserJID ->
            ejabberd_sm:route(UserJID, UserJID, {broadcast, {blocking, Type, Changed}})
    end.

blocking_query_response(Lst) ->
    #xmlel{
        name = <<"blocklist">>,
        attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
        children = [#xmlel{name= <<"item">>,
                           attrs = [{<<"jid">>, jid:to_binary(J#listitem.value)}]} || J <- Lst]}.

