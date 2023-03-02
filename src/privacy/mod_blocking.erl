-module(mod_blocking).

-xep([{xep, 191}, {version, "1.3"}]).

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2]).
-export([stop/1]).
-export([hooks/1]).
-export([deps/2]).
-export([supported_features/0]).
-export([config_spec/0]).

-export([
         process_iq_get/3,
         process_iq_set/3,
         disco_local_features/3,
         user_send_iq/3,
         foreign_event/3
        ]).

-include("jlib.hrl").
-include("mod_privacy.hrl").

-type blocking_type() :: block | unblock.
-type listitem() :: #listitem{}.

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

deps(_HostType, Opts) ->
    [{mod_privacy, Opts, hard}].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

config_spec() ->
    mod_privacy:config_spec().

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 99},
     {privacy_iq_get, HostType, fun ?MODULE:process_iq_get/3, #{}, 50},
     {privacy_iq_set, HostType, fun ?MODULE:process_iq_set/3, #{}, 50}
    | c2s_hooks(HostType)].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
c2s_hooks(HostType) ->
    [
     {user_send_iq, HostType, fun ?MODULE:user_send_iq/3, #{}, 50},
     {foreign_event, HostType, fun ?MODULE:foreign_event/3, #{}, 50}
    ].

-spec user_send_iq(mongoose_acc:t(), mongoose_c2s_hooks:params(), map()) ->
    mongoose_c2s_hooks:result().
user_send_iq(Acc, #{c2s_data := StateData}, #{host_type := HostType}) ->
    case mongoose_iq:info(Acc) of
        {#iq{xmlns = ?NS_BLOCKING, type = Type} = IQ, Acc1} when Type == get; Type == set ->
            mod_privacy:do_user_send_iq(Acc1, StateData, HostType, IQ);
        _ ->
            {ok, Acc}
    end.

-spec foreign_event(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
      mongoose_c2s_hooks:result().
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := cast,
                     event_tag := ?MODULE,
                     event_content := {blocking, UserList, Action, Changed}}, _Extra) ->
    {stop, handle_new_blocking_command(Acc, StateData, UserList, Action, Changed)};
foreign_event(Acc, _Params, _Extra) ->
    {ok, Acc}.

handle_new_blocking_command(Acc, StateData, UserList, Action, JIDs) ->
    blocking_push_to_resources(Action, JIDs, StateData),
    blocking_presence_to_contacts(Action, JIDs, StateData),
    ToAcc = [{state_mod, {mod_privacy, UserList}}],
    mongoose_c2s_acc:to_acc_many(Acc, ToAcc).

-spec blocking_push_to_resources(blocking_type(), [binary()], mongoose_c2s:data()) -> ok.
blocking_push_to_resources(Action, JIDs, StateData) ->
    SubEl = case Action of
                block -> blocking_stanza(JIDs, <<"block">>);
                unblock -> blocking_stanza(JIDs, <<"unblock">>)
            end,
    PrivPushIQ = blocking_iq(SubEl),
    T = mongoose_c2s:get_jid(StateData),
    F = jid:to_bare(T),
    PrivPushEl = jlib:replace_from_to(F, T, jlib:iq_to_xml(PrivPushIQ)),
    ejabberd_router:route(F, T, PrivPushEl),
    ok.

-spec blocking_presence_to_contacts(blocking_type(), [binary()], mongoose_c2s:data()) -> ok.
blocking_presence_to_contacts(_Action, [], _StateData) -> ok;
blocking_presence_to_contacts(Action, [Jid | JIDs], StateData) ->
    Presences = mod_presence:maybe_get_handler(StateData),
    Pres = case Action of
               block ->
                   mod_presence:presence_unavailable_stanza();
               unblock ->
                   mod_presence:get(Presences, last)
           end,
    T = jid:from_binary(Jid),
    case mod_presence:is_subscribed_to_my_presence(T, jid:to_bare(T), Presences) of
        true ->
            F = jid:to_bare(mongoose_c2s:get_jid(StateData)),
            ejabberd_router:route(F, T, Pres);
        false ->
            ok
    end,
    blocking_presence_to_contacts(Action, JIDs, StateData).

-spec disco_local_features(mongoose_disco:feature_acc(), map(), map()) ->
    {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := <<>>}, _, _) ->
    {ok, mongoose_disco:add_features([?NS_BLOCKING], Acc)};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

-spec process_iq_get(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{from := jid:jid(), iq := jlib:iq()},
    Extra :: gen_hook:extra().
process_iq_get(Acc, #{from := #jid{luser = LUser, lserver = LServer},
               iq := #iq{xmlns = ?NS_BLOCKING}}, _) ->
    HostType = mongoose_acc:host_type(Acc),
    Res = case mod_privacy_backend:get_privacy_list(HostType, LUser, LServer, <<"blocking">>) of
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
    {ok, mongoose_acc:set(hook, result, IqRes, Acc)};
process_iq_get(Acc, _, _) ->
    {ok, Acc}.

-spec process_iq_set(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{from := jid:jid(), iq := jlib:iq()},
    Extra :: gen_hook:extra().
process_iq_set(Acc, #{from := From, iq := #iq{xmlns = ?NS_BLOCKING, sub_el = SubEl}}, _) ->
    %% collect needed data
    HostType = mongoose_acc:host_type(Acc),
    #jid{luser = LUser, lserver = LServer} = From,
    #xmlel{name = BType} = SubEl,
    Type = parse_command_type(BType),
    Usrs = exml_query:paths(SubEl, [{element, <<"item">>}, {attr, <<"jid">>}]),
    CurrList = case mod_privacy_backend:get_privacy_list(HostType, LUser, LServer, <<"blocking">>) of
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
    {ok, mongoose_acc:set(hook, result, Res1, Acc2)};
process_iq_set(Acc, _, _) ->
    {ok, Acc}.

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
    HostType = mongoose_acc:host_type(Acc),
    {NType, Changed, NewList} = blocking_list_modify(Type, Usrs, CurrList),
    case mod_privacy_backend:replace_privacy_list(HostType, LUser, LServer, <<"blocking">>, NewList) of
        {error, E} ->
            {error, E};
        ok ->
            case mod_privacy_backend:set_default_list(HostType, LUser, LServer, <<"blocking">>) of
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
    broadcast_blocking_command(Acc, LUser, LServer, UserList, Changed, Type),
    % return a response here so that c2s sets the list in its state
    {Acc, {result, [], UserList}}.
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
    Item = {blocking, UserList, Type, Changed},
    UserPids = ejabberd_sm:get_user_present_pids(LUser, LServer),
    HostType = mongoose_acc:host_type(Acc),
    mongoose_hooks:privacy_list_push(HostType, LUser, LServer, Item, length(UserPids)),
    lists:foreach(fun({_, Pid}) -> mongoose_c2s:cast(Pid, ?MODULE, Item) end, UserPids).

-spec blocking_query_response([mod_privacy:list_name()]) -> exml:element().
blocking_query_response(Lst) ->
    #xmlel{
        name = <<"blocklist">>,
        attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
        children = [#xmlel{name= <<"item">>,
                           attrs = [{<<"jid">>, jid:to_binary(J#listitem.value)}]} || J <- Lst]}.

-spec blocking_stanza([binary()], binary()) -> exml:element().
blocking_stanza(JIDs, Name) ->
    #xmlel{name = Name,
           attrs = [{<<"xmlns">>, ?NS_BLOCKING}],
           children = lists:map(
                        fun(JID) ->
                                #xmlel{name = <<"item">>, attrs = [{<<"jid">>, JID}]}
                        end, JIDs)}.

-spec blocking_iq(exml:element()) -> jlib:iq().
blocking_iq(SubEl) ->
    #iq{type = set, xmlns = ?NS_BLOCKING, id = <<"push">>, sub_el = [SubEl]}.
