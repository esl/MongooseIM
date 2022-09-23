%%%----------------------------------------------------------------------
%%% File    : mod_privacy.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : jabber:iq:privacy support
%%% Created : 21 Jul 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_privacy).
-author('alexey@process-one.net').
-xep([{xep, 16}, {version, "1.6"}]).
-xep([{xep, 126}, {version, "1.1"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod
-export([start/2]).
-export([stop/1]).
-export([deps/2]).
-export([supported_features/0]).
-export([config_spec/0]).

-export([process_iq_set/4,
         process_iq_get/5,
         get_user_list/3,
         check_packet/5,
         remove_user/3,
         remove_domain/3,
         updated_list/3,
         disco_local_features/1,
         remove_unused_backend_opts/1,
         user_send_message_or_presence/3,
         user_send_iq/3,
         user_receive_message/3,
         user_receive_presence/3,
         user_receive_iq/3,
         foreign_event/3
        ]).

%% to be used by mod_blocking only
-export([do_user_send_iq/4]).

-export([config_metrics/1]).

-ignore_xref([
    check_packet/5, get_user_list/3, process_iq_get/5,
    process_iq_set/4, remove_user/3, updated_list/3,
    remove_user/3, remove_domain/3, disco_local_features/1]).

-include("jlib.hrl").
-include("mod_privacy.hrl").
-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-export_type([list_name/0]).
-export_type([list_item/0]).
-export_type([privacy_item_type/0]).

-type maybe_send() :: send | ignore.
-type list_name() :: binary() | none.
-type list_item() :: #listitem{}.
-type privacy_item_type() :: none | jid | group | subscription.

%% ------------------------------------------------------------------
%% gen_mod callbacks
%% ------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_privacy_backend:init(HostType, Opts),
    ejabberd_hooks:add(hooks(HostType)),
    gen_hook:add_handlers(c2s_hooks(HostType)).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_hook:delete_handlers(c2s_hooks(HostType)),
    ejabberd_hooks:delete(hooks(HostType)).

deps(_HostType, _Opts) ->
    [{mod_presence, #{}, hard}].

config_spec() ->
    #section{
       items = #{<<"backend">> => #option{type = atom,
                                          validate = {module, mod_privacy}},
                 <<"riak">> => riak_config_spec()},
       defaults = #{<<"backend">> => mnesia},
       process = fun ?MODULE:remove_unused_backend_opts/1
      }.

riak_config_spec() ->
    #section{
       items = #{<<"defaults_bucket_type">> => #option{type = binary,
                                                       validate = non_empty},
                 <<"names_bucket_type">> => #option{type = binary,
                                                    validate = non_empty},
                 <<"bucket_type">> => #option{type = binary,
                                              validate = non_empty}
                },
       defaults = #{<<"defaults_bucket_type">> => <<"privacy_defaults">>,
                    <<"names_bucket_type">> => <<"privacy_lists_names">>,
                    <<"bucket_type">> => <<"privacy_lists">>},
       include = always
      }.

remove_unused_backend_opts(Opts = #{backend := riak}) -> Opts;
remove_unused_backend_opts(Opts) -> maps:remove(riak, Opts).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

hooks(HostType) ->
    [
     {disco_local_features, HostType, ?MODULE, disco_local_features, 98},
     {privacy_iq_get, HostType, ?MODULE, process_iq_get, 50},
     {privacy_iq_set, HostType, ?MODULE, process_iq_set, 50},
     {privacy_get_user_list, HostType, ?MODULE, get_user_list, 50},
     {privacy_check_packet, HostType, ?MODULE, check_packet, 50},
     {privacy_updated_list, HostType, ?MODULE, updated_list, 50},
     {remove_user, HostType, ?MODULE, remove_user, 50},
     {remove_domain, HostType, ?MODULE, remove_domain, 50},
     {anonymous_purge_hook, HostType, ?MODULE, remove_user, 50}
    ].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:hook_fn()).
c2s_hooks(HostType) ->
    [
     {user_send_message, HostType, fun ?MODULE:user_send_message_or_presence/3, #{}, 10},
     {user_send_presence, HostType, fun ?MODULE:user_send_message_or_presence/3, #{}, 10},
     {user_send_iq, HostType, fun ?MODULE:user_send_iq/3, #{}, 50},
     {user_receive_message, HostType, fun ?MODULE:user_receive_message/3, #{}, 10},
     {user_receive_presence, HostType, fun ?MODULE:user_receive_presence/3, #{}, 10},
     {user_receive_iq, HostType, fun ?MODULE:user_receive_iq/3, #{}, 10},
     {foreign_event, HostType, fun ?MODULE:foreign_event/3, #{}, 50}
    ].

%% ------------------------------------------------------------------
%% Handlers
%% ------------------------------------------------------------------

-spec user_send_message_or_presence(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:hook_result().
user_send_message_or_presence(Acc, #{c2s_data := StateData}, _Extra) ->
    do_privacy_check_send(Acc, StateData).

-spec user_send_iq(mongoose_acc:t(), mongoose_c2s:hook_params(), map()) ->
    mongoose_c2s_hooks:hook_result().
user_send_iq(Acc, #{c2s_data := StateData}, #{host_type := HostType}) ->
    case mongoose_iq:info(Acc) of
        {#iq{xmlns = ?NS_PRIVACY, type = Type} = IQ, Acc1} when Type == get; Type == set ->
            do_user_send_iq(Acc1, StateData, HostType, IQ);
        _ ->
            do_privacy_check_send(Acc, StateData)
    end.

-spec user_receive_message(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:hook_result().
user_receive_message(Acc, #{c2s_data := StateData}, _Extra) ->
    do_privacy_check_receive(Acc, StateData, send).

-spec user_receive_presence(mongoose_acc:t(), mongoose_c2s_hooks:hook_params(), map()) ->
    mongoose_c2s_hooks:hook_result().
user_receive_presence(Acc, #{c2s_data := StateData}, _Extra) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"error">> -> {ok, Acc};
        <<"probe">> -> {ok, Acc};
        <<"invisible">> -> {ok, Acc};
        _ -> do_privacy_check_receive(Acc, StateData, ignore)
    end.

-spec user_receive_iq(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:hook_result().
user_receive_iq(Acc, #{c2s_data := StateData}, _Extra) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"get">> -> do_privacy_check_receive(Acc, StateData, send);
        <<"set">> -> do_privacy_check_receive(Acc, StateData, send);
        _ -> do_privacy_check_receive(Acc, StateData, ignore)
    end.

-spec foreign_event(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:hook_result().
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := info,
                     event_content := {broadcast, {privacy_list, PrivList, PrivListName}}},
              #{host_type := HostType}) ->
    {stop, handle_new_privacy_list(Acc, StateData, HostType, PrivList, PrivListName)};
foreign_event(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec do_privacy_check_send(mongoose_acc:t(), mongoose_c2s:c2s_data()) ->
    mongoose_c2s_hooks:hook_result().
do_privacy_check_send(Acc, StateData) ->
    case s_privacy_check_packet(Acc, StateData, out) of
        {allow, Acc1} ->
            {ok, Acc1};
        {block, Acc1} ->
            {stop, send_back_error(Acc1, not_acceptable_blocked, send)};
        {deny, Acc1} ->
            {stop, send_back_error(Acc1, not_acceptable_cancel, send)}
    end.

-spec do_privacy_check_receive(mongoose_acc:t(), mongoose_c2s:c2s_data(), maybe_send()) ->
    mongoose_c2s_hooks:hook_result().
do_privacy_check_receive(Acc, StateData, MaybeSendError) ->
    case s_privacy_check_packet(Acc, StateData, in) of
        {allow, Acc1} ->
            {ok, Acc1};
        {_, Acc1} ->
            {stop, send_back_error(Acc1, service_unavailable, MaybeSendError)}
    end.

-spec do_user_send_iq(mongoose_acc:t(), mongoose_c2s:hook_params(), mongooseim:host_type(), jlib:iq()) ->
    mongoose_c2s_hooks:hook_result().
do_user_send_iq(Acc1, StateData, HostType, #iq{type = Type, sub_el = SubEl} = IQ) ->
    FromJid = mongoose_acc:from_jid(Acc1),
    ToJid = mongoose_acc:to_jid(Acc1),
    Acc2 = process_privacy_iq(Acc1, HostType, Type, ToJid, StateData),
    Res = mongoose_acc:get(hook, result,
                           {error, mongoose_xmpp_errors:feature_not_implemented(
                                     <<"en">>, <<"Failed to handle the privacy IQ request in
                                                         c2s">>)}, Acc2),
    IQRes = case Res of
                {result, Result} ->
                    IQ#iq{type = result, sub_el = Result};
                {result, Result, _} ->
                    IQ#iq{type = result, sub_el = Result};
                {error, Error} ->
                    IQ#iq{type = error, sub_el = [SubEl, Error]}
            end,
    ejabberd_router:route(ToJid, FromJid, Acc2, jlib:iq_to_xml(IQRes)),
    {stop, Acc2}.

-spec handle_new_privacy_list(
        mongoose_acc:t(), mongoose_c2s:c2s_data(), mongooseim:host_type(), term(), term()) ->
    mongoose_acc:t().
handle_new_privacy_list(Acc, StateData, HostType, PrivList, PrivListName) ->
    OldPrivList = get_handler(StateData),
    case mongoose_hooks:privacy_updated_list(HostType, OldPrivList, PrivList) of
        false ->
            Acc;
        NewPrivList ->
            PrivPushIQ = privacy_list_push_iq(PrivListName),
            Jid = mongoose_c2s:get_jid(StateData),
            BareJid = jid:to_bare(Jid),
            PrivPushEl = jlib:replace_from_to(BareJid, Jid, jlib:iq_to_xml(PrivPushIQ)),
            maybe_update_presence(Acc, StateData, OldPrivList, NewPrivList),
            ToAcc = [{socket_send, PrivPushEl}, {state_mod, {?MODULE, NewPrivList}}],
            mongoose_c2s_acc:to_acc_many(Acc, ToAcc)
    end.

-spec process_privacy_iq(Acc :: mongoose_acc:t(),
                         mongooseim:host_type(),
                         Type :: get | set,
                         ToJid :: jid:jid(),
                         StateData :: mongoose_c2s:c2s_data()) -> mongoose_acc:t().
process_privacy_iq(Acc, HostType, get, ToJid, StateData) ->
    PrivacyList = get_handler(StateData),
    From = mongoose_acc:from_jid(Acc),
    {IQ, Acc1} = mongoose_iq:info(Acc),
    mongoose_hooks:privacy_iq_get(HostType, Acc1, From, ToJid, IQ, PrivacyList);
process_privacy_iq(Acc, HostType, set, ToJid, StateData) ->
    OldPrivList = get_handler(StateData),
    From = mongoose_acc:from_jid(Acc),
    {IQ, Acc1} = mongoose_iq:info(Acc),
    Acc2 = mongoose_hooks:privacy_iq_set(HostType, Acc1, From, ToJid, IQ),
    case mongoose_acc:get(hook, result, undefined, Acc2) of
        {result, _, NewPrivList} ->
            maybe_update_presence(Acc2, StateData, OldPrivList, NewPrivList),
            mongoose_c2s_acc:to_acc(Acc2, state_mod, {?MODULE, NewPrivList});
        _ ->
            Acc2
    end.

maybe_update_presence(Acc, StateData, OldList, NewList) ->
    Jid = mongoose_c2s:get_jid(StateData),
    Presences = mongoose_c2s:get_mod_state(StateData, mod_presence),
    FromS = mod_presence:get(Presences, s_from),
    % Our own jid is added to pres_f, even though we're not a "contact", so for
    % the purposes of this check we don't want it:
    SelfJID = jid:to_bare(Jid),
    FromsExceptSelf = gb_sets:del_element(SelfJID, FromS),
    gb_sets:fold(
      fun(T, Ac) ->
              send_unavail_if_newly_blocked(Ac, Jid, T, OldList, NewList, StateData)
      end, Acc, FromsExceptSelf).

send_unavail_if_newly_blocked(Acc, Jid, ContactJID, OldList, NewList, StateData) ->
    Packet = #xmlel{name = <<"presence">>,
                    attrs = [{<<"type">>, <<"unavailable">>}]},
    %% WARNING: we can not use accumulator to cache privacy check result - this is
    %% the only place where the list to check against changes
    {OldResult, _} = p_privacy_check_packet(Packet, Jid, ContactJID, out, OldList, StateData),
    {NewResult, _} = p_privacy_check_packet(Packet, Jid, ContactJID, out, NewList, StateData),
    do_send_unavail_if_newly_blocked(Acc, OldResult, NewResult, Jid, ContactJID, Packet).

do_send_unavail_if_newly_blocked(Acc, allow, deny, From, To, Packet) ->
    ejabberd_router:route(From, To, Acc, Packet);
do_send_unavail_if_newly_blocked(Acc, _, _, _, _, _) ->
    Acc.

-spec p_privacy_check_packet(Packet :: exml:element() | mongoose_acc:t(),
                           From :: jid:jid(),
                           To :: jid:jid(),
                           Dir :: mongoose_privacy:direction(),
                           PrivList :: mongoose_privacy:userlist(),
                           StateData :: mongoose_c2s:c2s_data()) ->
    {mongoose_privacy:decision(), mongoose_acc:t()}.
p_privacy_check_packet(#xmlel{} = Packet, From, To, Dir, PrivList, StateData) ->
    LServer = mongoose_c2s:get_lserver(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    Acc = mongoose_acc:new(#{host_type => HostType, lserver => LServer, location => ?LOCATION,
                             from_jid => From, to_jid => To, element => Packet}),
    mongoose_privacy:privacy_check_packet(Acc, From, PrivList, To, Dir).

-spec s_privacy_check_packet(mongoose_acc:t(), mongoose_c2s:c2s_data(), mongoose_privacy:direction()) ->
    {mongoose_privacy:decision(), mongoose_acc:t()}.
s_privacy_check_packet(Acc, StateData, Dir) ->
    To = mongoose_acc:to_jid(Acc),
    Jid = mongoose_c2s:get_jid(StateData),
    PrivList = get_handler(StateData),
    mongoose_privacy:privacy_check_packet(Acc, Jid, PrivList, To, Dir).

-spec send_back_error(mongoose_acc:t(), mongoose_xmpp_errors:stanza_error(), maybe_send()) ->
    mongoose_acc:t().
send_back_error(Acc1, ErrorType, send) ->
    Acc2 = mod_amp:check_packet(Acc1, delivery_failed),
    {From, To, _El} = mongoose_acc:packet(Acc2),
    Error = mongoose_xmpp_errors:ErrorType(),
    {Acc3, Err} = jlib:make_error_reply(Acc2, Error),
    ejabberd_router:route(To, From, Acc3, Err),
    Acc3;
send_back_error(Acc, _, _) ->
    Acc.

-spec get_handler(mongoose_c2s:c2s_data()) -> mongoose_privacy:userlist() | {error, not_found}.
get_handler(StateData) ->
    case mongoose_c2s:get_mod_state(StateData, ?MODULE) of
        {error, not_found} -> get_privacy_list(StateData);
        Handler -> Handler
    end.

-spec get_privacy_list(mongoose_c2s:c2s_data()) -> mongoose_privacy:userlist().
get_privacy_list(StateData) ->
    Jid = mongoose_c2s:get_jid(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    mongoose_hooks:privacy_get_user_list(HostType, Jid).

-spec privacy_list_push_iq(binary()) -> jlib:iq().
privacy_list_push_iq(PrivListName) ->
    #iq{type = set, xmlns = ?NS_PRIVACY,
        id = <<"push", (mongoose_bin:gen_from_crypto())/binary>>,
        sub_el = [#xmlel{name = <<"query">>,
                         attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
                         children = [#xmlel{name = <<"list">>,
                                            attrs = [{<<"name">>, PrivListName}]}]}]}.

-spec disco_local_features(mongoose_disco:feature_acc()) -> mongoose_disco:feature_acc().
disco_local_features(Acc = #{node := <<>>}) ->
    mongoose_disco:add_features([?NS_PRIVACY], Acc);
disco_local_features(Acc) ->
    Acc.

process_iq_get(Acc,
               _From = #jid{luser = LUser, lserver = LServer},
               _To,
               #iq{xmlns = ?NS_PRIVACY, sub_el = #xmlel{children = Els}},
               #userlist{name = Active}) ->
    HostType = mongoose_acc:host_type(Acc),
    Res = case xml:remove_cdata(Els) of
              [] ->
                  process_lists_get(HostType, LUser, LServer, Active);
              [#xmlel{name = Name, attrs = Attrs}] ->
                  case Name of
                      <<"list">> ->
                          ListName = xml:get_attr(<<"name">>, Attrs),
                          process_list_get(HostType, LUser, LServer, ListName);
                      _ ->
                          {error, mongoose_xmpp_errors:bad_request()}
                  end;
              _ ->
                  {error, mongoose_xmpp_errors:bad_request()}
          end,
    mongoose_acc:set(hook, result, Res, Acc);
process_iq_get(Val, _, _, _, _) ->
    Val.

process_lists_get(HostType, LUser, LServer, Active) ->
    case mod_privacy_backend:get_list_names(HostType, LUser, LServer) of
        {ok, {Default, ListNames}} ->
            {result, [list_names_query(Active, Default, ListNames)]};
        {error, not_found} ->
            {result, [empty_list_names_query()]};
        {error, _Reason} ->
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

process_list_get(HostType, LUser, LServer, {value, Name}) ->
    case mod_privacy_backend:get_privacy_list(HostType, LUser, LServer, Name) of
        {ok, List} ->
            LItems = lists:map(fun item_to_xml/1, List),
            {result, [list_query_result(Name, LItems)]};
        {error, not_found} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        {error, _Reason} ->
            {error, mongoose_xmpp_errors:internal_server_error()}
    end;
process_list_get(_HostType, _LUser, _LServer, false) ->
    {error, mongoose_xmpp_errors:bad_request()}.

process_iq_set(Acc, From, _To, #iq{xmlns = ?NS_PRIVACY, sub_el = SubEl}) ->
    #xmlel{children = Els} = SubEl,
    HostType = mongoose_acc:host_type(Acc),
    Res = case xml:remove_cdata(Els) of
              [#xmlel{name = Name, attrs = Attrs, children = SubEls}] ->
                  ListName = xml:get_attr(<<"name">>, Attrs),
                  case Name of
                      <<"list">> ->
                          process_list_set(HostType, From, ListName,
                                   xml:remove_cdata(SubEls));
                      <<"active">> ->
                          process_active_set(HostType, From, ListName);
                      <<"default">> ->
                          process_default_set(HostType, From, ListName);
                      _ ->
                          {error, mongoose_xmpp_errors:bad_request()}
                  end;
              _ ->
                  {error, mongoose_xmpp_errors:bad_request()}
          end,
    mongoose_acc:set(hook, result, Res, Acc);
process_iq_set(Val, _, _, _) ->
    Val.

process_default_set(HostType, #jid{luser = LUser, lserver = LServer}, {value, Name}) ->
    case mod_privacy_backend:set_default_list(HostType, LUser, LServer, Name) of
        ok ->
            {result, []};
        {error, not_found} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        {error, _Reason} ->
            {error, mongoose_xmpp_errors:internal_server_error()}
    end;
process_default_set(HostType, #jid{luser = LUser, lserver = LServer}, false) ->
    case mod_privacy_backend:forget_default_list(HostType, LUser, LServer) of
        ok ->
            {result, []};
        {error, _Reason} ->
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

process_active_set(HostType, #jid{luser = LUser, lserver = LServer}, {value, Name}) ->
    case mod_privacy_backend:get_privacy_list(HostType, LUser, LServer, Name) of
        {ok, List} ->
            NeedDb = is_list_needdb(List),
            {result, [], #userlist{name = Name, list = List, needdb = NeedDb}};
        {error, not_found} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        {error, _Reason} ->
            {error, mongoose_xmpp_errors:internal_server_error()}
    end;
process_active_set(_HostType, _UserJID, false) ->
    {result, [], #userlist{}}.

process_list_set(HostType, UserJID, {value, Name}, Els) ->
    case parse_items(Els) of
        false ->
            {error, mongoose_xmpp_errors:bad_request()};
        remove ->
            remove_privacy_list(HostType, UserJID, Name);
        List ->
            replace_privacy_list(HostType, UserJID, Name, List)
    end;
process_list_set(_HostType, _UserJID, false, _Els) ->
    {error, mongoose_xmpp_errors:bad_request()}.

remove_privacy_list(HostType, #jid{luser = LUser, lserver = LServer} = UserJID, Name) ->
    case mod_privacy_backend:remove_privacy_list(HostType, LUser, LServer, Name) of
        ok ->
            UserList = #userlist{name = Name, list = []},
            broadcast_privacy_list(UserJID, Name, UserList),
            {result, []};
        %% TODO if Name == Active -> conflict
        {error, conflict} ->
            {error, mongoose_xmpp_errors:conflict()};
        {error, _Reason} ->
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

replace_privacy_list(HostType, #jid{luser = LUser, lserver = LServer} = UserJID, Name, List) ->
    case mod_privacy_backend:replace_privacy_list(HostType, LUser, LServer, Name, List) of
        ok ->
            NeedDb = is_list_needdb(List),
            UserList = #userlist{name = Name, list = List, needdb = NeedDb},
            broadcast_privacy_list(UserJID, Name, UserList),
            {result, []};
        {error, _Reason} ->
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

is_list_needdb(Items) ->
    lists:any(fun is_item_needdb/1, Items).

is_item_needdb(#listitem{type = subscription}) -> true;
is_item_needdb(#listitem{type = group})        -> true;
is_item_needdb(_)                              -> false.

get_user_list(_, HostType, #jid{luser = LUser, lserver = LServer}) ->
    case mod_privacy_backend:get_default_list(HostType, LUser, LServer) of
        {ok, {Default, List}} ->
            NeedDb = is_list_needdb(List),
            #userlist{name = Default, list = List, needdb = NeedDb};
        {error, _} ->
            #userlist{}
    end.

%% From is the sender, To is the destination.
%% If Dir = out, User@Server is the sender account (From).
%% If Dir = in, User@Server is the destination account (To).
check_packet(Acc, _JID, #userlist{list = []}, _, _Dir) ->
    mongoose_acc:set(hook, result, allow, Acc);
check_packet(Acc, JID,
             #userlist{list = List, needdb = NeedDb},
             {From, To, Name, Type}, Dir) ->
    HostType = mongoose_acc:host_type(Acc),
    PType = packet_directed_type(Dir, packet_type(Name, Type)),
    LJID = case Dir of
               in -> jid:to_lower(From);
               out -> jid:to_lower(To)
           end,
    {Subscription, Groups} =
        case NeedDb of
            true ->
                roster_get_jid_info(HostType, JID, LJID);
            false ->
                {[], []}
        end,
    CheckResult = check_packet_aux(List, PType, Type, LJID, Subscription, Groups),
    mongoose_acc:set(hook, result, CheckResult, Acc).

%% allow error messages
check_packet_aux(_, message, <<"error">>, _JID, _Subscription, _Groups) ->
    allow;
%% if we run of of list items then it is allowed
check_packet_aux([], _PType, _MType, _JID, _Subscription, _Groups) ->
    allow;
%% check packet against next privacy list item
check_packet_aux([Item | List], PType, MType, JID, Subscription, Groups) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    do_check_packet_aux(Type, Action, PType, Value, JID, MType, Subscription, Groups, Item, List).

%% list set by blocking commands (XEP-0191) block all communication, both in and out,
%% for a given JID
do_check_packet_aux(jid, block, message, JID, JID, _, _, _, _, _) ->
    block;
do_check_packet_aux(jid, block, message_out, JID, JID, _, _, _, _, _) ->
    block;
%% then we do more complicated checking
do_check_packet_aux(Type, Action, PType, Value, JID, MType, Subscription, Groups, Item, List) ->
    #listitem{type = Type, value = Value, action = Action} = Item,
    case {is_ptype_match(Item, PType), Type} of
        {true, none} ->
            Action;
        {true, _} ->
            case is_type_match(Type, Value, JID, Subscription, Groups) of
                true ->
                    Action;
                false ->
                    check_packet_aux(List, PType, MType, JID, Subscription, Groups)
            end;
        {false, _} ->
            check_packet_aux(List, PType, MType, JID, Subscription, Groups)
    end.

is_ptype_match(#listitem{match_all = true}, _PType) ->
    true;
is_ptype_match(Item, message) ->
    Item#listitem.match_message;
is_ptype_match(_Item, message_out) ->
    false; % according to xep-0016, privacy lists do not stop outgoing messages (so they say)
is_ptype_match(Item, iq) ->
    Item#listitem.match_iq;
is_ptype_match(Item, presence_in) ->
    Item#listitem.match_presence_in;
is_ptype_match(Item, presence_out) ->
    Item#listitem.match_presence_out;
is_ptype_match(_Item, other) ->
    false.

is_type_match(jid, Value, JID, _Subscription, _Groups) ->
    case Value of
        {<<>>, Server, <<>>} ->
            case JID of
                {_, Server, _} ->
                    true;
                _ ->
                    false
            end;
        {User, Server, <<>>} ->
            case JID of
                {User, Server, _} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            Value == JID
    end;

is_type_match(subscription, Value, _JID, Subscription, _Groups) ->
    Value == Subscription;
is_type_match(group, Value, _JID, _Subscription, Groups) ->
    lists:member(Value, Groups).

remove_user(Acc, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    HostType = mongoose_acc:host_type(Acc),
    R = mod_privacy_backend:remove_user(HostType, LUser, LServer),
    mongoose_lib:log_if_backend_error(R, ?MODULE, ?LINE, {Acc, User, Server}),
    Acc.

-spec remove_domain(mongoose_hooks:simple_acc(),
                    mongooseim:host_type(), jid:lserver()) ->
    mongoose_hooks:simple_acc().
remove_domain(Acc, HostType, Domain) ->
    mod_privacy_backend:remove_domain(HostType, Domain),
    Acc.

updated_list(_, #userlist{name = SameName}, #userlist{name = SameName} = New) -> New;
updated_list(_, Old, _) -> Old.

%% ------------------------------------------------------------------
%% Deserialization
%% ------------------------------------------------------------------

packet_directed_type(out, message) -> message_out;
packet_directed_type(in, message) -> message;
packet_directed_type(in, iq) -> iq;
packet_directed_type(in, presence) -> presence_in;
packet_directed_type(out, presence) -> presence_out;
packet_directed_type(_Dir, _Type) -> other.

packet_type(<<"message">>, _Type) -> message;
packet_type(<<"iq">>, _Type) -> iq;
packet_type(<<"presence">>, Type) ->
    case Type of
        %% notification
        undefined -> presence;
        <<"unavailable">> -> presence;
        %% subscribe, subscribed, unsubscribe,
        %% unsubscribed, error, probe, or other
        _ -> other
    end.

parse_items([]) ->
    remove;
parse_items(Els) ->
    parse_items(Els, []).

parse_items([], Res) ->
    %% Sort the items by their 'order' attribute
    lists:keysort(#listitem.order, Res);
parse_items([#xmlel{name = <<"item">>, attrs = Attrs,
                    children = SubEls} | Els], Res) ->
    Type    = xml:get_attr_s(<<"type">>,   Attrs),
    Value   = xml:get_attr_s(<<"value">>,  Attrs),
    SAction = xml:get_attr_s(<<"action">>, Attrs),
    SOrder  = xml:get_attr_s(<<"order">>,  Attrs),
    Action  = parse_action(SAction),
    Order   = parse_order(SOrder),
    I1 = set_action_and_order(Action, Order),
    I2 = set_type_and_value(Type, Value, I1),
    I3 = set_matches(SubEls, I2),
    parse_items(Els, add_item(I3, Res));
parse_items(_, _Res) ->
    false.

parse_action(<<>>) ->
    false;
parse_action(Action) ->
    binary_to_action_s(Action).

parse_order(<<>>) ->
    false;
parse_order(Order) ->
    validate_order(binary_to_order_s(Order)).

validate_order(Order) when Order >= 0 ->
    Order;
validate_order(_) ->
    false.

set_action_and_order(false, _) ->
    false;
set_action_and_order(_, false) ->
    false;
set_action_and_order(Action, Order) when is_atom(Action), is_integer(Order) ->
    #listitem{action = Action, order = Order}.

set_type_and_value(_Type, _Value, false) ->
    false;
set_type_and_value(<<>>, _Value, Item) ->
    Item;
set_type_and_value(_Type, <<>>, _Item) ->
    false;
set_type_and_value(<<"jid">>, Value, Item) ->
    case jid:from_binary(Value) of
        error ->
            false;
        JID ->
            Item#listitem{type = jid, value = jid:to_lower(JID)}
    end;
set_type_and_value(<<"group">>, Value, Item) ->
    Item#listitem{type = group, value = Value};
set_type_and_value(<<"subscription">>, Value, Item) ->
    case Value of
        <<"none">> ->
            Item#listitem{type = subscription, value = none};
        <<"both">> ->
            Item#listitem{type = subscription, value = both};
        <<"from">> ->
            Item#listitem{type = subscription, value = from};
        <<"to">> ->
            Item#listitem{type = subscription, value = to};
        _ ->
        false
    end.

set_matches(_SubEls, false) ->
    false;
set_matches(SubEls, Item) ->
    parse_matches(Item, xml:remove_cdata(SubEls)).

parse_matches(Item, []) ->
    Item#listitem{match_all = true};
parse_matches(Item, Els) ->
    parse_matches1(Item, Els).

parse_matches1(Item, []) ->
    Item;
parse_matches1(Item, [#xmlel{name = <<"message">>} | Els]) ->
    parse_matches1(Item#listitem{match_message = true}, Els);
parse_matches1(Item, [#xmlel{name = <<"iq">>} | Els]) ->
    parse_matches1(Item#listitem{match_iq = true}, Els);
parse_matches1(Item, [#xmlel{name = <<"presence-in">>} | Els]) ->
    parse_matches1(Item#listitem{match_presence_in = true}, Els);
parse_matches1(Item, [#xmlel{name = <<"presence-out">>} | Els]) ->
    parse_matches1(Item#listitem{match_presence_out = true}, Els);
parse_matches1(_Item, [#xmlel{} | _Els]) ->
    false.

add_item(false, Items) ->
    Items;
add_item(Item, Items) ->
    [Item | Items].

%% ------------------------------------------------------------------
%% Serialization
%% ------------------------------------------------------------------

empty_list_names_query() ->
    #xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, ?NS_PRIVACY}]}.

list_names_query(Active, Default, ListNames) ->
    #xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
        children = list_names(Active, Default, ListNames)}.

list_names(Active, Default, ListNames) ->
    [list_name(<<"active">>, Active) || Active =/= none] ++
    [list_name(<<"default">>, Default) || Default =/= none] ++
    [list_name(<<"list">>, ListName) || ListName <- ListNames].

list_name(Type, Name) ->
    #xmlel{name = Type, attrs = [{<<"name">>, Name}]}.

list_query_result(Name, LItems) ->
    #xmlel{
        name = <<"query">>,
        attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
        children = [
            #xmlel{
                name = <<"list">>,
                attrs = [{<<"name">>, Name}],
                children = LItems}]}.

item_to_xml(Item) ->
    #xmlel{
        name = <<"item">>,
        attrs = item_to_xml_attrs(Item),
        children = item_to_xml_children(Item)}.

item_to_xml_attrs(Item=#listitem{type=none}) ->
    item_to_xml_attrs1(Item);
item_to_xml_attrs(Item=#listitem{type=Type, value=Value}) ->
    [{<<"type">>, type_to_binary(Type)},
     {<<"value">>, value_to_binary(Type, Value)}
     | item_to_xml_attrs1(Item)].

item_to_xml_attrs1(#listitem{action=Action, order=Order}) ->
    [{<<"action">>, action_to_binary(Action)},
     {<<"order">>, order_to_binary(Order)}].

item_to_xml_children(#listitem{match_all=true}) ->
    [];
item_to_xml_children(#listitem{match_all=false,
        match_iq=MatchIQ,
        match_message=MatchMessage,
        match_presence_in=MatchPresenceIn,
        match_presence_out=MatchPresenceOut}) ->
       [#xmlel{name = <<"message">>}        || MatchMessage]
    ++ [#xmlel{name = <<"presence-in">>}    || MatchPresenceIn]
    ++ [#xmlel{name = <<"presence-out">>}   || MatchPresenceOut]
    ++ [#xmlel{name = <<"iq">>}             || MatchIQ].

action_to_binary(Action) ->
    case Action of
    allow -> <<"allow">>;
    deny -> <<"deny">>
    end.

order_to_binary(Order) ->
    integer_to_binary(Order).

binary_to_order(Binary) ->
    binary_to_integer(Binary).

type_to_binary(Type) ->
    case Type of
    jid -> <<"jid">>;
    group -> <<"group">>;
    subscription -> <<"subscription">>
    end.

value_to_binary(Type, Val) ->
    case Type of
    jid -> jid:to_binary(Val);
    group -> Val;
    subscription ->
        case Val of
        both -> <<"both">>;
        to   -> <<"to">>;
        from -> <<"from">>;
        none -> <<"none">>
        end
    end.

binary_to_action(S) ->
    case S of
    <<"allow">> -> allow;
    <<"deny">> -> deny
    end.

binary_to_action_s(Action) ->
    try
        binary_to_action(Action)
    catch error:_ ->
        false
    end.

binary_to_order_s(Order) ->
    try
        binary_to_order(Order)
    catch error:_ ->
        false
    end.

%% ------------------------------------------------------------------
%% Ejabberd
%% ------------------------------------------------------------------

broadcast_privacy_list(UserJID, Name, UserList) ->
    JID = jid:to_bare(UserJID),
    ejabberd_sm:route(JID, JID, broadcast_privacy_list_packet(Name, UserList)).

%% TODO this is dirty
broadcast_privacy_list_packet(Name, UserList) ->
    {broadcast, {privacy_list, UserList, Name}}.

roster_get_jid_info(HostType, ToJID, LJID) ->
    mongoose_hooks:roster_get_jid_info(HostType, ToJID, LJID).

-spec config_metrics(mongooseim:host_type()) -> [{gen_mod:opt_key(), gen_mod:opt_value()}].
config_metrics(HostType) ->
    mongoose_module_metrics:opts_for_module(HostType, ?MODULE, [backend]).
