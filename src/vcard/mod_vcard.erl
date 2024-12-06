%%%----------------------------------------------------------------------
%%% File    : mod_vcard.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Vcard management in Mnesia
%%% Created :  2 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%% Store vCards in mnesia to provide "XEP-0054: vcard-temp"
%%% and "XEP-0055: Jabber Search"
%%%
%%% Most of this is now using binaries. The search fields l* in vcard_search
%%% are still stored as lists to allow string prefix search using the match
%%% spec with a trailing element String ++ '_'.
%%%
%%%----------------------------------------------------------------------
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

-module(mod_vcard).
-author('alexey@process-one.net').
-xep([{xep, 54}, {version, "1.2"}]).
-xep([{xep, 55}, {version, "1.3"}]).
-behaviour(gen_mod).
-behaviour(gen_server).
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").
-include("mongoose_rsm.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod handlers
-export([start/2, stop/1,
         hooks/1,
         supported_features/0]).

%% config_spec
-export([config_spec/0,
         process_map_spec/1,
         process_search_spec/1,
         process_search_reported_spec/1]).

%% gen_server handlers
-export([init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3]).

%% mongoose_packet_handler export
-export([process_packet/5]).

%% Hook handlers
-export([process_local_iq/5,
         process_sm_iq/5,
         remove_user/3,
         remove_domain/3,
         set_vcard/3]).

-export([start_link/2]).
-export([default_search_fields/0]).
-export([get_results_limit/1]).
-export([get_default_reported_fields/1]).
-export([unsafe_set_vcard/3]).

%% GDPR related
-export([get_personal_data/3]).

-export([config_metrics/1]).

-ignore_xref([
    process_packet/5,
    start_link/2
]).

-define(PROCNAME, ejabberd_mod_vcard).

-record(state, {search :: boolean(),
                host_type :: mongooseim:host_type()}).

%%--------------------------------------------------------------------
%% gdpr callback
%%--------------------------------------------------------------------

-spec get_personal_data(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: gdpr:personal_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().
get_personal_data(Acc, #{jid := #jid{luser = LUser, lserver = LServer}}, #{host_type := HostType}) ->
    Jid = jid:to_binary({LUser, LServer}),
    Schema = ["jid", "vcard"],
    Entries = case mod_vcard_backend:get_vcard(HostType, LUser, LServer) of
                  {ok, Record} ->
                      SerializedRecords = exml:to_binary(Record),
                      [{Jid, SerializedRecords}];
                  _ -> []
              end,
    {ok, [{vcard, Schema, Entries} | Acc]}.

-spec default_search_fields() -> list().
default_search_fields() ->
    [{<<"User">>, <<"user">>},
     {<<"Full Name">>, <<"fn">>},
     {<<"Given Name">>, <<"first">>},
     {<<"Middle Name">>, <<"middle">>},
     {<<"Family Name">>, <<"last">>},
     {<<"Nickname">>, <<"nick">>},
     {<<"Birthday">>, <<"bday">>},
     {<<"Country">>, <<"ctry">>},
     {<<"City">>, <<"locality">>},
     {<<"Email">>, <<"email">>},
     {<<"Organization Name">>, <<"orgname">>},
     {<<"Organization Unit">>, <<"orgunit">>}].

-spec get_results_limit(mongooseim:host_type()) -> non_neg_integer() | infinity.
get_results_limit(HostType) ->
    case gen_mod:get_module_opt(HostType, mod_vcard, matches) of
        infinity ->
            infinity;
        Val when is_integer(Val) and (Val > 0) ->
            Val
    end.

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

start(HostType, Opts) ->
    mod_vcard_backend:init(HostType, Opts),
    start_iq_handlers(HostType, Opts),
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [HostType, Opts]},
                 transient, 1000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    stop_iq_handlers(HostType),
    stop_backend(HostType),
    gen_server:call(Proc, stop),
    ejabberd_sup:stop_child(Proc),
    ok.

supported_features() -> [dynamic_domains].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {anonymous_purge, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50},
     {set_vcard, HostType, fun ?MODULE:set_vcard/3, #{}, 50},
     {get_personal_data, HostType, fun ?MODULE:get_personal_data/3, #{}, 50}].

start_iq_handlers(HostType, #{iqdisc := IQDisc}) ->
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_VCARD, ejabberd_sm,
                                             fun ?MODULE:process_sm_iq/5, #{}, IQDisc),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_VCARD, ejabberd_local,
                                             fun ?MODULE:process_local_iq/5, #{}, IQDisc).

stop_iq_handlers(HostType) ->
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_VCARD, ejabberd_local),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_VCARD, ejabberd_sm).

stop_backend(HostType) ->
    mod_vcard_backend:tear_down(HostType).

%% Domain registration
maybe_register_search(false, _HostType, _Opts) ->
    ok;
maybe_register_search(true, HostType, Opts) ->
    SubdomainPattern = gen_mod:get_opt(host, Opts),
    PacketHandler = mongoose_packet_handler:new(?MODULE, #{pid => self()}),
    %% Always register, even if search functionality is disabled.
    %% So, we can send 503 error, instead of 404 error.
    mongoose_domain_api:register_subdomain(HostType, SubdomainPattern, PacketHandler).

maybe_unregister_search(false, _HostType) ->
    ok;
maybe_unregister_search(true, HostType) ->
    SubdomainPattern = gen_mod:get_module_opt(HostType, ?MODULE, host),
    mongoose_domain_api:unregister_subdomain(HostType, SubdomainPattern).

%%--------------------------------------------------------------------
%% config_spec
%%--------------------------------------------------------------------

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"host">> => #option{type = string,
                                       validate = subdomain_template,
                                       process = fun mongoose_subdomain_utils:make_subdomain_pattern/1},
                 <<"search">> => #option{type = boolean},
                 <<"backend">> => #option{type = atom,
                                          validate = {module, mod_vcard}},
                 <<"matches">> => #option{type = int_or_infinity,
                                          validate = non_negative},
                 <<"ldap">> => ldap_section()
                },
       defaults = #{<<"iqdisc">> => parallel,
                    <<"host">> => mongoose_subdomain_utils:make_subdomain_pattern("vjud.@HOST@"),
                    <<"search">> => true,
                    <<"backend">> => mnesia,
                    <<"matches">> => 30
       },
       process = fun remove_unused_backend_opts/1
      }.

ldap_section() ->
    CommonLDAPSpec = mongoose_ldap_config:spec(),
    Items = #{
        <<"uids">> => #list{items = mongoose_ldap_config:uids()},
        <<"vcard_map">> => #list{items = ldap_vcard_map_spec()},
        <<"search_fields">> => #list{items = ldap_search_fields_spec()},
        <<"search_reported">> => #list{items = ldap_search_reported_spec()},
        <<"search_operator">> => #option{type = atom,
                                         validate = {enum, ['or', 'and']}},
        <<"binary_search_fields">> => #list{items = #option{type = binary,
                                                            validate = non_empty}}},
    Defaults = #{<<"uids">> => [{<<"uid">>, <<"%u">>}],
                 <<"vcard_map">> => mod_vcard_ldap:default_vcard_map(),
                 <<"search_fields">> => mod_vcard_ldap:default_search_fields(),
                 <<"search_reported">> => mod_vcard_ldap:default_search_reported(),
                 <<"search_operator">> => 'and',
                 <<"binary_search_fields">> => []},
    CommonLDAPSpec#section{items = maps:merge(CommonLDAPSpec#section.items, Items),
                           defaults = maps:merge(CommonLDAPSpec#section.defaults, Defaults),
                           include = always}.

ldap_vcard_map_spec() ->
    #section{
        items = #{<<"vcard_field">> => #option{type = binary,
                                               validate = non_empty},
                  <<"ldap_pattern">> => #option{type = binary,
                                                validate = non_empty},
                  <<"ldap_field">> => #option{type = binary,
                                              validate = non_empty}
                },
        required = all,
        process = fun ?MODULE:process_map_spec/1
    }.

ldap_search_fields_spec() ->
    #section{
        items = #{<<"search_field">> => #option{type = binary,
                                                validate = non_empty},
                  <<"ldap_field">> => #option{type = binary,
                                              validate = non_empty}
                },
        required = all,
        process = fun ?MODULE:process_search_spec/1
    }.

ldap_search_reported_spec() ->
    #section{
        items = #{<<"search_field">> => #option{type = binary,
                                                validate = non_empty},
                  <<"vcard_field">> => #option{type = binary,
                                               validate = non_empty}
                },
        required = all,
        process = fun ?MODULE:process_search_reported_spec/1
    }.

process_map_spec(#{vcard_field := VF, ldap_pattern := LP, ldap_field := LF}) ->
    {VF, LP, [LF]}.

process_search_spec(#{search_field := SF, ldap_field := LF}) ->
    {SF, LF}.

process_search_reported_spec(#{search_field := SF, vcard_field := VF}) ->
    {SF, VF}.

remove_unused_backend_opts(Opts = #{backend := ldap}) -> Opts;
remove_unused_backend_opts(Opts) -> maps:remove(ldap, Opts).

%%--------------------------------------------------------------------
%% mongoose_packet_handler callbacks for search
%%--------------------------------------------------------------------

-spec process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                     Packet :: exml:element(), #{}) -> mongoose_acc:t().
process_packet(Acc, From, To, _Packet, _Extra) ->
    handle_route(Acc, From, To),
    Acc.

handle_route(Acc, From, To) ->
    HostType = mongoose_acc:host_type(Acc),
    {IQ, Acc1} = mongoose_iq:info(Acc),
    LServer = directory_jid_to_server_host(To),
    try do_route(HostType, LServer, From, To, Acc1, IQ)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => vcard_route_failed, acc => Acc,
                         class => Class, reason => Reason, stacktrace => Stacktrace})
    end.

%%--------------------------------------------------------------------
%% gen_server callbacks for search
%%--------------------------------------------------------------------
start_link(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [HostType, Opts], []).

init([HostType, Opts]) ->
    process_flag(trap_exit, true),
    Search = gen_mod:get_opt(search, Opts),
    maybe_register_search(Search, HostType, Opts),
    {ok, #state{host_type = HostType, search = Search}}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_info(_, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{host_type = HostType, search = Search}) ->
    maybe_unregister_search(Search, HostType).

%%--------------------------------------------------------------------
%% Hook handlers
%%--------------------------------------------------------------------
process_local_iq(Acc, _From, _To, IQ = #iq{type = set, sub_el = SubEl}, _Extra) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq(Acc, _From, _To, IQ = #iq{type = get}, _Extra) ->
    DescCData = #xmlcdata{content = [<<"MongooseIM XMPP Server">>,
                                     <<"\nCopyright (c) Erlang Solutions Ltd.">>]},
    {Acc, IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"vCard">>, attrs = [{<<"xmlns">>, ?NS_VCARD}],
                           children = [#xmlel{name = <<"FN">>,
                                              children = [#xmlcdata{content = <<"MongooseIM">>}]},
                                       #xmlel{name = <<"URL">>,
                                              children = [#xmlcdata{content = ?MONGOOSE_URI}]},
                                       #xmlel{name = <<"DESC">>,
                                              children = [DescCData]}
                                      ]}]}}.

-spec process_sm_iq(Acc :: mongoose_acc:t(),
                    From :: jid:jid(),
                    To :: jid:jid(),
                    IQ :: jlib:iq(),
                    Extra :: map()) ->
     {stop, mongoose_acc:t()} | {mongoose_acc:t(), jlib:iq()}.
process_sm_iq(Acc, From, To, IQ = #iq{type = set, sub_el = VCARD}, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    process_sm_iq_set(HostType, From, To, Acc, IQ, VCARD);
process_sm_iq(Acc, From, To, IQ = #iq{type = get, sub_el = VCARD}, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    process_sm_iq_get(HostType, From, To, Acc, IQ, VCARD).

process_sm_iq_set(HostType, From, To, Acc, IQ, VCARD) ->
    #jid{luser = FromUser, lserver = FromVHost} = From,
    #jid{luser = ToUser, lserver = ToVHost, lresource = ToResource} = To,
    Local = ((FromUser == ToUser) andalso (FromVHost == ToVHost) andalso (ToResource == <<>>))
            orelse ((ToUser == <<>>) andalso (ToVHost == <<>>)),
    Res = case Local of
        true ->
            try unsafe_set_vcard(HostType, From, VCARD) of
                ok ->
                    IQ#iq{type = result, sub_el = []};
                {error, {invalid_input, {Field, Value}}} ->
                    ?LOG_WARNING(#{what => vcard_sm_iq_set_failed, value => Value,
                                   reason => invalid_input, field => Field, acc => Acc}),
                    Text = io_lib:format("Invalid input for vcard field ~s", [Field]),
                    ReasonEl = mongoose_xmpp_errors:bad_request(<<"en">>, erlang:iolist_to_binary(Text)),
                    vcard_error(IQ, ReasonEl);
                {error, Reason} ->
                    ?LOG_WARNING(#{what => vcard_sm_iq_set_failed,
                                   reason => Reason, acc => Acc}),
                    vcard_error(IQ, mongoose_xmpp_errors:unexpected_request_cancel())
            catch
                E:R:Stack ->
                    ?LOG_ERROR(#{what => vcard_sm_iq_set_failed,
                                 class => E, reason => R, stacktrace => Stack, acc => Acc}),
                    vcard_error(IQ, mongoose_xmpp_errors:internal_server_error())
            end;
        _ ->
            ?LOG_WARNING(#{what => vcard_sm_iq_get_failed,
                           reason => not_allowed, acc => Acc}),
            vcard_error(IQ, mongoose_xmpp_errors:not_allowed())
    end,
    {Acc, Res}.

process_sm_iq_get(HostType, _From, To, Acc, IQ, SubEl) ->
    #jid{luser = LUser, lserver = LServer} = To,
    Res = try mod_vcard_backend:get_vcard(HostType, LUser, LServer) of
        {ok, VCARD} ->
            IQ#iq{type = result, sub_el = VCARD};
        {error, Reason} ->
            IQ#iq{type = error, sub_el = [SubEl, Reason]}
        catch E:R:Stack ->
            ?LOG_ERROR(#{what => vcard_sm_iq_get_failed,
                         class => E, reason => R, stacktrace => Stack, acc => Acc}),
            vcard_error(IQ, mongoose_xmpp_errors:internal_server_error())
    end,
    {Acc, Res}.

unsafe_set_vcard(HostType, From, VCARD) ->
    #jid{luser = FromUser, lserver = FromVHost} = From,
    case parse_vcard(FromUser, FromVHost, VCARD) of
        {ok, VcardSearch} ->
            mod_vcard_backend:set_vcard(HostType, FromUser, FromVHost, VCARD, VcardSearch);
        {error, Reason} ->
            {error, Reason}
    end.


-spec set_vcard(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: ok | {error, term()},
    Params :: map(),
    Extra :: gen_hook:extra().
set_vcard(ok, _Params, _Extra) ->
    ?LOG_DEBUG(#{what => hook_call_already_handled}),
    {ok, ok};
set_vcard({error, no_handler_defined}, #{user := From, vcard := VCARD}, #{host_type := HostType}) ->
    Result = try unsafe_set_vcard(HostType, From, VCARD) of
                 ok -> ok;
                 {error, Reason} ->
                     ?LOG_ERROR(#{what => unsafe_set_vcard_failed, reason => Reason}),
                     {error, Reason}
             catch
                 E:R:S -> ?LOG_ERROR(#{what => unsafe_set_vcard_failed, class => E,
                                     reason => R, stacktrace => S}),
                     {error, {E, R}}
             end,
    {ok, Result};
set_vcard({error, _} = Error, _Params, _Extra) ->
    {ok, Error}.

-spec remove_domain(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_domain_api:remove_domain_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    mod_vcard_backend:remove_domain(HostType, Domain),
    {ok, Acc}.

-spec remove_user(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: gen_hook:extra().
remove_user(Acc, #{jid := #jid{luser = User, lserver = Server}}, #{host_type := HostType}) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nodeprep(Server),
    mod_vcard_backend:remove_user(HostType, LUser, LServer),
    {ok, Acc}.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------
do_route(_HostType, _LServer, From,
         #jid{luser = LUser, lresource = LResource} = To, Acc, _IQ)
  when (LUser /= <<>>) or (LResource /= <<>>) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:service_unavailable()),
    ejabberd_router:route(To, From, Acc1, Err);
do_route(HostType, LServer, From, To, Acc,
         #iq{type = set, xmlns = ?NS_SEARCH, lang = Lang, sub_el = SubEl} = IQ) ->
    route_search_iq_set(HostType, LServer, From, To, Acc, Lang, SubEl, IQ);
do_route(HostType, LServer, From, To, Acc,
         #iq{type = get, xmlns = ?NS_SEARCH, lang = Lang} = IQ) ->
    Instr = search_instructions(Lang),
    Form = search_form(To, mod_vcard_backend:search_fields(HostType, LServer), Lang),
    ResIQ = make_search_form_result_iq(IQ, [Instr, Form]),
    ejabberd_router:route(To, From, Acc, jlib:iq_to_xml(ResIQ));
do_route(_HostType, _LServer, From, To, Acc,
         #iq{type = set, xmlns = ?NS_DISCO_INFO}) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:not_allowed()),
    ejabberd_router:route(To, From, Acc1, Err);
do_route(HostType, _LServer, From, To, Acc,
         #iq{type = get, xmlns = ?NS_DISCO_INFO, lang = Lang} = IQ) ->
    IdentityXML = mongoose_disco:identities_to_xml([identity(Lang)]),
    FeatureXML = mongoose_disco:features_to_xml(features()),
    InfoXML = mongoose_disco:get_info(HostType, ?MODULE, <<>>, <<>>),
    ResIQ = IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_INFO}],
                                   children = IdentityXML ++ FeatureXML ++ InfoXML}]},
    ejabberd_router:route(To, From, Acc, jlib:iq_to_xml(ResIQ));
do_route(_HostType, _LServer, From, To, Acc,
         #iq{type = set, xmlns = ?NS_DISCO_ITEMS}) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:not_allowed()),
    ejabberd_router:route(To, From, Acc1, Err);
do_route(_HostType, _LServer, From, To, Acc,
         #iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"query">>,
                               attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}]}]},
    ejabberd_router:route(To, From, Acc, jlib:iq_to_xml(ResIQ));
do_route(_HostType, _LServer, From, To, Acc,
         #iq{type = get, xmlns = ?NS_VCARD} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"vCard">>,
                               attrs = [{<<"xmlns">>, ?NS_VCARD}],
                               children = iq_get_vcard()}]},
    ejabberd_router:route(To, From, Acc, jlib:iq_to_xml(ResIQ));
do_route(_HostType, _LServer, From, To, Acc, _IQ) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:service_unavailable()),
    ejabberd_router:route(To, From, Acc1, Err).

make_search_form_result_iq(IQ, Elements) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"query">>,
                           attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                           children = Elements
                          }]}.

search_instructions(Lang) ->
    Text = translate:translate(Lang, <<"You need an x:data capable client to search">>),
    #xmlel{name = <<"instructions">>, attrs = [], children = [#xmlcdata{content = Text}]}.

search_form(JID, SearchFields, Lang) ->
    Title = <<(translate:translate(Lang, <<"Search users in ">>))/binary,
              (jid:to_binary(JID))/binary>>,
    Instructions = <<"Fill in fields to search for any matching Jabber User">>,
    Fields = lists:map(fun ({X, Y}) -> ?TLFIELD(<<"text-single">>, X, Y) end, SearchFields),
    mongoose_data_forms:form(#{title => Title, instructions => Instructions, fields => Fields}).

route_search_iq_set(HostType, LServer, From, To, Acc, Lang, SubEl, IQ) ->
    XDataEl = mongoose_data_forms:find_form(SubEl),
    RSMIn = jlib:rsm_decode(IQ),
    case XDataEl of
        undefined ->
            {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:bad_request()),
            ejabberd_router:route(To, From, Acc1, Err);
        _ ->
            case mongoose_data_forms:parse_form_fields(XDataEl) of
                #{type := <<"submit">>, kvs := KVs} ->
                    {SearchResult, RSMOutEls} = search_result(HostType, LServer, Lang, To, KVs, RSMIn),
                    ResIQ = make_search_result_iq(IQ, SearchResult, RSMOutEls),
                    ejabberd_router:route(To, From, Acc, jlib:iq_to_xml(ResIQ));
                _ ->
                    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:bad_request()),
                    ejabberd_router:route(To, From, Acc1, Err)
            end
    end.

make_search_result_iq(IQ, SearchResult, RSMOutEls) ->
    Form = mongoose_data_forms:form(SearchResult),
    IQ#iq{
        type = result,
        sub_el = [#xmlel{name = <<"query">>,
                         attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                         children = [Form | RSMOutEls]}
                 ]}.

iq_get_vcard() ->
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"MongooseIM/mod_vcard">>}]},
     #xmlel{name = <<"URL">>, children = [#xmlcdata{content = ?MONGOOSE_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content = [<<"MongooseIM vCard module">>,
                                             <<"\nCopyright (c) Erlang Solutions Ltd.">>]}]}].

features() ->
    [?NS_DISCO_INFO, ?NS_SEARCH, ?NS_VCARD].

identity(Lang) ->
    #{category => <<"directory">>,
      type => <<"user">>,
      name => translate:translate(Lang, <<"vCard User Search">>)}.

search_result(HostType, LServer, Lang, JID, Data, RSMIn) ->
    Title = translate:translate(Lang, <<"Search Results for ", (jid:to_binary(JID))/binary>>),
    ReportedFields = mod_vcard_backend:search_reported_fields(HostType, LServer, Lang),
    Results1 = mod_vcard_backend:search(HostType, LServer, maps:to_list(Data)),
    Results2 = lists:filtermap(
                 fun(Result) ->
                         case search_result_get_jid(Result) of
                             [ResultJID] ->
                                 {true, {ResultJID, Result}};
                             [] ->
                                 false
                         end
                 end,
                 Results1),
    %% mnesia does not guarantee sorting order
    Results3 = lists:sort(Results2),
    {Results4, RSMOutEls} = apply_rsm_to_search_results(Results3, RSMIn, none),
    Results5 = [Result || {_, Result} <- Results4],
    Form = #{type => <<"result">>, title => Title, reported => ReportedFields, items => Results5},
    {Form, RSMOutEls}.

%% No RSM input, create empty
apply_rsm_to_search_results(Results, none, RSMOut) ->
    apply_rsm_to_search_results(Results, #rsm_in{}, RSMOut);

%% Create RSM output
apply_rsm_to_search_results(Results, #rsm_in{} = RSMIn, none) ->
    RSMOut = #rsm_out{count = length(Results)},
    apply_rsm_to_search_results(Results, RSMIn, RSMOut);

%% Skip by <after>$id</after>
apply_rsm_to_search_results(Results1, #rsm_in{direction = aft,
                                              id = After} = RSMIn, RSMOut)
  when is_binary(After) ->
    Results2 = lists:dropwhile(
                 fun({JID, _Result}) ->
                         JID == After
                 end,
                 lists:dropwhile(
                   fun({JID, _Result}) ->
                           JID =/= After
                   end,
                   Results1
                  )),
    Index = length(Results1) - length(Results2),
    apply_rsm_to_search_results(
      Results2,
      RSMIn#rsm_in{direction = undefined, id = undefined},
      RSMOut#rsm_out{index = Index}
     );

%% Seek by <before>$id</before>
apply_rsm_to_search_results(Results1, #rsm_in{max = Max,
                                              direction = before,
                                              id = Before} = RSMIn, RSMOut)
  when is_binary(Before) ->
    Results2 = lists:takewhile(
                 fun({JID, _Result}) ->
                         JID =/= Before
                 end, Results1),
    if
        is_integer(Max) ->
            Index = max(0, length(Results2) - Max),
            Results3 = lists:nthtail(Index, Results2);
        true ->
            Index = 0,
            Results3 = Results2
    end,
    apply_rsm_to_search_results(
      Results3,
      RSMIn#rsm_in{direction = undefined, id = undefined},
      RSMOut#rsm_out{index = Index}
     );

%% Skip by page number <index>371</index>
apply_rsm_to_search_results(Results1,
                            #rsm_in{max = Max,
                                    index = Index} = RSMIn1,
                            RSMOut)
  when is_integer(Max), is_integer(Index) ->
    Results2 = lists:nthtail(min(Index, length(Results1)), Results1),
    RSMIn2 = RSMIn1#rsm_in{index = undefined},
    apply_rsm_to_search_results(
      Results2,
      RSMIn2,
      RSMOut#rsm_out{index = Index}
     );

%% Limit to <max>10</max> items
apply_rsm_to_search_results(Results1, #rsm_in{max = Max} = RSMIn, RSMOut)
  when is_integer(Max)  ->
    Results2 = lists:sublist(Results1, Max),
    apply_rsm_to_search_results(Results2,
                                RSMIn#rsm_in{max = undefined}, RSMOut);

%% Encode RSM output
apply_rsm_to_search_results([_ | _] = Results, _, #rsm_out{} = RSMOut1) ->
    {FirstJID, _} = hd(Results),
    {LastJID, _} = lists:last(Results),
    RSMOut2 = RSMOut1#rsm_out{first = FirstJID,
                              last = LastJID},
    {Results, jlib:rsm_encode(RSMOut2)};

apply_rsm_to_search_results([], _, #rsm_out{} = RSMOut1) ->
    %% clear `index' without `after'
    RSMOut2 = RSMOut1#rsm_out{index = undefined},
    {[], jlib:rsm_encode(RSMOut2)}.

search_result_get_jid(Fields) ->
    [JID || #{var := <<"jid">>, values := [JID]} <- Fields].

parse_vcard(LUser, VHost, VCARD) ->
    FN       = exml_query:path(VCARD, [{element, <<"FN">>}, cdata], <<>>),
    Family   = exml_query:path(VCARD, [{element, <<"N">>},
                                       {element, <<"FAMILY">>}, cdata], <<>>),
    Given    = exml_query:path(VCARD, [{element, <<"N">>},
                                       {element, <<"GIVEN">>}, cdata], <<>>),
    Middle   = exml_query:path(VCARD, [{element, <<"N">>},
                                       {element, <<"MIDDLE">>}, cdata], <<>>),
    Nickname = exml_query:path(VCARD, [{element, <<"NICKNAME">>}, cdata], <<>>),
    BDay     = exml_query:path(VCARD, [{element, <<"BDAY">>}, cdata], <<>>),
    CTRY     = exml_query:path(VCARD, [{element, <<"ADR">>},
                                       {element, <<"CTRY">>}, cdata], <<>>),
    Locality = exml_query:path(VCARD, [{element, <<"ADR">>},
                                       {element, <<"LOCALITY">>}, cdata], <<>>),
    EMail1   = exml_query:path(VCARD, [{element, <<"EMAIL">>},
                                       {element, <<"USERID">>}, cdata], <<>>),
    EMail2   = exml_query:path(VCARD, [{element, <<"EMAIL">>}, cdata], <<>>),
    OrgName  = exml_query:path(VCARD, [{element, <<"ORG">>},
                                       {element, <<"ORGNAME">>}, cdata], <<>>),
    OrgUnit  = exml_query:path(VCARD, [{element, <<"ORG">>},
                                       {element, <<"ORGUNIT">>}, cdata], <<>>),
    EMail = case EMail1 of
                <<"">> -> EMail2;
                _ -> EMail1
            end,
    try
        LFN       = prepare_index(<<"FN">>, FN),
        LFamily   = prepare_index(<<"FAMILY">>, Family),
        LGiven    = prepare_index(<<"GIVEN">>, Given),
        LMiddle   = prepare_index(<<"MIDDLE">>, Middle),
        LNickname = prepare_index_allow_emoji(<<"NICKNAME">>, Nickname),
        LBDay     = prepare_index(<<"BDAY">>, BDay),
        LCTRY     = prepare_index(<<"CTRY">>, CTRY),
        LLocality = prepare_index(<<"LOCALITY">>, Locality),
        LEMail    = prepare_index(<<"EMAIL">>, EMail),
        LOrgName  = prepare_index(<<"ORGNAME">>, OrgName),
        LOrgUnit  = prepare_index(<<"ORGUNIT">>, OrgUnit),

        US = {LUser, VHost},

        {ok, #vcard_search{us        = US,
                           user      = {LUser, VHost},
                           luser     = LUser,
                           fn        = FN,       lfn        = LFN,
                           family    = Family,   lfamily    = LFamily,
                           given     = Given,    lgiven     = LGiven,
                           middle    = Middle,   lmiddle    = LMiddle,
                           nickname  = Nickname, lnickname  = LNickname,
                           bday      = BDay,     lbday      = LBDay,
                           ctry      = CTRY,     lctry      = LCTRY,
                           locality  = Locality, llocality  = LLocality,
                           email     = EMail,    lemail     = LEMail,
                           orgname   = OrgName,  lorgname   = LOrgName,
                           orgunit   = OrgUnit,  lorgunit   = LOrgUnit
                          }}
    catch
        throw:{invalid_input, Info} ->
            {error, {invalid_input, Info}}
    end.

prepare_index(FieldName, Value) ->
    case jid:str_tolower(Value) of
        error ->
            throw({invalid_input, {FieldName, Value}});
        LValue ->
            LValue
    end.

prepare_index_allow_emoji(FieldName, Value) ->
    {ok, Re} = re:compile(<<"[^[:alnum:][:space:][:punct:]]">>, [unicode, ucp]),
    Sanitized = re:replace(Value, Re, <<"">>, [global]),
    prepare_index(FieldName, Sanitized).


-spec get_default_reported_fields(binary()) -> [mongoose_data_forms:field()].
get_default_reported_fields(Lang) ->
    [
     ?TLFIELD(<<"jid-single">>, <<"Jabber ID">>, <<"jid">>),
     ?TLFIELD(<<"text-single">>, <<"Full Name">>, <<"fn">>),
     ?TLFIELD(<<"text-single">>, <<"Name">>, <<"first">>),
     ?TLFIELD(<<"text-single">>, <<"Middle Name">>, <<"middle">>),
     ?TLFIELD(<<"text-single">>, <<"Family Name">>, <<"last">>),
     ?TLFIELD(<<"text-single">>, <<"Nickname">>, <<"nick">>),
     ?TLFIELD(<<"text-single">>, <<"Birthday">>, <<"bday">>),
     ?TLFIELD(<<"text-single">>, <<"Country">>, <<"ctry">>),
     ?TLFIELD(<<"text-single">>, <<"City">>, <<"locality">>),
     ?TLFIELD(<<"text-single">>, <<"Email">>, <<"email">>),
     ?TLFIELD(<<"text-single">>, <<"Organization Name">>, <<"orgname">>),
     ?TLFIELD(<<"text-single">>, <<"Organization Unit">>, <<"orgunit">>)
    ].

config_metrics(Host) ->
    mongoose_module_metrics:opts_for_module(Host, ?MODULE, [backend]).

vcard_error(IQ = #iq{sub_el = VCARD}, ReasonEl) ->
    IQ#iq{type = error, sub_el = [VCARD, ReasonEl]}.

directory_jid_to_server_host(#jid{lserver = DirHost}) ->
    case mongoose_domain_api:get_subdomain_info(DirHost) of
        {ok, #{parent_domain := ServerHost}} when is_binary(ServerHost) ->
            ServerHost;
        Other ->
            error({dir_jid_to_server_host_failed, DirHost, Other})
    end.
