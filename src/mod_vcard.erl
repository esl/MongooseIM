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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_vcard).
-author('alexey@process-one.net').
-xep([{xep, 54}, {version, "1.2"}]).
-xep([{xep, 55}, {version, "1.3"}]).
-behaviour(gen_mod).
-behaviour(gen_server).
-behaviour(gdpr).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").
-include("mongoose_rsm.hrl").

%% gen_mod handlers
-export([start/2, stop/1]).

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
-export([process_local_iq/4,
         process_sm_iq/4,
         get_local_features/5,
         remove_user/2,
         remove_user/3,
         set_vcard/3]).

-export([start_link/2]).
-export([default_search_fields/0]).
-export([get_results_limit/1]).
-export([get_default_reported_fields/1]).
-export([default_host/0]).

-export([config_change/4]).

%% GDPR related
-export([get_personal_data/2, remove_personal_data/2]).

-define(PROCNAME, ejabberd_mod_vcard).

-record(state, {search           :: boolean(),
               host             :: binary(),
               directory_host   :: binary()
              }).

-type error() :: error | {error, any()}.

-callback init(Host, Opts) -> ok when
    Host :: binary(),
    Opts :: list().

-callback remove_user(LUser, LServer) -> any() when
    LUser :: binary(),
    LServer :: binary().

-callback set_vcard(User, VHost, VCard, VCardSearch) ->
    ok | {error, Reason :: term()} when
    User :: binary(),
    VHost :: binary(),
    VCard :: term(),
    VCardSearch :: term().

-callback get_vcard(LUser, LServer) ->
    {ok, Vcard :: term()} | {error, Reason :: term()} when
    LUser :: binary(),
    LServer :: binary().

-callback search(VHost, Data) ->
    Res :: term() when
    VHost :: binary(),
    Data :: term().

-callback search_fields(VHost) ->
    Res :: list() when
    VHost :: binary().

-callback search_reported_fields(VHost, Lang) ->
    Res :: term() when
    VHost :: jid:lserver(),
    Lang :: binary().

-callback tear_down(jid:lserver()) -> ok.

-optional_callbacks([tear_down/1]).

%%--------------------------------------------------------------------
%% gdpr callback
%%--------------------------------------------------------------------

-spec get_personal_data(jid:user(), jid:server()) ->
    [{gdpr:data_group(), gdpr:schema(), gdpr:entries()}].
get_personal_data(Username, Server) ->
    LUser = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    Jid = jid:to_binary({LUser, LServer}),
    Schema = ["jid", "vcard"],
    Entries = lists:flatmap(fun(B) ->
                                    try B:get_vcard(LUser, LServer) of
                                        {ok, Record} ->
                                            SerializedRecords = exml:to_binary(Record),
                                            [{Jid, SerializedRecords}];
                                        _ -> []
                                    catch
                                        _:_ ->
                                            []
                                    end
                            end, mongoose_lib:find_behaviour_implementations(mod_vcard)),
    [{vcard, Schema, Entries}].

-spec remove_personal_data(jid:user(), jid:server()) -> ok.
remove_personal_data(Username, Server) ->
    LUser = jid:nodeprep(Username),
    LServer = jid:nameprep(Server),
    lists:foreach(fun(B) -> try_remove_personal_data(LUser, LServer, B) end, mongoose_lib:find_behaviour_implementations(mod_vcard)).

try_remove_personal_data(LUser, LServer, Module) ->
    try
        Module:remove_user(LUser, LServer)
    catch
        _:_ ->
            ok
    end.


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

-spec get_results_limit(jid:lserver()) -> non_neg_integer() | infinity.
get_results_limit(LServer) ->
    case gen_mod:get_module_opt(LServer, mod_vcard, matches, ?JUD_MATCHES) of
        infinity ->
            infinity;
        Val when is_integer(Val) and (Val > 0) ->
            Val;
        Val ->
            ?ERROR_MSG("Illegal option value ~p. "
            "Default value ~p substituted.",
                [{matches, Val}, ?JUD_MATCHES]),
            ?JUD_MATCHES
    end.

-spec default_host() -> binary().
default_host() ->
    <<"vjud.@HOST@">>.

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

start(VHost, Opts) ->
    gen_mod:start_backend_module(?MODULE, Opts, [set_vcard, get_vcard, search]),
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [VHost, Opts]},
                 transient, 1000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

stop(VHost) ->
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    try
      mod_vcard_backend:tear_down(VHost)
    catch
      error:undef ->
        %% This is expected for other backends than ldap
        ok
    end,

    gen_server:call(Proc, stop),
    ejabberd_sup:stop_child(Proc).

%%--------------------------------------------------------------------
%% mongoose_packet_handler callbacks
%%--------------------------------------------------------------------

-spec process_packet(Acc :: mongoose_acc:t(), From ::jid:jid(), To ::jid:jid(),
                     Packet :: exml:element(), Pid :: pid()) -> any().
process_packet(Acc, From, To, Packet, Pid) ->
    Pid ! {route, From, To, Acc, Packet}.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
start_link(VHost, Opts) ->
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [VHost, Opts], []).

init([VHost, Opts]) ->
    process_flag(trap_exit, true),
    mod_vcard_backend:init(VHost, Opts),
    [ ejabberd_hooks:add(Hook, VHost, M, F, Prio)
      || {Hook, M, F, Prio} <- hook_handlers() ],
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, VHost, ?NS_VCARD,
                                  ?MODULE, process_sm_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, VHost, ?NS_VCARD,
                                  ?MODULE, process_local_iq, IQDisc),
    DirectoryHost = gen_mod:get_opt_subhost(VHost, Opts, default_host()),
    Search = gen_mod:get_opt(search, Opts, true),
    case Search of
        true ->
            ejabberd_router:register_route(
              DirectoryHost, mongoose_packet_handler:new(?MODULE, self()));
        _ ->
            ok
    end,
    {ok, #state{host = VHost, search = Search, directory_host = DirectoryHost}}.

terminate(_Reason, State) ->
    VHost = State#state.host,
    case State#state.search of
        true ->
            ejabberd_router:unregister_route(State#state.directory_host);
        _ ->
            ok
    end,
    [ ejabberd_hooks:delete(Hook, VHost, M, F, Prio)
      || {Hook, M, F, Prio} <- hook_handlers() ],
    gen_iq_handler:remove_iq_handler(ejabberd_local, VHost, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, VHost, ?NS_VCARD).

hook_handlers() ->
    %% Hook, Module, Function, Priority
    [{remove_user,          ?MODULE, remove_user,        50},
     {anonymous_purge_hook, ?MODULE, remove_user,        50},
     {disco_local_features, ?MODULE, get_local_features, 50},
     {host_config_update,   ?MODULE, config_change,      50},
     {set_vcard,            ?MODULE, set_vcard,          50}].

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, bad_request, State}.

handle_info({route, From, To, Acc, _El}, State) ->
    {IQ, Acc1} = mongoose_iq:info(Acc),
    case catch do_route(State#state.host, From, To, Acc1, IQ) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p", [Reason]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Hook handlers
%%--------------------------------------------------------------------
process_local_iq(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq(_From, _To, Acc, #iq{type = get} = IQ) ->
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

process_sm_iq(From, To, Acc, #iq{type = set, sub_el = VCARD} = IQ) ->
    #jid{user = FromUser, lserver = FromVHost} = From,
    #jid{user = ToUser, lserver = ToVHost, resource = ToResource} = To,
    Res = case lists:member(FromVHost, ?MYHOSTS) of
        true when FromUser == ToUser,
                  FromVHost == ToVHost,
                  ToResource == <<>>;
                  ToUser == <<>>,
                  ToVHost == <<>> ->
            try unsafe_set_vcard(From, VCARD) of
                ok ->
                    IQ#iq{type = result,
                          sub_el = []};
                {error, Reason} ->
                    IQ#iq{type = error,
                          sub_el = [VCARD, Reason]}
            catch
                E:R ->
                    Stack = erlang:get_stacktrace(),
                    ?ERROR_MSG("issue=process_sm_iq_set_failed "
                                "reason=~p:~p "
                                "stacktrace=~1000p "
                                "from=~ts "
                                "to=~ts "
                                "sub_el=~ts",
                                [E, R, Stack,
                                 jid:to_binary(From),
                                 jid:to_binary(To),
                                 exml:to_binary(VCARD)]),
                    IQ#iq{type = error,
                          sub_el = [VCARD, mongoose_xmpp_errors:internal_server_error()]}
            end;
        _ ->
            IQ#iq{type = error,
                  sub_el = [VCARD, mongoose_xmpp_errors:not_allowed()]}
    end,
    {Acc, Res};
process_sm_iq(From, To, Acc, #iq{type = get, sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = To,
    Res = try mod_vcard_backend:get_vcard(LUser, LServer) of
        {ok, VCARD} ->
            IQ#iq{type = result, sub_el = VCARD};
        {error, Reason} ->
            IQ#iq{type = error, sub_el = [SubEl, Reason]}
        catch E:R ->
            Stack = erlang:get_stacktrace(),
            ?ERROR_MSG("issue=process_sm_iq_get_failed "
                        "reason=~p:~p "
                        "stacktrace=~1000p "
                        "from=~ts "
                        "to=~ts "
                        "sub_el=~ts",
                        [E, R, Stack,
                         jid:to_binary(From),
                         jid:to_binary(To),
                         exml:to_binary(SubEl)]),
            IQ#iq{type = error,
                  sub_el = [SubEl, mongoose_xmpp_errors:internal_server_error()]}
    end,
    {Acc, Res}.

unsafe_set_vcard(From, VCARD) ->
    #jid{user = FromUser, lserver = FromVHost} = From,
    {ok, VcardSearch} = prepare_vcard_search_params(FromUser, FromVHost, VCARD),
    mod_vcard_backend:set_vcard(FromUser, FromVHost, VCARD, VcardSearch).

-spec set_vcard(HandlerAcc, From, VCARD) -> Result when
      HandlerAcc :: ok | error(),
      From ::jid:jid(),
      VCARD :: exml:element(),
      Result :: ok | error().
set_vcard(ok, _From, _VCARD) ->
    ?DEBUG("hook call already handled - skipping", []),
    ok;
set_vcard({error, no_handler_defined}, From, VCARD) ->
    try unsafe_set_vcard(From, VCARD) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR_MSG("unsafe set_vcard failed: ~p", [Reason]),
            {error, Reason}
    catch
        E:R -> ?ERROR_MSG("unsafe set_vcard failed: ~p", [{E, R}]),
               {error, {E, R}}
    end;
set_vcard({error, _} = E, _From, _VCARD) -> E.

get_local_features({error, _Error}=Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_local_features(Acc, _From, _To, Node, _Lang) ->
    case Node of
        <<>> ->
            case Acc of
                {result, Features} ->
                    {result, [?NS_VCARD | Features]};
                empty ->
                    {result, [?NS_VCARD]}
            end;
        _ ->
            Acc
    end.

%% #rh
remove_user(Acc, User, Server) ->
    remove_user(User, Server),
    Acc.

remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nodeprep(Server),
    mod_vcard_backend:remove_user(LUser, LServer).

%% react to "global" config change
config_change(Acc, Host, ldap, _NewConfig) ->
    case mod_vcard_backend:backend() of
        mod_vcard_ldap ->
            Mods = ejabberd_config:get_local_option({modules, Host}),
            Opts = proplists:get_value(?MODULE, Mods, []),
            gen_mod:stop_module(Host, ?MODULE),
            gen_mod:start_module(Host, ?MODULE, Opts);
        _ ->
            ok
    end,
    %ok = gen_server:call(Proc, {new_config, Host, Opts}),
    Acc;
config_change(Acc, _, _, _) ->
    Acc.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------
do_route(_VHost, From, #jid{user = User,
                            resource =Resource} = To, Acc, _IQ)
  when (User /= <<"">>) or (Resource /= <<"">>) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:service_unavailable()),
    ejabberd_router:route(To, From, Acc1, Err);
do_route(VHost, From, To, Acc, #iq{type = set,
                                      xmlns = ?NS_SEARCH,
                                      lang = Lang,
                                      sub_el = SubEl} = IQ) ->

    XDataEl = find_xdata_el(SubEl),
    RSMIn = jlib:rsm_decode(IQ),
    case XDataEl of
        false ->
            {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:bad_request()),
            ejabberd_router:route(To, From, Acc1, Err);
        _ ->
            XData = jlib:parse_xdata_submit(XDataEl),
            case XData of
                invalid ->
                    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:bad_request()),
                    ejabberd_router:route(To, From, Acc1, Err);
                _ ->
                    {SearchResult, RSMOutEls} = search_result(Lang, To, VHost, XData, RSMIn),
                    ResIQ = IQ#iq{
                              type = result,
                              sub_el = [#xmlel{name = <<"query">>,
                                               attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                                               children = [#xmlel{name = <<"x">>,
                                                               attrs = [{<<"xmlns">>, ?NS_XDATA},
                                                                        {<<"type">>, <<"result">>}],
                                                               children = SearchResult}
                                                          ] ++ RSMOutEls}
                                       ]},
                    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
            end
    end;
do_route(VHost, From, To, _Acc, #iq{type = get,
                                        xmlns = ?NS_SEARCH,
                                        lang = Lang} = IQ) ->
    ResIQ =
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"query">>,
                           attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                           children = ?FORM(To, mod_vcard_backend:search_fields(VHost), Lang)
                          }]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, Acc, #iq{type = set,
                                       xmlns = ?NS_DISCO_INFO}) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:not_allowed()),
    ejabberd_router:route(To, From, Acc1, Err);
do_route(VHost, From, To, _Acc, #iq{type = get,
                                       xmlns = ?NS_DISCO_INFO,
                                       lang = Lang} = IQ) ->
    Info = ejabberd_hooks:run_fold(disco_info, VHost, [],
                                   [VHost, ?MODULE, <<"">>, <<"">>]),
    NameTxt = translate:translate(Lang, <<"vCard User Search">>),
    ResIQ = IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs =[{<<"xmlns">>, ?NS_DISCO_INFO}],
                                   children = [#xmlel{name = <<"identity">>,
                                                      attrs = [{<<"category">>, <<"directory">>},
                                                               {<<"type">>, <<"user">>},
                                                               {<<"name">>, NameTxt}]},
                                               #xmlel{name = <<"feature">>,
                                                      attrs = [{<<"var">>, ?NS_DISCO_INFO}]},
                                               #xmlel{name = <<"feature">>,
                                                      attrs = [{<<"var">>, ?NS_SEARCH}]},
                                               #xmlel{name = <<"feature">>,
                                                      attrs = [{<<"var">>, ?NS_VCARD}]}
                                              ] ++ Info}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, Acc, #iq{type=set,
                                       xmlns = ?NS_DISCO_ITEMS}) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:not_allowed()),
    ejabberd_router:route(To, From, Acc1, Err);
do_route(_VHost, From, To, _Acc, #iq{ type = get,
                                         xmlns = ?NS_DISCO_ITEMS} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"query">>,
                               attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}]}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, _Acc, #iq{ type = get,
                                         xmlns = ?NS_VCARD,
                                         lang = Lang} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"vCard">>,
                               attrs = [{<<"xmlns">>, ?NS_VCARD}],
                               children = iq_get_vcard(Lang)}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, Acc, _IQ) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, mongoose_xmpp_errors:service_unavailable()),
    ejabberd_router:route(To, From, Acc1, Err).

iq_get_vcard(_Lang) ->
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"MongooseIM/mod_vcard">>}]},
     #xmlel{name = <<"URL">>, children = [#xmlcdata{content = ?MONGOOSE_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content = [<<"MongooseIM vCard module">>,
                                             <<"\nCopyright (c) Erlang Solutions Ltd.">>]}]}].
find_xdata_el(#xmlel{children = SubEls}) ->
    find_xdata_el1(SubEls).

find_xdata_el1([]) ->
    false;
find_xdata_el1([XE = #xmlel{attrs = Attrs} | Els]) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_XDATA ->
            XE;
        _ ->
            find_xdata_el1(Els)
    end;
find_xdata_el1([_ | Els]) ->
    find_xdata_el1(Els).

search_result(Lang, JID, VHost, Data, RSMIn) ->
    Text = translate:translate(Lang, <<"Search Results for ">>),
    TitleEl = #xmlel{name = <<"title">>,
                     children = [#xmlcdata{content = [Text, jid:to_binary(JID)]}]},
    ReportedFields = mod_vcard_backend:search_reported_fields(VHost, Lang),
    Results1 = mod_vcard_backend:search(VHost, Data),
    Results2 = lists:filtermap(
                 fun(Result) ->
                         case search_result_get_jid(Result) of
                             {ok, ResultJID} ->
                                 {true, {ResultJID, Result}};
                             undefined ->
                                 false
                         end
                 end,
                 Results1),
    %% mnesia does not guarantee sorting order
    Results3 = lists:sort(Results2),
    {Results4, RSMOutEls} =
        apply_rsm_to_search_results(Results3, RSMIn, none),
    Results5 = [Result
                || {_, Result} <- Results4],

    {[TitleEl, ReportedFields
      | Results5],
     RSMOutEls}.

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

search_result_get_jid(#xmlel{name = <<"item">>,
                             children = Children}) ->
    Fields = jlib:parse_xdata_fields(Children),
    case lists:keysearch(<<"jid">>, 1, Fields) of
        {value, {<<"jid">>, JID}} ->
            {ok, list_to_binary(JID)};
        false ->
            undefined
    end.

b2l(Binary) ->
    binary_to_list(Binary).

prepare_vcard_search_params(User, VHost, VCARD) ->
    FN       = xml:get_path_s(VCARD, [{elem, <<"FN">>}, cdata]),
    Family   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"FAMILY">>}, cdata]),
    Given    = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"GIVEN">>}, cdata]),
    Middle   = xml:get_path_s(VCARD, [{elem, <<"N">>},
                                      {elem, <<"MIDDLE">>}, cdata]),
    Nickname = xml:get_path_s(VCARD, [{elem, <<"NICKNAME">>}, cdata]),
    BDay     = xml:get_path_s(VCARD, [{elem, <<"BDAY">>}, cdata]),
    CTRY     = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"CTRY">>}, cdata]),
    Locality = xml:get_path_s(VCARD, [{elem, <<"ADR">>},
                                      {elem, <<"LOCALITY">>}, cdata]),
    EMail1   = xml:get_path_s(VCARD, [{elem, <<"EMAIL">>},
                                      {elem, <<"USERID">>}, cdata]),
    EMail2   = xml:get_path_s(VCARD, [{elem, <<"EMAIL">>}, cdata]),
    OrgName  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGNAME">>}, cdata]),
    OrgUnit  = xml:get_path_s(VCARD, [{elem, <<"ORG">>},
                                      {elem, <<"ORGUNIT">>}, cdata]),
    EMail = case EMail1 of
                <<"">> -> EMail2;
                _ -> EMail1
            end,

    LUser     = jid:nodeprep(User),
    LFN       = stringprep:tolower(FN),
    LFamily   = stringprep:tolower(Family),
    LGiven    = stringprep:tolower(Given),
    LMiddle   = stringprep:tolower(Middle),
    LNickname = stringprep:tolower(Nickname),
    LBDay     = stringprep:tolower(BDay),
    LCTRY     = stringprep:tolower(CTRY),
    LLocality = stringprep:tolower(Locality),
    LEMail    = stringprep:tolower(EMail),
    LOrgName  = stringprep:tolower(OrgName),
    LOrgUnit  = stringprep:tolower(OrgUnit),

    US = {LUser, VHost},

    {ok, #vcard_search{us        = US,
                       user      = {User, VHost},
                       luser     = b2l(LUser),
                       fn        = FN,       lfn        = b2l(LFN),
                       family    = Family,   lfamily    = b2l(LFamily),
                       given     = Given,    lgiven     = b2l(LGiven),
                       middle    = Middle,   lmiddle    = b2l(LMiddle),
                       nickname  = Nickname, lnickname  = b2l(LNickname),
                       bday      = BDay,     lbday      = b2l(LBDay),
                       ctry      = CTRY,     lctry      = b2l(LCTRY),
                       locality  = Locality, llocality  = b2l(LLocality),
                       email     = EMail,    lemail     = b2l(LEMail),
                       orgname   = OrgName,  lorgname   = b2l(LOrgName),
                       orgunit   = OrgUnit,  lorgunit   = b2l(LOrgUnit)
                      }}.

-spec get_default_reported_fields(binary()) -> exml:element().
get_default_reported_fields(Lang) ->
    #xmlel{name = <<"reported">>,
           children = [
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
                      ]}.
