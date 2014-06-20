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

-behaviour(gen_mod).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_vcard.hrl").

%% gen_mod handlers
-export([start/2,stop/1]).

%% gen_server handlers
-export([init/1,handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

%% Hook handlers
-export([process_local_iq/3,process_sm_iq/3,get_local_features/5,remove_user/2]).

-export([start_link/2]).

-export([config_change/4]).

-define(PROCNAME, ejabberd_mod_vcard).
-define(BACKEND, (mod_vcard_backend:backend())).

-record(state,{search           :: boolean(),
               host             :: binary(),
               directory_host   :: string()
              }).

%%--------------------------------------------------------------------
%% backend callbacks
%%--------------------------------------------------------------------
-callback init(Host, Opts) -> ok when
    Host :: binary(),
    Opts :: list().

-callback remove_user(LUser, LServer) -> ok when
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

-callback search(VHost, Data, Lang, DefaultReportedFields) ->
    Res :: term() when
    VHost :: binary(),
    Data :: term(),
    Lang :: binary(),
    DefaultReportedFields :: #xmlel{}.

-callback search_fields(VHost) ->
    Res :: list() when
    VHost :: binary().

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------
start(VHost, Opts) ->
    start_backend_module(Opts),
    Proc = gen_mod:get_module_proc(VHost,?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [VHost,Opts]},
                 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(VHost) ->
    Proc = gen_mod:get_module_proc(VHost,?PROCNAME),
    supervisor:terminate_child(ejabberd_sup,Proc),
    supervisor:delete_child(ejabberd_sup,Proc).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
start_link(VHost, Opts) ->
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [VHost, Opts],[]).

init([VHost, Opts]) ->
    process_flag(trap_exit, true),
    ?BACKEND:init(VHost, Opts),

    ejabberd_hooks:add(remove_user, VHost,
                       ?MODULE, remove_user, 50),
    ejabberd_hooks:add(disco_local_features, VHost,
                       ?MODULE, get_local_features,50),

    ejabberd_hooks:add(host_config_update, VHost,
                       ?MODULE, config_change, 50),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_sm, VHost, ?NS_VCARD,
                                  ?MODULE,process_sm_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, VHost, ?NS_VCARD,
                                  ?MODULE,process_local_iq, IQDisc),

    DirectoryHost = gen_mod:get_opt_host(VHost, Opts, "vjud.@HOST@"),
    Search = gen_mod:get_opt(search, Opts, true),

    case Search of
        true ->
            ejabberd_router:register_route(DirectoryHost);
        _ ->
            ok
    end,
    {ok,#state{host=VHost, search = Search, directory_host = DirectoryHost}}.

terminate(_Reason, State) ->
    VHost = State#state.host,
    case State#state.search of
        true ->
            ejabberd_router:unregister_route(State#state.directory_host);
        _ ->
            ok
    end,
    ejabberd_hooks:delete(remove_user, VHost, ?MODULE, remove_user, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, VHost, ?NS_VCARD),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, VHost, ?NS_VCARD),
    ejabberd_hooks:delete(host_config_update, VHost, ?MODULE, config_change, 50),
    ejabberd_hooks:delete(disco_local_features, VHost, ?MODULE, get_local_features, 50).

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(stop,_From,State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From,State) ->
    {reply, bad_request, State}.

handle_info({route, From, To, Packet},State) ->
    IQ = jlib:iq_query_info(Packet),
    case catch do_route(State#state.host, From, To, Packet, IQ) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p", [Reason]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info(_,State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Hook handlers
%%--------------------------------------------------------------------
process_local_iq(_From,_To,#iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
process_local_iq(_From,_To,#iq{type = get, lang = Lang} = IQ) ->
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"vCard">>, attrs = [{"xmlns", ?NS_VCARD}],
                           children = [#xmlel{name = <<"FN">>,
                                              children = [#xmlcdata{content = <<"ejabberd">>}]},
                                       #xmlel{name = <<"URL">>,
                                              children = [#xmlcdata{content = ?EJABBERD_URI}]},
                                       #xmlel{name = <<"DESC">>,
                                              children = [#xmlcdata{content = [translate:translate(Lang,<<"Erlang Jabber Server">>),
                                                                               <<"\nCopyright (c) 2002-2011 ProcessOne">>]}]},
                                       #xmlel{name = <<"BDAY">>,
                                              children = [#xmlcdata{content = <<"2002-11-16">>}]}
                                      ]}]}.
process_sm_iq(From, To, #iq{type = set, sub_el = VCARD} = IQ) ->
    #jid{user = FromUser, lserver = FromVHost} = From,
    #jid{user = ToUser, lserver = ToVHost, resource = ToResource} = To,
    case lists:member(FromVHost, ?MYHOSTS) of
        true when FromUser == ToUser,
                  FromVHost == ToVHost,
                  ToResource == <<>>;
                  ToUser == <<>>,
                  ToVHost == <<>> ->

            {ok, VcardSearch} = prepare_vcard_search_params(FromUser, FromVHost, VCARD),
            case catch ?BACKEND:set_vcard(FromUser, FromVHost,VCARD, VcardSearch) of
                ok ->
                    IQ#iq{type = result,
                          sub_el = []};
                {error, Reason} ->
                    IQ#iq{type = error,
                          sub_el = [VCARD, Reason]};
                Else ->
                    ?ERROR_MSG("~p",[Else]),
                    IQ#iq{type = error,
                          sub_el = [VCARD, ?ERR_INTERNAL_SERVER_ERROR]}
            end;
        _ ->
            IQ#iq{type = error,
                  sub_el = [VCARD, ?ERR_NOT_ALLOWED]}
    end;
process_sm_iq(_From, To, #iq{type = get, sub_el = SubEl} = IQ) ->
    #jid{luser = LUser, lserver = LServer} = To,
    case catch ?BACKEND:get_vcard(LUser, LServer) of
        {ok, VCARD} ->
            IQ#iq{type = result, sub_el = VCARD};
        {error, Reason} ->
            IQ#iq{type = error, sub_el = [SubEl,Reason]};
        Else ->
            ?ERROR_MSG("~p",[Else]),
            IQ#iq{type = error,
                  sub_el = [SubEl, ?ERR_INTERNAL_SERVER_ERROR]}
    end.

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

remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nodeprep(Server),
    ?BACKEND:remove_user(LUser,LServer).

%% react to "global" config change
config_change(Acc, Host, ldap, _NewConfig) ->
    case ?BACKEND of
        mod_vcard_ldap ->
            Mods = ejabberd_config:get_local_option({modules, Host}),
            Opts = proplists:get_value(?MODULE, Mods, []),
            gen_mod:stop_module(Host, ?MODULE),
            gen_mod:start_module(Host, ?MODULE, Opts);
        _ ->
            ok
    end,
    %ok = gen_server:call(Proc,{new_config, Host, Opts}),
    Acc;
config_change(Acc, _, _, _) ->
    Acc.
%% ------------------------------------------------------------------
%% Dynamic modules
%% ------------------------------------------------------------------
start_backend_module(Opts) ->
    Backend = gen_mod:get_opt(backend, Opts, mnesia),
    {Mod, Code} = dynamic_compile:from_string(mod_vcard_backend(Backend)),
    code:load_binary(Mod, "mod_vcard_backend.erl", Code).

-spec mod_vcard_backend(atom()) -> string().
mod_vcard_backend(Backend) when is_atom(Backend) ->
    lists:flatten(
      ["-module(mod_vcard_backend).
       -export([backend/0]).
       -spec backend() -> atom().
       backend() ->
       mod_vcard_",
       atom_to_list(Backend),
       ".\n"]).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------
do_route(_VHost, From, #jid{user = User,
                            resource =Resource} = To, Packet, _IQ)
  when (User /= <<"">>) or (Resource /= <<"">>) ->

    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err);
do_route(VHost, From, To, Packet, #iq{type = set,
                                      xmlns = ?NS_SEARCH,
                                      lang = Lang,
                                      sub_el = SubEl} = IQ) ->

    XDataEl = find_xdata_el(SubEl),
    case XDataEl of
        false ->
            Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
            ejabberd_router:route(To, From, Err);
        _ ->
            XData = jlib:parse_xdata_submit(XDataEl),
            case XData of
                invalid ->
                    Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
                    ejabberd_router:route(To, From, Err);
                _ ->
                    ResIQ = IQ#iq{
                              type = result,
                              sub_el = [#xmlel{name = <<"query">>,
                                               attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                                               children = [#xmlel{name = <<"x">>,
                                                               attrs = [{<<"xmlns">>, ?NS_XDATA},
                                                                        {<<"type">>, <<"result">>}],
                                                               children = search_result(Lang,To, VHost, XData)}]}]},
                    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ))
            end
    end;
do_route(VHost, From, To, _Packet, #iq{type = get,
                                        xmlns = ?NS_SEARCH,
                                        lang = Lang} = IQ) ->
    ResIQ = IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_SEARCH}],
                                   children = ?FORM(To,?BACKEND:search_fields(VHost),Lang)
                                  }]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, Packet, #iq{type = set,
                                       xmlns = ?NS_DISCO_INFO}) ->
    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
    ejabberd_router:route(To, From, Err);
do_route(VHost, From, To, _Packet, #iq{type = get,
                                       xmlns = ?NS_DISCO_INFO,
                                       lang = Lang} = IQ) ->
    Info = ejabberd_hooks:run_fold(disco_info, VHost, [], [VHost, ?MODULE, "", ""]),
    ResIQ = IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs =[{<<"xmlns">>,?NS_DISCO_INFO}],
                                   children = [#xmlel{name = <<"identity">>,
                                                      attrs = [{<<"category">>, <<"directory">>},
                                                               {<<"type">>, <<"user">>},
                                                               {<<"name">>,
                                                                translate:translate(Lang,<<"vCard User Search">>)}]},
                                               #xmlel{name = <<"feature">>,
                                                      attrs = [{<<"var">>, ?NS_DISCO_INFO}]},
                                               #xmlel{name = <<"feature">>,
                                                      attrs = [{<<"var">>, ?NS_SEARCH}]},
                                               #xmlel{name = <<"feature">>,
                                                      attrs = [{<<"var">>, ?NS_VCARD}]}
                                              ] ++ Info}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, Packet, #iq{type=set,
                                       xmlns = ?NS_DISCO_ITEMS}) ->
    Err = jlib:make_error_reply(Packet, ?ERR_NOT_ALLOWED),
    ejabberd_router:route(To, From, Err);
do_route(_VHost, From, To, _Packet, #iq{ type = get,
                                         xmlns = ?NS_DISCO_ITEMS} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"query">>,
                               attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}]}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, _Packet, #iq{ type = get,
                                         xmlns = ?NS_VCARD,
                                         lang = Lang} = IQ) ->
    ResIQ =
        IQ#iq{type = result,
              sub_el = [#xmlel{name = <<"vCard">>,
                               attrs = [{<<"xmlns">>, ?NS_VCARD}],
                               children = iq_get_vcard(Lang)}]},
    ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
do_route(_VHost, From, To, Packet, _IQ) ->
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err).

iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"ejabberd/mod_vcard">>}]},
     #xmlel{name = <<"URL">>, children = [#xmlcdata{content = ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content = [translate:translate(
                                               Lang,
                                               <<"ejabberd vCard module">>),
                                             <<"\nCopyright (c) 2003-2011 ProcessOne">>]}]}].
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

search_result(Lang, JID, VHost, Data) ->
    [#xmlel{name = <<"title">>,
            children = [#xmlcdata{content = [translate:translate(Lang, <<"Search Results for ">>),
                                             jlib:jid_to_binary(JID)]}]}
                                             | ?BACKEND:search(VHost, Data, Lang, get_default_reported_fields(Lang))].
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
                "" ->
                    EMail2;
                _ ->
                    EMail1
            end,

    LUser     = jlib:nodeprep(User),
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

    if
        (LUser     == error) or
        (LFN       == error) or
        (LFamily   == error) or
        (LGiven    == error) or
        (LMiddle   == error) or
        (LNickname == error) or
        (LBDay     == error) or
        (LCTRY     == error) or
        (LLocality == error) or
        (LEMail    == error) or
        (LOrgName  == error) or
        (LOrgUnit  == error) ->
            {error, badarg};
        true ->
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
                              }}
    end.

-spec get_default_reported_fields(binary()) -> #xmlel{}.
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
