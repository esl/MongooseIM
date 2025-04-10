%%%----------------------------------------------------------------------
%%% File    : mod_register.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Inband registration support
%%% Created :  8 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_register).
-author('alexey@process-one.net').
-xep([{xep, 77}, {version, "2.4"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% Gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

%% IQ and hook handlers
-export([c2s_stream_features/3, process_iq/5]).
-export([user_send_xmlel/3]).

%% API
-export([try_register/6,
         process_ip_access/1,
         process_welcome_message/1]).

-ignore_xref([try_register/6]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-define(TABLE, mod_register_ip).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{iqdisc := IQDisc}) ->
    [gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_REGISTER, Component, Fn, #{}, IQDisc) ||
        {Component, Fn} <- iq_handlers()],
    Concurrency = case IQDisc of
                      one_queue -> [];
                      _ -> [{read_concurrency, true}]
                  end,
    ejabberd_sup:create_ets_table(?TABLE, [named_table, public | Concurrency]).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    [gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_REGISTER, Component) ||
        {Component, _Fn} <- iq_handlers()],
    ok.

iq_handlers() ->
    [{ejabberd_local, fun ?MODULE:process_iq/5},
     {ejabberd_sm, fun ?MODULE:process_iq/5}].

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{c2s_stream_features, HostType, fun ?MODULE:c2s_stream_features/3, #{}, 50}
     | c2s_hooks(HostType) ].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
c2s_hooks(HostType) ->
    [{user_send_xmlel, HostType, fun ?MODULE:user_send_xmlel/3, #{}, 30}].

%%%
%%% config_spec
%%%

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"iqdisc">> => mongoose_config_spec:iqdisc(),
                 <<"access">> => #option{type = atom,
                                         validate = access_rule},
                 <<"welcome_message">> => welcome_message_spec(),
                 <<"registration_watchers">> => #list{items = #option{type = binary,
                                                                      validate = jid}},
                 <<"password_strength">> => #option{type = integer,
                                                    validate = non_negative},
                 <<"ip_access">> => #list{items = ip_access_spec()}
                },
       defaults = #{<<"iqdisc">> => one_queue,
                    <<"access">> => all,
                    <<"registration_watchers">> => [],
                    <<"password_strength">> => 0,
                    <<"ip_access">> => []}
      }.

welcome_message_spec() ->
    #section{
        items = #{<<"body">> => #option{type = string},
                  <<"subject">> => #option{type = string}},
        defaults = #{<<"body">> => "",
                     <<"subject">> => ""},
        process = fun ?MODULE:process_welcome_message/1
    }.

ip_access_spec() ->
    #section{
        items = #{<<"address">> => #option{type = string,
                                           validate = ip_mask},
                  <<"policy">> => #option{type = atom,
                                          validate = {enum, [allow, deny]}}
                },
        required = all,
        process = fun ?MODULE:process_ip_access/1
    }.

supported_features() -> [dynamic_domains].

process_ip_access(#{policy := Policy, address := Address}) ->
    {Policy, Address}.

process_welcome_message(#{subject := Subject, body := Body}) ->
    {Subject, Body}.

%%%
%%% Hooks and IQ handlers
%%%

-spec c2s_stream_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [exml:element()],
    Params :: map(),
    Extra :: gen_hook:extra().
c2s_stream_features(Acc, _, _) ->
    NewAcc = [#xmlel{name = <<"register">>,
                     attrs = #{<<"xmlns">> => ?NS_FEATURE_IQREGISTER}} | Acc],
    {ok, NewAcc}.

-spec user_send_xmlel(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_xmlel(Acc, Params, Extra) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"iq">> ->
            {Iq, Acc1} = mongoose_iq:info(Acc),
            handle_unauthenticated_iq(Acc1, Params, Extra, Iq);
        _ -> {ok, Acc}
    end.

-spec handle_unauthenticated_iq(
        mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra(), atom() | jlib:iq()) ->
    mongoose_c2s_hooks:result().
handle_unauthenticated_iq(Acc,
                          #{c2s_data := StateData},
                          #{host_type := HostType},
                          #iq{xmlns = ?NS_REGISTER} = Iq) ->
    process_unauthenticated_iq(Acc, StateData, Iq, HostType);
handle_unauthenticated_iq(Acc, _, _, _) ->
    {ok, Acc}.

process_unauthenticated_iq(Acc, StateData, Iq, HostType) ->
    {Address, _} = mongoose_c2s:get_ip(StateData),
    LServer = mongoose_c2s:get_lserver(StateData),
    FromServer = jid:make_noprep(<<>>, LServer, <<>>),
    ResIQ = process_unauthenticated_iq(HostType,
                                       no_JID,
                                       %% For the above: the client is
                                       %% not registered (no JID), at
                                       %% least not yet, so they can
                                       %% not be authenticated either.
                                       FromServer,
                                       Iq,
                                       Address),
    Response = set_sender(jlib:iq_to_xml(ResIQ), FromServer),
    AccParams = #{from_jid => FromServer, to_jid => FromServer, element => Response},
    ResponseAcc = mongoose_acc:update_stanza(AccParams, Acc),
    {stop, mongoose_c2s_acc:to_acc(Acc, route, ResponseAcc)}.

%% Clients must register before being able to authenticate.
process_unauthenticated_iq(HostType, From, To, #iq{type = set} = IQ, IPAddr) ->
    process_iq_set(HostType, From, To, IQ, IPAddr);
process_unauthenticated_iq(HostType, From, To, #iq{type = get} = IQ, IPAddr) ->
    process_iq_get(HostType, From, To, IQ, IPAddr).

-spec process_iq(mongoose_acc:t(), jid:jid(), jid:jid(), jlib:iq(), map())
        -> {mongoose_acc:t(), jlib:iq()}.
process_iq(Acc, From, To, #iq{type = set} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    Res = process_iq_set(HostType, From, To, IQ, jid:to_lower(From)),
    {Acc, Res};
process_iq(Acc, From, To, #iq{type = get} = IQ, _Extra) ->
    HostType = mongoose_acc:host_type(Acc),
    Res = process_iq_get(HostType, From, To, IQ, jid:to_lower(From)),
    {Acc, Res}.

process_iq_set(HostType, From, To, #iq{sub_el = Child} = IQ, Source) ->
    true = is_query_element(Child),
    handle_set(HostType, IQ, From, To, Source).

handle_set(HostType, IQ, ClientJID, ServerJID, Source) ->
    #iq{sub_el = Query} = IQ,
    case which_child_elements(Query) of
        bad_request ->
            error_response(IQ, mongoose_xmpp_errors:bad_request());
        only_remove_child ->
            attempt_cancelation(HostType, ClientJID, ServerJID, IQ);
        various_elements_present ->
            case has_username_and_password_children(Query) of
                true ->
                    Credentials = get_username_and_password_values(Query),
                    register_or_change_password(HostType, Credentials, ClientJID, ServerJID, IQ, Source);
                false ->
                    error_response(IQ, mongoose_xmpp_errors:bad_request())
            end
    end.

which_child_elements(#xmlel{children = C} = Q) when length(C) =:= 1 ->
    case Q#xmlel.children of
        [#xmlel{name = <<"remove">>}] ->
            only_remove_child;
        [_] ->
            bad_request
    end;
which_child_elements(#xmlel{children = C} = Q) when length(C) > 1 ->
    case exml_query:subelement(Q, <<"remove">>) of
        #xmlel{name = <<"remove">>} ->
            bad_request;
        undefined ->
            various_elements_present
    end;
which_child_elements(#xmlel{children = []}) ->
    bad_request.

has_username_and_password_children(Q) ->
    (undefined =/= exml_query:path(Q, [{element, <<"username">>}]))
     and
    (undefined =/= exml_query:path(Q, [{element, <<"password">>}])).

get_username_and_password_values(Q) ->
    {exml_query:path(Q, [{element, <<"username">>}, cdata]),
     exml_query:path(Q, [{element, <<"password">>}, cdata])}.

register_or_change_password(HostType, Credentials, ClientJID, #jid{lserver = ServerDomain}, IQ, IPAddr) ->
    {Username, Password} = Credentials,
    case inband_registration_and_cancelation_allowed(HostType, ServerDomain, ClientJID) of
        true ->
            #iq{sub_el = Children, lang = Lang} = IQ,
            try_register_or_set_password(HostType, Username, ServerDomain, Password,
                                         ClientJID, IQ, Children, IPAddr, Lang);
        false ->
            %% This is not described in XEP 0077.
            error_response(IQ, mongoose_xmpp_errors:forbidden())
    end.

attempt_cancelation(HostType, #jid{} = ClientJID, #jid{lserver = ServerDomain}, #iq{} = IQ) ->
    case inband_registration_and_cancelation_allowed(HostType, ServerDomain, ClientJID) of
        true ->
            %% The response must be sent *before* the
            %% XML stream is closed (the call to
            %% `ejabberd_auth:remove_user/1' does
            %% this): as it is, when canceling a
            %% registration, there is no way to deal
            %% with failure.
            ResIQ = IQ#iq{type = result, sub_el = []},
            ejabberd_router:route(
              jid:make_noprep(<<>>, <<>>, <<>>),
              ClientJID,
              jlib:iq_to_xml(ResIQ)),
            ejabberd_auth:remove_user(ClientJID),
            ignore;
        false ->
            error_response(IQ, mongoose_xmpp_errors:not_allowed())
    end.

inband_registration_and_cancelation_allowed(_HostType, _ServerDomain, no_JID) ->
    true;
inband_registration_and_cancelation_allowed(HostType, ServerDomain, JID) ->
    Rule = gen_mod:get_module_opt(HostType, ?MODULE, access),
    allow =:= acl:match_rule(HostType, ServerDomain,  Rule, JID).

process_iq_get(_HostType, From, _To, #iq{lang = Lang, sub_el = Child} = IQ, _Source) ->
    true = is_query_element(Child),
    {_IsRegistered, UsernameSubels, QuerySubels} =
        case From of
            JID = #jid{luser = LUser} ->
                case ejabberd_auth:does_user_exist(JID) of
                    true ->
                        {true, [#xmlcdata{content = LUser}],
                         [#xmlel{name = <<"registered">>}]};
                    false ->
                        {false, [#xmlcdata{content = LUser}], []}
                end;
            _ ->
                {false, [], []}
        end,
    TranslatedMsg = service_translations:do(
                      Lang, <<"Choose a username and password to register with this server">>),
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"query">>,
                           attrs = #{<<"xmlns">> => <<"jabber:iq:register">>},
                           children = [#xmlel{name = <<"instructions">>,
                                              children = [#xmlcdata{content = TranslatedMsg}]},
                                       #xmlel{name = <<"username">>,
                                              children = UsernameSubels},
                                       #xmlel{name = <<"password">>}
                                       | QuerySubels]}]}.

try_register_or_set_password(HostType, LUser, Server, Password, #jid{luser = LUser, lserver = Server} = UserJID,
                             IQ, SubEl, _Source, Lang) ->
    try_set_password(HostType, UserJID, Password, IQ, SubEl, Lang);
try_register_or_set_password(HostType, LUser, Server, Password, _From, IQ, SubEl, Source, Lang) ->
    case check_timeout(Source) of
        true ->
            case try_register(HostType, LUser, Server, Password, Source, Lang) of
                ok ->
                    IQ#iq{type = result, sub_el = [SubEl]};
                {error, Error} ->
                    error_response(IQ, [SubEl, Error])
            end;
        false ->
            ErrText = <<"Users are not allowed to register accounts so quickly">>,
            error_response(IQ, mongoose_xmpp_errors:resource_constraint(Lang, ErrText))
    end.

%% @doc Try to change password and return IQ response
try_set_password(HostType, #jid{} = UserJID, Password, IQ, SubEl, Lang) ->
    case is_strong_password(HostType, Password) of
        true ->
            case ejabberd_auth:set_password(UserJID, Password) of
                ok ->
                    IQ#iq{type = result, sub_el = [SubEl]};
                {error, empty_password} ->
                    error_response(IQ, [SubEl, mongoose_xmpp_errors:bad_request()]);
                {error, not_allowed} ->
                    error_response(IQ, [SubEl, mongoose_xmpp_errors:not_allowed()]);
                {error, invalid_jid} ->
                    error_response(IQ, [SubEl, mongoose_xmpp_errors:item_not_found()])
            end;
        false ->
            ErrText = <<"The password is too weak">>,
            error_response(IQ, [SubEl, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)])
    end.

try_register(HostType, User, Server, Password, SourceRaw, Lang) ->
    case jid:is_nodename(User) of
        false ->
            {error, mongoose_xmpp_errors:bad_request()};
        _ ->
            JID = jid:make_bare(User, Server),
            Access = gen_mod:get_module_opt(HostType, ?MODULE, access),
            IPAccess = get_ip_access(HostType),
            case {acl:match_rule(HostType, Server, Access, JID),
                  check_ip_access(SourceRaw, IPAccess)} of
                {deny, _} ->
                    {error, mongoose_xmpp_errors:forbidden()};
                {_, deny} ->
                    {error, mongoose_xmpp_errors:forbidden()};
                {allow, allow} ->
                    verify_password_and_register(HostType, JID, Password, SourceRaw, Lang)
            end
    end.

verify_password_and_register(HostType, #jid{} = JID, Password, SourceRaw, Lang) ->
    case is_strong_password(HostType, Password) of
        true ->
            case ejabberd_auth:try_register(JID, Password) of
                {error, exists} ->
                    {error, mongoose_xmpp_errors:conflict()};
                {error, invalid_jid} ->
                    {error, mongoose_xmpp_errors:jid_malformed()};
                {error, not_allowed} ->
                    {error, mongoose_xmpp_errors:not_allowed()};
                {error, null_password} ->
                    {error, mongoose_xmpp_errors:not_acceptable()};
                _ ->
                    send_welcome_message(HostType, JID),
                    send_registration_notifications(HostType, JID, SourceRaw),
                    ok
            end;
        false ->
            ErrText = <<"The password is too weak">>,
            {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)}
    end.

send_welcome_message(HostType, #jid{lserver = Server} = JID) ->
    case gen_mod:lookup_module_opt(HostType, ?MODULE, welcome_message) of
        {error, not_found} ->
            ok;
        {ok, {Subj, Body}} ->
            ejabberd_router:route(
              jid:make_noprep(<<>>, Server, <<>>),
              JID,
              #xmlel{name = <<"message">>, attrs = #{<<"type">> => <<"normal">>},
                     children = [#xmlel{name = <<"subject">>,
                                        children = [#xmlcdata{content = Subj}]},
                                 #xmlel{name = <<"body">>,
                                        children = [#xmlcdata{content = Body}]}]})
    end.

send_registration_notifications(HostType, #jid{lserver = Domain} = UJID, Source) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, registration_watchers) of
        [] -> ok;
        JIDs when is_list(JIDs) ->
            Body = lists:flatten(
                     io_lib:format(
                       "[~s] The account ~s was registered from IP address ~s "
                       "on node ~w using ~p.",
                       [get_time_string(), jid:to_binary(UJID),
                        ip_to_string(Source), node(), ?MODULE])),
            lists:foreach(fun(S) -> send_registration_notification(S, Domain, Body) end, JIDs);
        _ ->
            ok
    end.

send_registration_notification(JIDBin, Domain, Body) ->
    case jid:from_binary(JIDBin) of
        error -> ok;
        JID ->
            Message = #xmlel{name = <<"message">>,
                             attrs = #{<<"type">> => <<"chat">>},
                             children = [#xmlel{name = <<"body">>,
                                                children = [#xmlcdata{content = Body}]}]},
            ejabberd_router:route(jid:make_noprep(<<>>, Domain, <<>>), JID, Message)
    end.

check_timeout(Source) ->
    Timeout = mongoose_config:get_opt(registration_timeout),
    case is_integer(Timeout) of
        true ->
            TS = erlang:system_time(second),
            clean_ets(TS - Timeout),
            check_and_store_ip_entry(Source, TS);
        false ->
            true
    end.

check_and_store_ip_entry(Source, Timestamp) ->
    case ets:member(?TABLE, Source) of
        false ->
            ets:insert(?TABLE, {Source, Timestamp}),
            true;
        true ->
            false
    end.

clean_ets(CleanTimestamp) ->
    ets:select_delete(?TABLE, [{ {'_', '$1'}, [{'<', '$1', CleanTimestamp}], [true]}]).

ip_to_string(Source) when is_tuple(Source) -> inet_parse:ntoa(Source);
ip_to_string(undefined) -> "undefined";
ip_to_string(_) -> "unknown".

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized
write_time({{Y, Mo, D}, {H, Mi, S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                  [Y, Mo, D, H, Mi, S]).

is_strong_password(HostType, Password) ->
    case gen_mod:get_module_opt(HostType, ?MODULE, password_strength) of
        Entropy when is_number(Entropy), Entropy == 0 ->
            true;
        Entropy when is_number(Entropy), Entropy > 0 ->
            ejabberd_auth:entropy(Password) >= Entropy;
        Wrong ->
            ?LOG_WARNING(#{what => reg_wrong_password_strength,
                           host => HostType, value => Wrong}),
            true
    end.

%%%
%%% ip_access management
%%%

get_ip_access(HostType) ->
    IPAccess = gen_mod:get_module_opt(HostType, ?MODULE, ip_access),
    lists:flatmap(
      fun({Access, {IP, Mask}}) ->
              [{Access, IP, Mask}];
         ({Access, S}) ->
              case mongoose_lib:parse_ip_netmask(S) of
                  {ok, {IP, Mask}} ->
                      [{Access, IP, Mask}];
                  error ->
                      ?LOG_ERROR(#{what => reg_invalid_network_specification,
                                   specification => S}),
                      []
              end
      end, IPAccess).

check_ip_access(_Source, []) ->
    allow;
check_ip_access({User, Server, Resource}, IPAccess) ->
    case ejabberd_sm:get_session_ip(jid:make(User, Server, Resource)) of
        {IPAddress, _PortNumber} -> check_ip_access(IPAddress, IPAccess);
        _ -> true
    end;
check_ip_access({_, _, _, _} = IP,
                [{Access, {_, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (32 - Mask)) - 1),
    case IPInt band M =:= NetInt band M of
        true -> Access;
        false -> check_ip_access(IP, IPAccess)
    end;
check_ip_access({_, _, _, _, _, _, _, _} = IP,
                [{Access, {_, _, _, _, _, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (128 - Mask)) - 1),
    case IPInt band M =:= NetInt band M of
        true -> Access;
        false -> check_ip_access(IP, IPAccess)
    end;
check_ip_access(IP, [_ | IPAccess]) ->
    check_ip_access(IP, IPAccess).

ip_to_integer({IP1, IP2, IP3, IP4}) ->
    <<X:32>> = <<IP1, IP2, IP3, IP4>>,
    X;
ip_to_integer({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}) ->
    <<X:64>> = <<IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8>>,
    X.

set_sender(#xmlel{attrs = A} = Stanza, #jid{} = From) ->
    Stanza#xmlel{attrs = A#{<<"from">> => jid:to_binary(From)}}.

is_query_element(#xmlel{name = <<"query">>}) ->
    true;
is_query_element(_) ->
    false.

error_response(Request, Reasons) when is_list(Reasons) ->
    Request#iq{type = error, sub_el = Reasons};
error_response(Request, Reason) ->
    Request#iq{type = error, sub_el = Reason}.
