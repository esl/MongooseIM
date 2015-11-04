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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_register).
-author('alexey@process-one.net').
-xep([{xep, 77}, {version, "2.4"}]).
-behaviour(gen_mod).

-export([start/2,
         stop/1,
         clean_opts/1,
         stream_feature_register/2,
         unauthenticated_iq_register/4,
         try_register/5,
         process_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_REGISTER,
                                  ?MODULE, process_iq, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_REGISTER,
                                  ?MODULE, process_iq, IQDisc),
    ejabberd_hooks:add(c2s_stream_features, Host,
                       ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:add(c2s_unauthenticated_iq, Host,
                       ?MODULE, unauthenticated_iq_register, 50),
    mnesia:create_table(mod_register_ip,
                        [{ram_copies, [node()]},
                         {local_content, true},
                         {attributes, [key, value]}]),
    mnesia:add_table_copy(mod_register_ip, node(), ram_copies),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(c2s_stream_features, Host,
                          ?MODULE, stream_feature_register, 50),
    ejabberd_hooks:delete(c2s_unauthenticated_iq, Host,
                          ?MODULE, unauthenticated_iq_register, 50),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_REGISTER),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_REGISTER).

clean_opts(Opts) ->
    lists:map(fun clean_opt/1, Opts).

clean_opt({registration_watchers, Watchers}) ->
    CleanWatchers = lists:map(fun ejabberd_binary:string_to_binary/1, Watchers),
    {registration_watchers, CleanWatchers};
clean_opt(Item) ->
    Item.

stream_feature_register(Acc, _Host) ->
    [#xmlel{name = <<"register">>,
            attrs = [{<<"xmlns">>, ?NS_FEATURE_IQREGISTER}]} | Acc].

unauthenticated_iq_register(_Acc,
                            Server, #iq{xmlns = ?NS_REGISTER} = IQ, IP) ->
    Address = case IP of
                  {A, _Port} -> A;
                  _ -> undefined
              end,
    ResIQ = process_unauthenticated_iq(no_JID,
                                       %% For the above: the client is
                                       %% not registered (no JID), at
                                       %% least not yet, so they can
                                       %% not be authenticated either.
                                       make_host_only_JID(Server),
                                       IQ,
                                       Address),
    set_sender(jlib:iq_to_xml(ResIQ), make_host_only_JID(Server));
unauthenticated_iq_register(Acc, _Server, _IQ, _IP) ->
    Acc.

%% Clients must register before being able to authenticate.
process_unauthenticated_iq(Sender, Receiver, #iq{type = set} = Stanza, IPAddr) ->
    process_iq_set(Sender, Receiver, Stanza, IPAddr);
process_unauthenticated_iq(Sender, Receiver, #iq{type = get} = Stanza, IPAddr) ->
    process_iq_get(Sender, Receiver, Stanza, IPAddr).

process_iq(From, To, #iq{type = set} = IQ) ->
    process_iq_set(From, To, IQ, jlib:jid_tolower(From));
process_iq(From, To, #iq{type = get} = IQ) ->
    process_iq_get(From, To, IQ, jlib:jid_tolower(From)).

process_iq_set(From, To, #iq{sub_el = Child} = IQ, Source) ->
    true = is_query_element(Child),
    handle_set(IQ, From, To, Source).

handle_set(Stanza, ClientJID, ServerJID, Source) ->
    #iq{sub_el = Query} = Stanza,
    case has_only_remove_child(Query) of
        true ->
            attempt_cancelation(ClientJID, ServerJID, Stanza);
        {false, more} ->
            error_response(Stanza, ?ERR_BAD_REQUEST);
        {false, absent} ->
            case has_username_and_password_children(Query) of
                true ->
                    Credentials = get_username_and_password_values(Query),
                    register_or_change_password(Credentials, ClientJID, ServerJID, Stanza, Source);
                false ->
                    ignore
            end
    end.

has_only_remove_child(#xmlel{children = C} = Q) when length(C) =:= 1 ->
    case exml_query:path(Q, [{element, <<"remove">>}], absent) of
        absent ->
            {false, absent};
        _ ->
            true
    end;
has_only_remove_child(#xmlel{children = C} = Q) when length(C) > 1 ->
    case exml_query:path(Q, [{element, <<"remove">>}], absent) of
        absent ->
            {false, absent};
        _ ->
            {false, more}
    end.

has_username_and_password_children(Q) ->
    {absent, absent} =/= get_username_and_password_elements(Q).

get_username_and_password_elements(Q) ->
    {exml_query:path(Q, [{element, <<"username">>}], absent),
     exml_query:path(Q, [{element, <<"password">>}], absent)}.

get_username_and_password_values(Q) ->
    {exml_query:path(Q, [{element, <<"username">>}, cdata]),
     exml_query:path(Q, [{element, <<"password">>}, cdata])}.

register_or_change_password(Credentials, ClientJID, #jid{lserver = ServerDomain}, IQ, IPAddr) ->
    {Username, Password} = Credentials,
    case inband_registration_and_cancelation_allowed(ServerDomain, ClientJID) of
        true ->
            #iq{sub_el = Children, lang = Lang} = IQ,
            try_register_or_set_password(Username, ServerDomain, Password, ClientJID, IQ, Children, IPAddr, Lang);
        false ->
            %% This is not described in XEP 0077.
            error_response(IQ, ?ERR_FORBIDDEN)
    end.

attempt_cancelation(ClientJID, #jid{lserver = ServerDomain}, #iq{id = ID, sub_el = Child} = IQ) ->
    #jid{user = Username, lserver = UserDomain, resource = Resource} = ClientJID,
    case inband_registration_and_cancelation_allowed(ServerDomain, ClientJID) of
        true ->
            %% The response must be sent *before* the
            %% XML stream is closed (the call to
            %% `ejabberd_auth:remove_user/2' does
            %% this): as it is, when canceling a
            %% registration, there is no way to deal
            %% with failure.
            ResIQ = #iq{type = result, xmlns = ?NS_REGISTER,
                        id = ID,
                        sub_el = [Child]},
            ejabberd_router:route(
              jlib:make_jid(<<>>, <<>>, <<>>),
              jlib:make_jid(Username, UserDomain, Resource),
              jlib:iq_to_xml(ResIQ)),
            ejabberd_auth:remove_user(Username, UserDomain),
            ignore;
        false ->
            error_response(IQ, ?ERR_NOT_ALLOWED)
    end.

inband_registration_and_cancelation_allowed(Server, JID) ->
    Rule = gen_mod:get_module_opt(Server, ?MODULE, access, none),
    allow =:= acl:match_rule(Server, Rule, JID).

process_iq_get(From, _To, #iq{lang = Lang, sub_el = Child} = IQ, _Source) ->
    true = is_query_element(Child),
    {_IsRegistered, UsernameSubels, QuerySubels} =
        case From of
            #jid{user = User, lserver = Server} ->
                case ejabberd_auth:is_user_exists(User, Server) of
                    true ->
                        {true, [#xmlcdata{content = User}],
                         [#xmlel{name = <<"registered">>}]};
                    false ->
                        {false, [#xmlcdata{content = User}], []}
                end;
            _ ->
                {false, [], []}
        end,
    TranslatedMsg = translate:translate(
                      Lang, <<"Choose a username and password to register with this server">>),
    IQ#iq{type = result,
          sub_el = [#xmlel{name = <<"query">>,
                           attrs = [{<<"xmlns">>, <<"jabber:iq:register">>}],
                           children = [#xmlel{name = <<"instructions">>,
                                              children = [#xmlcdata{content = TranslatedMsg}]},
                                       #xmlel{name = <<"username">>,
                                              children = UsernameSubels},
                                       #xmlel{name = <<"password">>}
                                       | QuerySubels]}]}.

try_register_or_set_password(User, Server, Password, From, IQ,
                             SubEl, Source, Lang) ->
    case From of
        #jid{user = User, lserver = Server} ->
            try_set_password(User, Server, Password, IQ, SubEl, Lang);
        _ ->
            case check_from(From, Server) of
                allow ->
                    case try_register(User, Server, Password,
                                      Source, Lang) of
                        ok ->
                            IQ#iq{type = result,
                                  sub_el = [SubEl]};
                        {error, Error} ->
                            error_response(IQ, [SubEl, Error])
                    end;
                deny ->
                    error_response(IQ, [SubEl, ?ERR_FORBIDDEN])
            end
    end.

%% @doc Try to change password and return IQ response
try_set_password(User, Server, Password, IQ, SubEl, Lang) ->
    case is_strong_password(Server, Password) of
        true ->
            case ejabberd_auth:set_password(User, Server, Password) of
                ok ->
                    IQ#iq{type = result, sub_el = [SubEl]};
                {error, empty_password} ->
                    error_response(IQ, [SubEl, ?ERR_BAD_REQUEST]);
                {error, not_allowed} ->
                    error_response(IQ, [SubEl, ?ERR_NOT_ALLOWED]);
                {error, invalid_jid} ->
                    error_response(IQ, [SubEl, ?ERR_ITEM_NOT_FOUND])
            end;
        false ->
            ErrText = <<"The password is too weak">>,
            error_response(IQ, [SubEl, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)])
    end.

try_register(User, Server, Password, SourceRaw, Lang) ->
    case jlib:is_nodename(User) of
        false ->
            {error, ?ERR_BAD_REQUEST};
        _ ->
            JID = jlib:make_jid(User, Server, <<>>),
            Access = gen_mod:get_module_opt(Server, ?MODULE, access, all),
            IPAccess = get_ip_access(Server),
            case {acl:match_rule(Server, Access, JID),
                  check_ip_access(SourceRaw, IPAccess)} of
                {deny, _} ->
                    {error, ?ERR_FORBIDDEN};
                {_, deny} ->
                    {error, ?ERR_FORBIDDEN};
                {allow, allow} ->
                    Source = may_remove_resource(SourceRaw),
                    case check_timeout(Source) of
                        true ->
                            case is_strong_password(Server, Password) of
                                true ->
                                    case ejabberd_auth:try_register(
                                           User, Server, Password) of
                                        ok ->
                                            send_welcome_message(JID),
                                            send_registration_notifications(JID, Source),
                                            ok;
                                        Error ->
                                            remove_timeout(Source),
                                            case Error of
                                                {error, exists} ->
                                                    {error, ?ERR_CONFLICT};
                                                {error, invalid_jid} ->
                                                    {error, ?ERR_JID_MALFORMED};
                                                {error, not_allowed} ->
                                                    {error, ?ERR_NOT_ALLOWED}
                                            end
                                    end;
                                false ->
                                    ErrText = <<"The password is too weak">>,
                                    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
                            end;
                        false ->
                            ErrText = <<"Users are not allowed to register accounts so quickly">>,
                            {error, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)}
                    end
            end
    end.


send_welcome_message(JID) ->
    Host = JID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, welcome_message, {"", ""}) of
        {"", ""} ->
            ok;
        {Subj, Body} ->
            ejabberd_router:route(
              jlib:make_jid(<<>>, Host, <<>>),
              JID,
              #xmlel{name = <<"message">>, attrs = [{<<"type">>, <<"normal">>}],
                     children = [#xmlel{name = <<"subject">>,
                                        children = [#xmlcdata{content = Subj}]},
                                 #xmlel{name = <<"body">>,
                                        children = [#xmlcdata{content = Body}]}]});
        _ ->
            ok
    end.

send_registration_notifications(UJID, Source) ->
    Host = UJID#jid.lserver,
    case gen_mod:get_module_opt(Host, ?MODULE, registration_watchers, []) of
        [] -> ok;
        JIDs when is_list(JIDs) ->
            Body = lists:flatten(
                     io_lib:format(
                       "[~s] The account ~s was registered from IP address ~s "
                       "on node ~w using ~p.",
                       [get_time_string(), jlib:jid_to_binary(UJID),
                        ip_to_string(Source), node(), ?MODULE])),
            lists:foreach(
              fun(S) ->
                      case jlib:binary_to_jid(S) of
                          error -> ok;
                          JID ->
                              ejabberd_router:route(
                                jlib:make_jid(<<>>, Host, <<>>),
                                JID,
                                #xmlel{name = <<"message">>,
                                       attrs = [{<<"type">>, <<"chat">>}],
                                       children = [#xmlel{name = <<"body">>,
                                                          children = [#xmlcdata{content = Body}]}]})
                      end
              end, JIDs);
        _ ->
            ok
    end.

check_from(no_JID, _Server) ->
    allow;
check_from(JID, Server) ->
    Access = gen_mod:get_module_opt(Server, ?MODULE, access_from, none),
    acl:match_rule(Server, Access, JID).

check_timeout(undefined) ->
    true;
check_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
                  undefined ->  600;
                  TO -> TO
              end,
    if
        is_integer(Timeout) ->
            {MSec, Sec, _USec} = now(),
            Priority = -(MSec * 1000000 + Sec),
            CleanPriority = Priority + Timeout,
            F = fun() ->
                        Treap = case mnesia:read(mod_register_ip, treap,
                                                 write) of
                                    [] ->
                                        treap:empty();
                                    [{mod_register_ip, treap, T}] -> T
                                end,
                        Treap1 = clean_treap(Treap, CleanPriority),
                        case treap:lookup(Source, Treap1) of
                            error ->
                                Treap2 = treap:insert(Source, Priority, [],
                                                      Treap1),
                                mnesia:write({mod_register_ip, treap, Treap2}),
                                true;
                            {ok, _, _} ->
                                mnesia:write({mod_register_ip, treap, Treap1}),
                                false
                        end
                end,

            case mnesia:transaction(F) of
                {atomic, Res} ->
                    Res;
                {aborted, Reason} ->
                    ?ERROR_MSG("mod_register: timeout check error: ~p~n",
                               [Reason]),
                    true
            end;
        true ->
            true
    end.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
        true ->
            Treap;
        false ->
            {_Key, Priority, _Value} = treap:get_root(Treap),
            if
                Priority > CleanPriority ->
                    clean_treap(treap:delete_root(Treap), CleanPriority);
                true ->
                    Treap
            end
    end.

remove_timeout(undefined) ->
    true;
remove_timeout(Source) ->
    Timeout = case ejabberd_config:get_local_option(registration_timeout) of
                  undefined -> 600;
                  TO -> TO
              end,
    if
        is_integer(Timeout) ->
            F = fun() ->
                        Treap = case mnesia:read(mod_register_ip, treap,
                                                 write) of
                                    [] ->
                                        treap:empty();
                                    [{mod_register_ip, treap, T}] -> T
                                end,
                        Treap1 = treap:delete(Source, Treap),
                        mnesia:write({mod_register_ip, treap, Treap1}),
                        ok
                end,
            case mnesia:transaction(F) of
                {atomic, ok} ->
                    ok;
                {aborted, Reason} ->
                    ?ERROR_MSG("mod_register: timeout remove error: ~p~n",
                               [Reason]),
                    ok
            end;
        true ->
            ok
    end.

ip_to_string(Source) when is_tuple(Source) -> inet_parse:ntoa(Source);
ip_to_string(undefined) -> "undefined";
ip_to_string(_) -> "unknown".

get_time_string() -> write_time(erlang:localtime()).
%% Function copied from ejabberd_logger_h.erl and customized
write_time({{Y,Mo,D},{H,Mi,S}}) ->
    io_lib:format("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",
                  [Y, Mo, D, H, Mi, S]).

is_strong_password(Server, Password) ->
    LServer = jid:nameprep(Server),
    case gen_mod:get_module_opt(LServer, ?MODULE, password_strength, 0) of
        Entropy when is_number(Entropy), Entropy >= 0 ->
            if Entropy == 0 ->
                    true;
               true ->
                    ejabberd_auth:entropy(Password) >= Entropy
            end;
        Wrong ->
            ?WARNING_MSG("Wrong value for password_strength option: ~p",
                         [Wrong]),
            true
    end.

%%%
%%% ip_access management
%%%

may_remove_resource({_, _, _} = From) ->
    jid:to_bare(From);
may_remove_resource(From) ->
    From.

get_ip_access(Host) ->
    IPAccess = gen_mod:get_module_opt(Host, ?MODULE, ip_access, []),
    lists:flatmap(
      fun({Access, S}) ->
              case parse_ip_netmask(S) of
                  {ok, IP, Mask} ->
                      [{Access, IP, Mask}];
                  error ->
                      ?ERROR_MSG("mod_register: invalid "
                                 "network specification: ~p",
                                 [S]),
                      []
              end
      end, IPAccess).

parse_ip_netmask(S) ->
    case string:tokens(S, "/") of
        [IPStr] ->
            case inet_parse:address(IPStr) of
                {ok, {_, _, _, _} = IP} ->
                    {ok, IP, 32};
                {ok, {_, _, _, _, _, _, _, _} = IP} ->
                    {ok, IP, 128};
                _ ->
                    error
            end;
        [IPStr, MaskStr] ->
            case catch list_to_integer(MaskStr) of
                Mask when is_integer(Mask),
                          Mask >= 0 ->
                    case inet_parse:address(IPStr) of
                        {ok, {_, _, _, _} = IP} when Mask =< 32 ->
                            {ok, IP, Mask};
                        {ok, {_, _, _, _, _, _, _, _} = IP} when Mask =< 128 ->
                            {ok, IP, Mask};
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.

check_ip_access(_Source, []) ->
    allow;
check_ip_access({User, Server, Resource}, IPAccess) ->
    case ejabberd_sm:get_session_ip(User, Server, Resource) of
        {IPAddress, _PortNumber} -> check_ip_access(IPAddress, IPAccess);
        _ -> true
    end;
check_ip_access({_, _, _, _} = IP,
                [{Access, {_, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (32 - Mask)) - 1),
    if
        IPInt band M =:= NetInt band M ->
            Access;
        true ->
            check_ip_access(IP, IPAccess)
    end;
check_ip_access({_, _, _, _, _, _, _, _} = IP,
                [{Access, {_, _, _, _, _, _, _, _} = Net, Mask} | IPAccess]) ->
    IPInt = ip_to_integer(IP),
    NetInt = ip_to_integer(Net),
    M = bnot ((1 bsl (128 - Mask)) - 1),
    if
        IPInt band M =:= NetInt band M ->
            Access;
        true ->
            check_ip_access(IP, IPAccess)
    end;
check_ip_access(IP, [_ | IPAccess]) ->
    check_ip_access(IP, IPAccess).

ip_to_integer({IP1, IP2, IP3, IP4}) ->
    (((((IP1 bsl 8) bor IP2) bsl 8) bor IP3) bsl 8) bor IP4;
ip_to_integer({IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8}) ->
    (((((((((((((IP1 bsl 16) bor IP2) bsl 16) bor IP3) bsl 16) bor IP4)
               bsl 16) bor IP5) bsl 16) bor IP6) bsl 16) bor IP7) bsl 16) bor IP8.

make_host_only_JID(Name) when is_binary(Name) ->
    jlib:make_jid(<<>>, Name, <<>>).

set_sender(#xmlel{attrs = A} = Stanza, #jid{} = Sender) ->
    Stanza#xmlel{attrs = [{<<"from">>, jlib:jid_to_binary(Sender)}|A]}.

is_query_element(#xmlel{name = <<"query">>}) ->
    true;
is_query_element(_) ->
    false.

error_response(Request, Reasons) when is_list(Reasons) ->
    Request#iq{type = error, sub_el = Reasons};
error_response(Request, Reason) ->
    Request#iq{type = error, sub_el = Reason}.

print_it(Text, Value) ->
    lager:error("***** ~s = ~p *****~n", [Text, Value]).
