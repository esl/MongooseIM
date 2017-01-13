%%%----------------------------------------------------------------------
%%% File    : mod_disco.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Service Discovery (XEP-0030) support
%%% Created :  1 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
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

-module(mod_disco).
-author('alexey@process-one.net').
-xep([{xep, 30}, {version, "2.4"}]).
-xep([{xep, 157}, {version, "1.0"}]).
-behaviour(gen_mod).

-export([start/2,
         stop/1,
         process_local_iq_items/3,
         process_local_iq_info/3,
         get_local_identity/5,
         get_local_features/5,
         get_local_services/5,
         process_sm_iq_items/3,
         process_sm_iq_info/3,
         get_sm_identity/5,
         get_sm_features/5,
         get_sm_items/5,
         get_info/5,
         register_feature/2,
         unregister_feature/2,
         register_extra_domain/2,
         unregister_extra_domain/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-type feature() :: any().

-spec start(ejabberd:server(), list()) -> 'ok'.
start(Host, Opts) ->
    ejabberd_local:refresh_iq_handlers(),

    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS,
                                  ?MODULE, process_local_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO,
                                  ?MODULE, process_local_iq_info, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS,
                                  ?MODULE, process_sm_iq_items, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO,
                                  ?MODULE, process_sm_iq_info, IQDisc),

    catch ets:new(disco_features, [named_table, ordered_set, public]),
    register_feature(Host, <<"iq">>),
    register_feature(Host, <<"presence">>),
    register_feature(Host, <<"presence-invisible">>),

    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts, []),
    lists:foreach(fun(Domain) -> register_extra_domain(Host, Domain) end,
                  ExtraDomains),
    catch ets:new(disco_sm_features, [named_table, ordered_set, public]),
    catch ets:new(disco_sm_nodes, [named_table, ordered_set, public]),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_local_services, 100),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_local_features, 100),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:add(disco_info, Host, ?MODULE, get_info, 100),
    ok.


-spec stop(ejabberd:server()) -> ok.
stop(Host) ->
    ejabberd_hooks:delete(disco_sm_identity, Host, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:delete(disco_sm_features, Host, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:delete(disco_sm_items, Host, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, get_local_identity, 100),
    ejabberd_hooks:delete(disco_local_features, Host, ?MODULE, get_local_features, 100),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_local_services, 100),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE, get_info, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_DISCO_INFO),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_ITEMS),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_DISCO_INFO),
    catch ets:match_delete(disco_features, {{'_', Host}}),
    catch ets:match_delete(disco_extra_domains, {{'_', Host}}),
    ok.


-spec register_feature(ejabberd:server(), feature()) -> 'true'.
register_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {{Feature, Host}}).


-spec unregister_feature(ejabberd:server(), feature()) -> 'true'.
unregister_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:delete(disco_features, {Feature, Host}).


-spec register_extra_domain(ejabberd:server(), binary()) -> 'true'.
register_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:insert(disco_extra_domains, {{Domain, Host}}).


-spec unregister_extra_domain(ejabberd:server(), binary()) -> 'true'.
unregister_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:delete(disco_extra_domains, {Domain, Host}).


-spec process_local_iq_items(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
            -> ejabberd:iq().
process_local_iq_items(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
process_local_iq_items(From, To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    Host = To#jid.lserver,

    Acc = mongoose_stanza:new(),
    Acc2 = ejabberd_hooks:run_fold(disco_local_items,
                                 Host,
                                 Acc,
                                 [From, To, Node, Lang]),
    case Acc2 of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        _ ->
            Items = mongoose_stanza:get(local_items, Acc2, []),
            ANode = case Node of
                        <<>> -> [];
                        _ -> [{<<"node">>, Node}]
            end,
            IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS} | ANode],
                                   children = Items}]}
    end.


-spec process_local_iq_info(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
            -> ejabberd:iq().
process_local_iq_info(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
process_local_iq_info(From, To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    Host = To#jid.lserver,
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    F = fun(K, A) -> mongoose_stanza:put(K, [], A) end,
    A1 = lists:foldl(F, mongoose_stanza:new(), [local_identity, info, features]),
    A2 = ejabberd_hooks:run_fold(disco_local_identity,
                                       Host,
                                       A1,
                                       [From, To, Node, Lang]),
    A3 = ejabberd_hooks:run_fold(disco_info, Host, A2,
                                   [Host, ?MODULE, Node, Lang]),
    Res = ejabberd_hooks:run_fold(disco_local_features,
                                 Host,
                                 A3,
                                 [From, To, Node, Lang]),
    case mongoose_stanza:to_map(Res) of
        #{features := Features, info := Info, local_identity := Identity} ->
            ANode = case Node of
                        <<>> -> [];
                        _ -> [{<<"node">>, Node}]
                    end,
            IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_INFO} | ANode],
                                   children = Identity ++
                                              Info ++
                                              features_to_xml(Features)}]};
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]}
    end.


-spec get_local_identity(Acc :: mongoose_stanza:t(),
                        From :: ejabberd:jid(),
                        To :: ejabberd:jid(),
                        Node :: binary(),
                        Lang :: ejabberd:lang()) -> mongoose_stanza:t().
get_local_identity(Acc, _From, _To, <<>>, _Lang) ->
    NIds = [#xmlel{name = <<"identity">>,
                   attrs = [{<<"category">>, <<"server">>},
                            {<<"type">>, <<"im">>},
                            {<<"name">>, <<"MongooseIM">>}]}],
    mongoose_stanza:append(local_identity, NIds, Acc);
get_local_identity(Acc, _From, _To, Node, _Lang) when is_binary(Node) ->
    Acc.


-spec get_local_features(Acc :: mongoose_stanza:t(),
                        From :: ejabberd:jid(),
                        To :: ejabberd:jid(),
                        Node :: binary(),
                        Lang :: ejabberd:lang()) -> mongoose_stanza:t().
get_local_features(Acc, _From, To, <<>>, _Lang) ->
    Feats = mongoose_stanza:get(features, Acc, []),
    Host = To#jid.lserver,
    NFeats = ets:select(disco_features, [{{{'_', Host}}, [], ['$_']}]) ++ Feats,
    mongoose_stanza:put(features, NFeats, Acc);

get_local_features(Acc, _From, _To, Node, _Lang) when is_binary(Node) ->
    #{features := F} = Acc,
    case F of
        [] ->
            {error, ?ERR_ITEM_NOT_FOUND};
        [_|_] ->
            Acc
    end.


-spec features_to_xml(FeatureList :: [{feature(), ejabberd:server()}]
                     ) -> [jlib:xmlel()].
features_to_xml(FeatureList) ->
    %% Avoid duplicating features
    [#xmlel{name = <<"feature">>, attrs = [{<<"var">>, Feat}]} ||
                  Feat <- lists:usort(
                            lists:map(
                              fun({{Feature, _Host}}) ->
                                  Feature;
                                 (Feature) when is_binary(Feature) ->
                          Feature
                              end, FeatureList))].


-spec domain_to_xml(binary() | {binary()}) -> jlib:xmlel().
domain_to_xml({Domain}) ->
    #xmlel{name = <<"item">>, attrs = [{<<"jid">>, Domain}]};
domain_to_xml(Domain) ->
    #xmlel{name = <<"item">>, attrs = [{<<"jid">>, Domain}]}.


-spec get_local_services(Acc :: mongoose_stanza:t(),
                         From :: ejabberd:jid(),
                         To :: ejabberd:jid(),
                         Node :: binary(),
                         Lang :: ejabberd:lang()) -> mongoose_stanza:t().
get_local_services(Acc, _From, To, <<>>, _Lang) ->
     Host = To#jid.lserver,
     NItems = lists:usort(
       lists:map(fun domain_to_xml/1,
                 get_vh_services(Host) ++
                 ets:select(disco_extra_domains,
                            [{{{'$1', Host}}, [], ['$1']}]))
       ),
     mongoose_stanza:append(local_items, NItems, Acc);
get_local_services({result, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_local_services(empty, _From, _To, _Node, _Lang) ->
    {error, ?ERR_ITEM_NOT_FOUND}.


-type route() :: any().
-spec get_vh_services(ejabberd:server()) -> [route()].
get_vh_services(Host) ->
    Hosts = lists:sort(fun(H1, H2) -> size(H1) >= size(H2) end, ?MYHOSTS),
    lists:filter(fun(H) ->
                         case lists:dropwhile(
                                fun(VH) ->
                                        not lists:suffix("." ++ binary_to_list(VH),
                                            binary_to_list(H))
                                end, Hosts) of
                             [] ->
                                 false;
                             [VH | _] ->
                                 VH == Host
                         end
                 end, ejabberd_router:dirty_get_all_routes()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_sm_iq_items(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
            -> ejabberd:iq().
process_sm_iq_items(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
        IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
process_sm_iq_items(From, To, #iq{type = get} = IQ) ->
    IsSubscribed = is_presence_subscribed(From, To),
    process_sm_iq_items(IsSubscribed, From, To, IQ).

process_sm_iq_items(true, From, To, #iq{lang = Lang, sub_el = SubEl} = IQ) ->
    Host = To#jid.lserver,
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    Acc = mongoose_stanza:new(),
    case ejabberd_hooks:run_fold(disco_sm_items,
        Host,
        Acc,
        [From, To, Node, Lang]) of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        Acc1 ->
            Items = mongoose_stanza:get(sm_items, Acc1, []),
            ANode = case Node of
                        <<>> -> [];
                        _ -> [{<<"node">>, Node}]
                    end,
            IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"query">>,
                    attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS} | ANode],
                    children = Items}]}
    end;
process_sm_iq_items(false, _From, _To, #iq{sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]}.

-spec get_sm_items(Acc :: mongoose_stanza:t(),
                   From :: ejabberd:jid(),
                   To :: ejabberd:jid(),
                   Node :: binary(),
                   Lang :: ejabberd:lang()) -> mongoose_stanza:t().
get_sm_items({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_sm_items(Acc, From,
            #jid{user = User, server = Server} = To,
            [], _Lang) ->
    Items = mongoose_stanza:get(sm_items, Acc, []),
    Items1 = case Items of
        [] ->
            get_sm_items(From, To);
        _ ->
            case is_presence_subscribed(From, To) of
                true ->
                    get_user_resources(User, Server);
                _ ->
                    []
            end
    end,
    mongoose_stanza:append(sm_items, Items1, Acc).
get_sm_items(From, To) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
        {LTo, LSTo} ->
            {error, ?ERR_ITEM_NOT_FOUND};
        _ ->
            {error, ?ERR_NOT_ALLOWED}
    end.


-spec is_presence_subscribed(ejabberd:jid(), ejabberd:jid()) -> boolean().
is_presence_subscribed(#jid{luser=User, lserver=Server}, #jid{luser=LUser, lserver=LServer}) ->
    lists:any(fun({roster, _, _, {TUser, TServer, _}, _, S, _, _, _, _}) ->
                            if
                                LUser == TUser, LServer == TServer, S/=none ->
                                    true;
                                true ->
                                    false
                            end
                    end,
                    ejabberd_hooks:run_fold(roster_get, Server, [], [{User, Server}]))
                orelse User == LUser andalso Server == LServer.


-spec process_sm_iq_info(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
            -> ejabberd:iq().
process_sm_iq_info(_From, _To, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
process_sm_iq_info(From, To, #iq{type = get} = IQ) ->
    IsSubscribed = is_presence_subscribed(From, To),
    process_sm_iq_info(IsSubscribed, From, To, IQ).

process_sm_iq_info(true, From, To, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    Host = To#jid.lserver,
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    Acc = mongoose_stanza:new(),
    Acc1 = ejabberd_hooks:run_fold(disco_sm_identity,
                                       Host,
                                       Acc,
                                       [From, To, Node, Lang]),
    Acc2 = ejabberd_hooks:run_fold(disco_sm_features,
                                 Host,
                                 Acc1,
                                 [From, To, Node, Lang]),
    case Acc2 of
        {error, Error} ->
            IQ#iq{type = error, sub_el = [SubEl, Error]};
        _ ->
            Features = mongoose_stanza:get(sm_features, Acc2, []),
            Identity = mongoose_stanza:get(sm_identity, Acc2, []),
            ANode = case Node of
                        <<>> -> [];
                        _ -> [{<<"node">>, Node}]
                    end,
            IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_INFO} | ANode],
                                   children = Identity ++
                                              features_to_xml(Features)}]}
    end;
process_sm_iq_info(false, _From, _To, #iq{type = get, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_SERVICE_UNAVAILABLE]}.


-spec get_sm_identity(Acc :: mongoose_stanza:t(),
                      From :: ejabberd:jid(),
                      To :: ejabberd:jid(),
                      Node :: binary(),
                      Lang :: ejabberd:lang()) -> mongoose_stanza:t().
get_sm_identity(Acc, _From, #jid{luser = LUser, lserver=LServer}, _Node, _Lang) ->
    Ids = mongoose_stanza:get(sm_identity, Acc, []),
    Id = case ejabberd_auth:is_user_exists(LUser, LServer) of
            true ->
               [#xmlel{name = <<"identity">>, attrs = [{<<"category">>, <<"account">>},
                      {<<"type">>, <<"registered">>}]}
               ];
            _ ->
               []
         end,
    mongoose_stanza:put(sm_identity, Ids ++ Id, Acc).


-spec get_sm_features(Acc :: mongoose_stanza:t(),
                      From :: ejabberd:jid(),
                      To :: ejabberd:jid(),
                      Node :: binary(),
                      Lang :: ejabberd:lang()) -> mongoose_stanza:t().
get_sm_features(Acc, From, To, _Node, _Lang) ->
    case mongoose_stanza:get(sm_features, Acc, []) of
        [] ->
            #jid{luser = LFrom, lserver = LSFrom} = From,
            #jid{luser = LTo, lserver = LSTo} = To,
            case {LFrom, LSFrom} of
                {LTo, LSTo} ->
                    {error, ?ERR_ITEM_NOT_FOUND};
                _ ->
                    {error, ?ERR_NOT_ALLOWED}
            end;
        _ ->
            Acc
    end.


-spec get_user_resources(ejabberd:user(), ejabberd:server()) -> [jlib:xmlel()].
get_user_resources(User, Server) ->
    Rs = ejabberd_sm:get_user_resources(User, Server),
    lists:map(fun(R) ->
                JID = jid:to_binary({User, Server, R}),
                #xmlel{name = <<"item">>,
                       attrs = [{<<"jid">>, JID}, {<<"name">>, User}]}
              end, lists:sort(Rs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Support for: XEP-0157 Contact Addresses for XMPP Services

-spec get_info(Acc :: mongoose_stanza:t(), ejabberd:server(), module(), Node :: binary(),
        Lang :: ejabberd:lang()) -> mongoose_stanza:t().
get_info(Acc, Host, Mod, Node, _Lang) when Node == [] ->
    Module = case Mod of
                 undefined ->
                     ?MODULE;
                 _ ->
                     Mod
             end,
    Serverinfo_fields = get_fields_xml(Host, Module),
    Info = [#xmlel{name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
            children = [#xmlel{name = <<"field">>,
                               attrs = [{<<"var">>, <<"FORM_TYPE">>}, {<<"type">>, <<"hidden">>}],
                               children = [#xmlel{name = <<"value">>,
                                           children = [#xmlcdata{content = ?NS_SERVERINFO}]}]}]
                     ++ Serverinfo_fields}],
    mongoose_stanza:append(info, Info, Acc);
get_info(Acc, _, _, _Node, _) ->
    Acc.


-spec get_fields_xml(ejabberd:server(), module()) -> [jlib:xmlel()].
get_fields_xml(Host, Module) ->
    Fields = gen_mod:get_module_opt(Host, ?MODULE, server_info, []),

    %% filter, and get only the ones allowed for this module
    Fields_good = lists:filter(
                    fun({Modules, _, _}) ->
                            case Modules of
                                all -> true;
                                Modules -> lists:member(Module, Modules)
                            end
                    end,
                    Fields),

    fields_to_xml(Fields_good).


-spec fields_to_xml([{_, Var :: binary(), Values :: [binary()]}]) -> [jlib:xmlel()].
fields_to_xml(Fields) ->
    [ field_to_xml(Field) || Field <- Fields].


-spec field_to_xml({_, Var :: binary(), Values :: [binary()]}) -> jlib:xmlel().
field_to_xml({_, Var, Values}) ->
    Values_xml = values_to_xml(Values),
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, Var}],
           children = Values_xml}.


-spec values_to_xml([binary()]) -> [jlib:xmlel()].
values_to_xml(Values) ->
    lists:map(
      fun(Value) ->
              #xmlel{name = <<"value">>, children = [#xmlcdata{content = Value}]}
      end,
      Values
     ).
