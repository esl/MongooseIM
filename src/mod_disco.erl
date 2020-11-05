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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_disco).
-author('alexey@process-one.net').
-xep([{xep, 30}, {version, "2.4"}]).
-xep([{xep, 157}, {version, "1.0"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([start/2,
         stop/1,
         process_local_iq_items/4,
         process_local_iq_info/4,
         get_local_identity/5,
         get_local_features/5,
         get_local_services/5,
         process_sm_iq_items/4,
         process_sm_iq_info/4,
         get_sm_identity/5,
         get_sm_features/5,
         get_sm_items/5,
         get_info/5,
         register_feature/2,
         unregister_feature/2,
         register_extra_domain/2,
         unregister_extra_domain/2,
         register_subhost/2,
         unregister_subhost/2]).

-export([handler/1,
         handler_spec/0
        ]).

-include("mongoose.hrl").
-include("jlib.hrl").

-type feature() :: any().

-type return_hidden() :: ejabberd_router:return_hidden().

-spec start(jid:server(), list()) -> 'ok'.
start(Host, Opts) ->
    [catch ets:new(Name, [named_table, ordered_set, public]) || Name <-
        [disco_features, disco_extra_domains, disco_sm_features, disco_sm_nodes, disco_subhosts]],

    register_host(Host, Opts),
    register_feature(Host, <<"iq">>),
    register_feature(Host, <<"presence">>),
    register_feature(Host, <<"presence-invisible">>),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE, get_local_identity, 100),
    ExtraDomains = gen_mod:get_opt(extra_domains, Opts, []),
    lists:foreach(fun(Domain) -> register_extra_domain(Host, Domain) end, ExtraDomains).


-spec stop(jid:server()) -> ok.
stop(Host) ->
    unregister_host(Host),
    ejabberd_hooks:delete(disco_local_identity, Host, ?MODULE, get_local_identity, 100).

%%--
%% module_opt([<<"extra_domains">>, <<"mod_disco">>|_] = Path, V) ->
%%     Domains = parse_list(Path, V),
%%     [{extra_domains, Domains}];
%% module_opt([<<"server_info">>, <<"mod_disco">>|_] = Path, V) ->
%%     Info = parse_list(Path, V),
%%     [{server_info, Info}];
%% module_opt([<<"users_can_see_hidden_services">>, <<"mod_disco">>|_], V) ->
%%     [{users_can_see_hidden_services, V}];

%% -spec mod_disco_server_info(path(), toml_section()) -> [option()].
%% mod_disco_server_info(Path, #{<<"module">> := <<"all">>, <<"name">> := Name, <<"urls">> := Urls}) ->
%%     URLList = parse_list([<<"urls">> | Path], Urls),
%%     [{all, b2l(Name), URLList}];
%% mod_disco_server_info(Path, #{<<"module">> := Modules, <<"name">> := Name, <<"urls">> := Urls}) ->
%%     Mods = parse_list([<<"module">> | Path], Modules),
%%     URLList = parse_list([<<"urls">> | Path], Urls),
%%     [{Mods, b2l(Name), URLList}].

%% handler([_, <<"extra_domains">>, <<"mod_disco">>, <<"modules">>]) ->
%%     fun(_, V) -> [V] end;
%% handler([_, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
%%     fun mod_disco_server_info/2;
%% handler([_, <<"urls">>, _, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
%%     fun(_, V) -> [b2l(V)] end;
%% handler([_, <<"module">>, _, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
%%     fun(_, V) -> [b2a(V)]  end;

%% validate([item, <<"extra_domains">>, <<"mod_disco">>, <<"modules">>|_],
%%          [V]) ->
%%     validate_binary_domain(V);
%% validate([item, <<"module">>, item, <<"server_info">>, <<"mod_disco">>, <<"modules">>|_],
%%          [V]) ->
%%     validate_module(V);
%% %% NEVER CALLED!
%% validate([<<"name">>, item, <<"server_info">>, <<"mod_disco">>, <<"modules">>|_],
%%          [V]) ->
%%     validate_non_empty_binary(V); %% it's a list!
%% validate([item, <<"urls">>, item, <<"server_info">>, <<"mod_disco">>, <<"modules">>|_],
%%          [V]) ->
%%     validate_url(V);
%% validate([item, <<"urls">>, <<"mod_disco">>, <<"modules">>|_],
%%          [V]) ->
%%     validate_url(V);
%% validate([<<"users_can_see_hidden_services">>, <<"mod_disco">>, <<"modules">>|_],
%%          [{users_can_see_hidden_services, V}]) ->
%%     validate_boolean(V);
%%--

%% handle_opt([<<"extra_domains">>|_], V) ->
%%     [{extra_domains, V}];
%% handle_opt([<<"server_info">>|_], V) ->
%%     [{server_info, V}];
%% handle_opt([<<"users_can_see_hidden_services">>|_], V) ->
%%     mongoose_config_validator_toml:validate_boolean(V),
%%     [{users_can_see_hidden_services, V}];
%% handle_opt([<<"iqdisc">>|_], V) ->
%%     IQDisc = format_iqdisc(V),
%%     [{iqdisc, IQDisc}].

format_iqdisc(V) ->
    case proplists:lookup(type, V) of
        {_, queues} ->
            {_, N} = proplists:lookup(workers, V),
            {queues, N};
        {_, Type} ->
            none = proplists:lookup(workers, V),
            Type
    end.

%% handle_extra_domain(_Path, V) ->
%%     mongoose_config_validator_toml:validate_binary_domain(V),
%%     [V].

%% handle_server_info(_Path, KVs) ->
%%     [format_server_info(KVs)].

format_server_info(KVs) ->
    {_, Name} = proplists:lookup(name, KVs),
    {_, URLs} = proplists:lookup(urls, KVs),
    Mods = proplists:get_value(module, KVs, all),
    {Mods, Name, URLs}.

%% handle_server_info_opt([<<"urls">>|_], V) ->
%%     [{urls, V}];
%% handle_server_info_opt([<<"module">>|_], V) ->
%%     [{module, V}];
%% handle_server_info_opt([<<"name">>|_], V) ->
%%     Name = binary_to_list(V),
%%     mongoose_config_validator_toml:validate_non_empty_string(Name),
%%     [{name, Name}].

%% handle_url(_Path, V) ->
%%     URL = binary_to_list(V),
%%     mongoose_config_validator_toml:validate_url(URL),
%%     [URL].

%% handle_module(_Path, V) ->
%%     Module = binary_to_atom(V),
%%     mongoose_config_validator_toml:validate_module(Module),
%%     [Module].

%% handle_iq_disc_opt([<<"type">>|_], V) ->
%%     Type = binary_to_atom(V),
%%     mongoose_config_validator_toml:validate_enum(Type, [no_queue, one_queue, parallel, queues]),
%%     [{type, Type}];
%% handle_iq_disc_opt([<<"workers">>|_], V) ->
%%     mongoose_config_validator_toml:validate_positive_integer(V),
%%     [{workers, V}].

%-record(section, {child_handler, children = #{}}).
%-record(list, {item_handler, items}).

-define(HANDLER, handler1).

handler(Path) ->
    ?HANDLER(Path).

handler1(Path) ->
    handler(lists:reverse(Path), handler_spec()).

%% handler2(Path) ->
%%     y(Path).

%% handler3(Path) ->
%%     z(lists:reverse(Path)).

-record(section, {children, process = fun(V) -> V end}).
-record(option, {type, validate}).
-record(list, {items, process = fun(V) -> V end}).

handler([Node], #section{children = Children}) ->
    maps:get(Node, Children);
handler([item], #list{items = Item}) ->
    Item;
handler([Node|Rest], #section{children = Children}) ->
    Child = maps:get(Node, Children),
    handler(Rest, Child);
handler([item|Rest], #list{items = Items}) ->
    handler(Rest, Items).

handler_spec() ->
    #section{
       children = #{<<"users_can_see_hidden_services">> => #option{type = boolean},
                    <<"extra_domains">> => #list{items = #option{type = binary,
                                                                 validate = domain}},
                    <<"server_info">> => #list{items = server_info_spec()},
                    <<"iqdisc">> => iqdisc_spec()
                   }
     }.

server_info_spec() ->
    #section{
       children = #{<<"name">> => #option{type = string,
                                          validate = non_empty},
                    <<"urls">> => #list{items = #option{type = string,
                                                        validate = url}},
                    <<"module">> => #list{items = #option{type = atom,
                                                          validate = module}}
                   },
       process = fun format_server_info/1
      }.

iqdisc_spec() ->
    #section{
       children = #{<<"type">> => #option{type = atom,
                                          validate = {enum, [no_queue, one_queue, parallel, queues]}},
                    <<"workers">> => #option{type = integer,
                                             validate = positive}},
       process = fun ?MODULE:format_iqdisc/1
      }.

%% top_section() ->
%%     #section{child_handler = fun handle_opt/2,
%%              children = #{<<"extra_domains">> => #list{item_handler = fun handle_extra_domain/2},
%%                           <<"server_info">> => #list{item_handler = fun handle_server_info/2,
%%                                                      items = server_info_section()},
%%                           <<"iqdisc">> => #section{child_handler = fun handle_iq_disc_opt/2}
%%                          }
%%             }.

%% server_info_section() ->
%%     #section{child_handler = fun handle_server_info_opt/2,
%%              children = #{<<"urls">> => #list{item_handler = fun handle_url/2},
%%                           <<"module">> => #list{item_handler = fun handle_module/2}
%%                          }
%%             }.

%% y([_]) -> fun handle_opt/2;
%% y([item, <<"extra_domains">>]) -> fun handle_extra_domain/2;
%% y([item, <<"server_info">>]) -> fun handle_server_info/2;
%% y([_, item, <<"server_info">>]) -> fun handle_server_info_opt/2;
%% y([item, <<"urls">>, item, <<"server_info">>]) -> fun handle_url/2;
%% y([item, <<"module">>, item, <<"server_info">>]) -> fun handle_module/2;
%% y([_, <<"iqdisc">>]) -> fun handle_iq_disc_opt/2.

%% z([_]) -> fun handle_opt/2;
%% z([<<"extra_domains">>, item]) -> fun handle_extra_domain/2;
%% z([<<"server_info">>, item]) -> fun handle_server_info/2;
%% z([<<"server_info">>, item, _]) -> fun handle_server_info_opt/2;
%% z([<<"server_info">>, item, <<"urls">>, item]) -> fun handle_url/2;
%% z([<<"server_info">>, item, <<"module">>, item]) -> fun handle_module/2;
%% z([<<"iqdisc">>, _]) -> fun handle_iq_disc_opt/2.

register_subhost(Host, Subhost) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
        false -> ok;
        true ->
            register_host(Subhost, gen_mod:get_module_opts(Host, ?MODULE)),
            ets:insert(disco_subhosts, {{Subhost, Host}})
    end.
register_host(Host, Opts) ->
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

    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_local_services, 100),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE, get_local_features, 100),
    ejabberd_hooks:add(disco_sm_items, Host, ?MODULE, get_sm_items, 100),
    ejabberd_hooks:add(disco_sm_features, Host, ?MODULE, get_sm_features, 100),
    ejabberd_hooks:add(disco_sm_identity, Host, ?MODULE, get_sm_identity, 100),
    ejabberd_hooks:add(disco_info, Host, ?MODULE, get_info, 100),
    ok.

unregister_subhost(Host, Subhost) ->
    case gen_mod:is_loaded(Host, ?MODULE) of
        false -> ok;
        true ->
            unregister_host(Subhost),
            ets:delete(disco_subhosts, {Subhost, Host})
    end.
unregister_host(Host) ->
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
    lists:foreach(fun([{Subhost, _}]) -> unregister_subhost(Host, Subhost) end,
                  ets:match(disco_subhosts, {{'_', Host}})),
    ok.

-spec register_feature(jid:server(), feature()) -> 'true'.
register_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:insert(disco_features, {{Feature, Host}}).


-spec unregister_feature(jid:server(), feature()) -> 'true'.
unregister_feature(Host, Feature) ->
    catch ets:new(disco_features, [named_table, ordered_set, public]),
    ets:delete(disco_features, {Feature, Host}).


-spec register_extra_domain(jid:server(), binary()) -> 'true'.
register_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:insert(disco_extra_domains, {{Domain, Host}}).


-spec unregister_extra_domain(jid:server(), binary()) -> 'true'.
unregister_extra_domain(Host, Domain) ->
    catch ets:new(disco_extra_domains, [named_table, ordered_set, public]),
    ets:delete(disco_extra_domains, {Domain, Host}).


-spec process_local_iq_items(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq_items(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq_items(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    Host = To#jid.lserver,

    case mongoose_hooks:disco_local_items(Host, empty, From, To, Node, Lang) of
        {result, Items} ->
            ANode = make_node_attr(Node),
            {Acc, IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS} | ANode],
                                   children = Items}]}};
        {error, Error} ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
    end.

-spec process_local_iq_info(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_local_iq_info(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_local_iq_info(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    Host = To#jid.lserver,
    Node = xml:get_tag_attr_s(<<"node">>, SubEl),
    Identity = mongoose_hooks:disco_local_identity(Host,
                                                   [],
                                                   From, To, Node, Lang),
    Info = mongoose_hooks:disco_info(Host,
                                     [],
                                     ?MODULE, Node, Lang),
    case mongoose_hooks:disco_local_features(Host,
                                             empty,
                                             From, To, Node, Lang) of
        {result, Features} ->
            ANode = make_node_attr(Node),
            {Acc, IQ#iq{type = result,
                  sub_el = [#xmlel{name = <<"query">>,
                                   attrs = [{<<"xmlns">>, ?NS_DISCO_INFO} | ANode],
                                   children = Identity ++
                                   Info ++
                                   features_to_xml(Features)}]}};
        {error, Error} ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
    end.

-spec get_local_identity(Acc :: [exml:element()],
                        From :: jid:jid(),
                        To :: jid:jid(),
                        Node :: binary(),
                        Lang :: ejabberd:lang()) -> [exml:element()].
get_local_identity(Acc, _From, _To, <<>>, _Lang) ->
    Acc ++ [#xmlel{name = <<"identity">>,
                   attrs = [{<<"category">>, <<"server">>},
                            {<<"type">>, <<"im">>},
                            {<<"name">>, <<"MongooseIM">>}]}];
get_local_identity(Acc, _From, _To, Node, _Lang) when is_binary(Node) ->
    Acc.


-spec get_local_features(Acc :: 'empty' | {'error', _} | {'result', _},
                        From :: jid:jid(),
                        To :: jid:jid(),
                        Node :: binary(),
                        Lang :: ejabberd:lang()) -> {'error', _} | {'result', _}.
get_local_features({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_local_features(Acc, _From, To, <<>>, _Lang) ->
    Feats = case Acc of
                {result, Features} -> Features;
                empty -> []
            end,
    Host = To#jid.lserver,
    {result,
     ets:select(disco_features, [{{{'_', Host}}, [], ['$_']}]) ++ Feats};
get_local_features(Acc, _From, _To, Node, _Lang) when is_binary(Node) ->
    case Acc of
        {result, _Features} ->
            Acc;
        empty ->
            {error, mongoose_xmpp_errors:item_not_found()}
    end.


-spec features_to_xml(FeatureList :: [{feature(), jid:server()}]
                     ) -> [exml:element()].
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


-spec domain_to_xml(binary() | {binary()}) -> exml:element().
domain_to_xml({Domain}) ->
    #xmlel{name = <<"item">>, attrs = [{<<"jid">>, Domain}]};
domain_to_xml(Domain) ->
    #xmlel{name = <<"item">>, attrs = [{<<"jid">>, Domain}]}.


-spec get_local_services(Acc :: 'empty' | {'error', _} | {'result', _},
                         From :: jid:jid(),
                         To :: jid:jid(),
                         Node :: binary(),
                         Lang :: ejabberd:lang()) -> {'error', _} | {'result', _}.
get_local_services({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_local_services(Acc, From, To, <<>>, _Lang) ->
    Items = case Acc of
                {result, Its} -> Its;
                empty -> []
            end,
    Host = To#jid.lserver,
    ReturnHidden = should_return_hidden(Host, From),
    {result,
     lists:usort(
       lists:map(fun domain_to_xml/1,
                 get_vh_services(Host, ReturnHidden) ++
                 ets:select(disco_extra_domains,
                            [{{{'$1', Host}}, [], ['$1']}]))
       ) ++ Items};
get_local_services({result, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_local_services(empty, _From, _To, _Node, _Lang) ->
    {error, mongoose_xmpp_errors:item_not_found()}.

-spec should_return_hidden(Host :: jid:lserver(), From :: jid:jid()) -> return_hidden().
should_return_hidden(_Host, #jid{ luser = <<>> } = _From) ->
    %% We respect "is hidden" flag only when a client performs the query
    all;
should_return_hidden(Host, _From) ->
    case gen_mod:get_module_opt(Host, ?MODULE, users_can_see_hidden_services, true) of
        true -> all;
        false -> only_public
    end.

-type route() :: binary().
-spec get_vh_services(jid:server(), return_hidden()) -> [route()].
get_vh_services(Host, ReturnHidden) ->
    VHosts = lists:sort(fun(H1, H2) -> size(H1) >= size(H2) end, ?MYHOSTS),
    lists:filter(fun(Route) ->
                         check_if_host_is_the_shortest_suffix_for_route(Route, Host, VHosts)
                 end, ejabberd_router:dirty_get_all_routes(ReturnHidden)).

-spec check_if_host_is_the_shortest_suffix_for_route(
        Route :: route(), Host :: binary(), VHosts :: [binary()]) -> boolean().
check_if_host_is_the_shortest_suffix_for_route(Route, Host, VHosts) ->
    RouteS = binary_to_list(Route),
    case lists:dropwhile(
           fun(VH) ->
                   not lists:suffix("." ++ binary_to_list(VH), RouteS)
           end, VHosts) of
        [] ->
            false;
        [VH | _] ->
            VH == Host
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_sm_iq_items(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {string(), jlib:iq()}.
process_sm_iq_items(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq_items(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    case is_presence_subscribed(From, To) of
        true ->
            Host = To#jid.lserver,
            Node = xml:get_tag_attr_s(<<"node">>, SubEl),
            case mongoose_hooks:disco_sm_items(Host,
                                               empty,
                                               From, To, Node, Lang) of
                {result, Items} ->
                    ANode = make_node_attr(Node),
                    {Acc, IQ#iq{type = result,
                          sub_el = [#xmlel{name = <<"query">>,
                                           attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS} | ANode],
                                           children = Items}]}};
                {error, Error} ->
                    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
            end;
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]}}
    end.


-spec get_sm_items(Acc :: 'empty' | {'error', _} | {'result', _},
                   From :: jid:jid(),
                   To :: jid:jid(),
                   Node :: binary(),
                   Lang :: ejabberd:lang()) -> {'error', _} | {'result', _}.
get_sm_items({error, _Error} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_sm_items(Acc, From, To, [], _Lang) ->
    Items = case Acc of
                {result, Its} -> Its;
                empty -> []
            end,
    Items1 = case is_presence_subscribed(From, To) of
                   true ->
                       get_user_resources(To);
                   _ ->
                       []
                end,
    {result, Items ++ Items1};
get_sm_items({result, _} = Acc, _From, _To, _Node, _Lang) ->
    Acc;
get_sm_items(empty, From, To, _Node, _Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
        {LTo, LSTo} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        _ ->
            {error, mongoose_xmpp_errors:not_allowed()}
    end.


-spec is_presence_subscribed(jid:jid(), jid:jid()) -> boolean().
is_presence_subscribed(#jid{luser = LFromUser, lserver = LFromServer} = _From,
                       #jid{luser = LToUser, lserver = LToServer} = _To) ->
    A = mongoose_acc:new(#{ location => ?LOCATION,
                            lserver => LFromServer,
                            element => undefined }),
    A2 = mongoose_hooks:roster_get(LFromServer, A, LFromUser, LFromServer),
    Roster = mongoose_acc:get(roster, items, [], A2),
    lists:any(fun({roster, _, _, JID, _, S, _, _, _, _}) ->
                      {TUser, TServer} = jid:to_lus(JID),
                      LToUser == TUser andalso LToServer == TServer andalso S /= none
              end,
              Roster)
    orelse LFromUser == LToUser andalso LFromServer == LToServer.


-spec process_sm_iq_info(jid:jid(), jid:jid(), mongoose_acc:t(), jlib:iq()) ->
    {mongoose_acc:t(), jlib:iq()}.
process_sm_iq_info(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:not_allowed()]}};
process_sm_iq_info(From, To, Acc, #iq{type = get, lang = Lang, sub_el = SubEl} = IQ) ->
    case is_presence_subscribed(From, To) of
        true ->
            Host = To#jid.lserver,
            Node = xml:get_tag_attr_s(<<"node">>, SubEl),
            Identity = mongoose_hooks:disco_sm_identity(Host,
                                                        [],
                                                        From, To, Node, Lang),
            case mongoose_hooks:disco_sm_features(Host,
                                                  empty,
                                                  From, To, Node, Lang) of
                {result, Features} ->
                    ANode = make_node_attr(Node),
                    {Acc, IQ#iq{type = result,
                          sub_el = [#xmlel{name = <<"query">>,
                                           attrs = [{<<"xmlns">>, ?NS_DISCO_INFO} | ANode],
                                           children = Identity ++
                                           features_to_xml(Features)}]}};
                {error, Error} ->
                    {Acc, IQ#iq{type = error, sub_el = [SubEl, Error]}}
            end;
        false ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:service_unavailable()]}}
    end.


-spec get_sm_identity(Acc :: [exml:element()],
                      From :: jid:jid(),
                      To :: jid:jid(),
                      Node :: binary(),
                      Lang :: ejabberd:lang()) -> [exml:element()].
get_sm_identity(Acc, _From, JID = #jid{}, _Node, _Lang) ->
    Acc ++ case ejabberd_auth:does_user_exist(JID) of
               true ->
                   [#xmlel{name = <<"identity">>,
                           attrs = [{<<"category">>, <<"account">>},
                                    {<<"type">>, <<"registered">>}]}];
               _ ->
                   []
           end.


-spec get_sm_features(empty | {result, [exml:element()]} | {error, any()},
                      From :: jid:jid(),
                      To :: jid:jid(),
                      Node :: binary(),
                      Lang :: ejabberd:lang()) -> {result, [exml:element()]} | {error, any()}.
get_sm_features(empty, From, To, _Node, _Lang) ->
    #jid{luser = LFrom, lserver = LSFrom} = From,
    #jid{luser = LTo, lserver = LSTo} = To,
    case {LFrom, LSFrom} of
        {LTo, LSTo} ->
            {error, mongoose_xmpp_errors:item_not_found()};
        _ ->
            {error, mongoose_xmpp_errors:not_allowed()}
    end;
get_sm_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.


-spec get_user_resources(jid:jid()) -> [exml:element()].
get_user_resources(JID) ->
    #jid{user = User, server = Server} = JID,
    Rs = ejabberd_sm:get_user_resources(JID),
    lists:map(fun(R) ->
                BJID = jid:to_binary({User, Server, R}),
                #xmlel{name = <<"item">>,
                       attrs = [{<<"jid">>, BJID}, {<<"name">>, User}]}
              end, lists:sort(Rs)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec make_node_attr(Node :: binary()) -> [{binary(), binary()}].
make_node_attr(<<>>) -> [];
make_node_attr(Node) -> [{<<"node">>, Node}].

%%% Support for: XEP-0157 Contact Addresses for XMPP Services

-spec get_info(Acc :: [exml:element()], jid:server(), module(), Node :: binary(),
        Lang :: ejabberd:lang()) -> [exml:element()].
get_info(Acc, Host, Mod, Node, _Lang) when Node == <<>> ->
    Module = case Mod of
                 undefined ->
                     ?MODULE;
                 _ ->
                     Mod
             end,
    ServerInfoFields = get_fields_xml(Host, Module),
    FormTypeField = #xmlel{name = <<"field">>,
                           attrs = [{<<"var">>, <<"FORM_TYPE">>}, {<<"type">>, <<"hidden">>}],
                           children = [#xmlel{name = <<"value">>,
                                              children = [#xmlcdata{content = ?NS_SERVERINFO}]}]},
    [#xmlel{name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
            children = [FormTypeField | ServerInfoFields]} | Acc];
get_info(Acc, _, _, _Node, _) ->
    Acc.


-spec get_fields_xml(jid:server(), module()) -> [exml:element()].
get_fields_xml(Host, Module) ->
    Fields = gen_mod:get_module_opt(Host, ?MODULE, server_info, []),

    %% filter, and get only the ones allowed for this module
    FilteredFields = lists:filter(
                       fun({Modules, _, _}) ->
                               case Modules of
                                   all -> true;
                                   Modules -> lists:member(Module, Modules)
                               end
                       end,
                       Fields),

    fields_to_xml(FilteredFields).


-spec fields_to_xml([{Modules :: [module()], Var :: string(), Values :: [string()]}]) ->
    [exml:element()].
fields_to_xml(Fields) ->
    [ field_to_xml(Field) || Field <- Fields].


-spec field_to_xml({Modules :: [module()], Var :: string(), Values :: [string()]}) -> exml:element().
field_to_xml({_Module, Var, Values}) ->
    #xmlel{name = <<"field">>, attrs = [{<<"var">>, list_to_binary(Var)}],
           children = values_to_xml(Values)}.


-spec values_to_xml([binary()]) -> [exml:element()].
values_to_xml(Values) ->
    [ #xmlel{name = <<"value">>, children = [#xmlcdata{content = list_to_binary(Value)}]}
      || Value <- Values ].

