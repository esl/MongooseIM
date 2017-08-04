%%%----------------------------------------------------------------------
%%% File    : mod_caps.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : Request and cache Entity Capabilities (XEP-0115)
%%% Created : 7 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2015   ProcessOne
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
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%% 2009, improvements from ProcessOne to support correct PEP handling
%%% through s2s, use less memory, and speedup global caps handling
%%%----------------------------------------------------------------------

-module(mod_caps).

-author('henoch@dtek.chalmers.se').

-xep([{xep, 115}, {version, "1.5"}]).

-behaviour(gen_server).

-behaviour(gen_mod).

-export([read_caps/1, caps_stream_features/2,
         disco_features/5, disco_identity/5, disco_info/5,
         get_features/2]).

%% gen_mod callbacks
-export([start/2, start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
         handle_cast/2, terminate/2, code_change/3]).

-export([user_send_packet/4, user_receive_packet/5,
         c2s_presence_in/2, c2s_filter_packet/6,
         c2s_broadcast_recipients/6]).

%% for test cases
-export([delete_caps/1, make_disco_hash/2]).

-include("ejabberd.hrl").

-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_caps).

-define(BAD_HASH_LIFETIME, 600).

-record(caps,
        {
          node    = <<"">> :: binary(),
          version = <<"">> :: binary(),
          hash    = <<"">> :: binary(),
          exts    = []     :: [binary()]
        }).

-type caps() :: #caps{}.

-export_type([caps/0]).

-record(caps_features,
        {
          node_pair = {<<"">>, <<"">>} :: {binary(), binary()},
          features  = []               :: [binary()] | pos_integer()
        }).

-record(state, {host = <<"">> :: binary()}).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
                          [Host, Opts], []).

start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
                 transient, 1000, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).

get_features_list(Host, Caps) ->
    case get_features(Host, Caps) of
        unknown -> [];
        Features -> Features
    end.

get_features(_Host, nothing) -> [];
get_features(Host, #caps{node = Node, version = Version,
                         exts = Exts}) ->
    SubNodes = [Version | Exts],
    lists:foldl(fun (SubNode, Acc) ->
                        NodePair = {Node, SubNode},
                        case cache_tab:lookup(caps_features, NodePair,
                                              caps_read_fun(Host, NodePair))
                        of
                            {ok, Features} when is_list(Features) ->
                                Features ++ Acc;
                            _ when Acc == [] ->
                                unknown;
                            _ ->
                                Acc
                        end
                end,
                [], SubNodes).

-spec read_caps([xmlel()]) -> nothing | caps().

read_caps(Els) -> read_caps(Els, nothing).

read_caps([#xmlel{name = <<"c">>, attrs = Attrs}
           | Tail],
          Result) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_CAPS ->
            Node = xml:get_attr_s(<<"node">>, Attrs),
            Version = xml:get_attr_s(<<"ver">>, Attrs),
            Hash = xml:get_attr_s(<<"hash">>, Attrs),
            Exts = str:tokens(xml:get_attr_s(<<"ext">>, Attrs),
                              <<" ">>),
            read_caps(Tail,
                      #caps{node = Node, hash = Hash, version = Version,
                            exts = Exts});
        _ -> read_caps(Tail, Result)
    end;
read_caps([#xmlel{name = <<"x">>, attrs = Attrs}
           | Tail],
          Result) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_MUC_USER -> nothing;
        _ -> read_caps(Tail, Result)
    end;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) -> Result.

user_send_packet(Acc,
                 #jid{luser = User, lserver = Server} = From,
                 #jid{luser = User, lserver = Server,
                      lresource = <<"">>},
                 #xmlel{name = <<"presence">>, attrs = Attrs,
                        children = Els}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case Type == <<"">> orelse Type == <<"available">> of
        true ->
            case read_caps(Els) of
                nothing -> ok;
                #caps{version = Version, exts = Exts} = Caps ->
                    feature_request(Server, From, Caps, [Version | Exts])
            end;
        false -> ok
    end,
    Acc;
user_send_packet(Acc, _From, _To, _Pkt) ->
    Acc.

user_receive_packet(Acc, #jid{lserver = Server}, From, _To,
                    #xmlel{name = <<"presence">>, attrs = Attrs,
                           children = Els}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case (not lists:member(From#jid.lserver, ?MYHOSTS))
         andalso ((Type == <<"">>) or (Type == <<"available">>)) of
        true ->
            case read_caps(Els) of
                nothing -> ok;
                #caps{version = Version, exts = Exts} = Caps ->
                    feature_request(Server, From, Caps, [Version | Exts])
            end;
        false -> ok
    end,
    Acc;
user_receive_packet(Acc, _JID, _From, _To, _Pkt) ->
    Acc.

-spec caps_stream_features([xmlel()], binary()) -> [xmlel()].

caps_stream_features(Acc, MyHost) ->
    case make_my_disco_hash(MyHost) of
        <<"">> -> Acc;
        Hash ->
            [#xmlel{name = <<"c">>,
                    attrs =
                        [{<<"xmlns">>, ?NS_CAPS}, {<<"hash">>, <<"sha-1">>},
                         {<<"node">>, ?MONGOOSE_URI}, {<<"ver">>, Hash}],
                    children = []}
             | Acc]
    end.

disco_features(Acc, From, To, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_local_features,
                                    To#jid.lserver, empty,
                                    [From, To, <<"">>, Lang]);
        false ->
            Acc
    end.

disco_identity(Acc, From, To, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_local_identity,
                                    To#jid.lserver, [],
                                    [From, To, <<"">>, Lang]);
        false ->
            Acc
    end.

disco_info(Acc, Host, Module, Node, Lang) ->
    case is_valid_node(Node) of
        true ->
            ejabberd_hooks:run_fold(disco_info, Host, [],
                                    [Host, Module, <<"">>, Lang]);
        false ->
            Acc
    end.

c2s_presence_in(C2SState,
                {From, To, {_, _, Attrs, Els}}) ->
    ?DEBUG("Presence to ~p from ~p with Els ~p", [To, From, Els]),
    Type = xml:get_attr_s(<<"type">>, Attrs),
    Subscription = ejabberd_c2s:get_subscription(From,
                                                 C2SState),
    ?DEBUG("Subscription ~p, type ~p", [Subscription, Type]),
    Insert = ((Type == <<"">>) or (Type == <<"available">>))
        and ((Subscription == both) or (Subscription == to)),
    Delete = (Type == <<"unavailable">>) or
                                           (Type == <<"error">>),
    case Insert or Delete of
        true ->
            LFrom = jid:to_lower(From),
            Rs = case ejabberd_c2s:get_aux_field(caps_resources,
                                                 C2SState)
                 of
                     {ok, Rs1} -> Rs1;
                     error -> gb_trees:empty()
                 end,
            Caps = read_caps(Els),
            NewRs = case Caps of
                        nothing when Insert == true -> Rs;
                        _ when Insert == true ->
                            ?DEBUG("Set CAPS to ~p for ~p in ~p", [Caps, LFrom, To]),
                            upsert_caps(LFrom, From, To, Caps, Rs);
                        _ -> gb_trees:delete_any(LFrom, Rs)
                    end,
            ejabberd_c2s:set_aux_field(caps_resources, NewRs,
                                       C2SState);
       false -> C2SState
    end.

upsert_caps(LFrom, From, To, Caps, Rs) ->
    case gb_trees:lookup(LFrom, Rs) of
        {value, Caps} -> Rs;
        none ->
            ejabberd_hooks:run(caps_add, To#jid.lserver,
                               [From, To, self(),
                                get_features(To#jid.lserver, Caps)]),
            gb_trees:insert(LFrom, Caps, Rs);
        _ ->
            ejabberd_hooks:run(caps_update, To#jid.lserver,
                               [From, To, self(),
                                get_features(To#jid.lserver, Caps)]),
            gb_trees:update(LFrom, Caps, Rs)
    end.

c2s_filter_packet(InAcc, Host, C2SState, {pep_message, Feature}, To, _Packet) ->
    case ejabberd_c2s:get_aux_field(caps_resources, C2SState) of
        {ok, Rs} ->
            ?DEBUG("Look for CAPS for ~p in ~p (res: ~p)", [To, C2SState, Rs]),
            LTo = jid:to_lower(To),
            case gb_trees:lookup(LTo, Rs) of
                {value, Caps} ->
                    Drop = not lists:member(Feature, get_features_list(Host, Caps)),
                    {stop, Drop};
                none ->
                    {stop, true}
            end;
        _ -> InAcc
    end;
c2s_filter_packet(Acc, _, _, _, _, _) -> Acc.

c2s_broadcast_recipients(InAcc, Host, C2SState,
                         {pep_message, Feature}, _From, _Packet) ->
    case ejabberd_c2s:get_aux_field(caps_resources, C2SState) of
        {ok, Rs} ->
            filter_recipients_by_caps(InAcc, Feature, Host, Rs);
        _ -> InAcc
    end;
c2s_broadcast_recipients(Acc, _, _, _, _, _) -> Acc.

filter_recipients_by_caps(InAcc, Feature, Host, Rs) ->
    gb_trees_fold(fun(USR, Caps, Acc) ->
                          case lists:member(Feature, get_features_list(Host, Caps)) of
                              true -> [USR | Acc];
                              false -> Acc
                          end
                  end,
                  InAcc, Rs).

init_db(mnesia, _Host) ->
    case catch mnesia:table_info(caps_features, storage_type) of
        {'EXIT', _} ->
            ok;
        disc_only_copies ->
            ok;
        _ ->
            mnesia:delete_table(caps_features)
    end,
    mnesia:create_table(caps_features,
                        [{disc_only_copies, [node()]},
                         {local_content, true},
                         {attributes,
                          record_info(fields, caps_features)}]),
    mnesia:add_table_copy(caps_features, node(),
                          disc_only_copies).

init([Host, Opts]) ->
    init_db(mnesia, Host),
    MaxSize = gen_mod:get_opt(cache_size, Opts,
                              fun(I) when is_integer(I), I>0 -> I end,
                              1000),
    LifeTime = gen_mod:get_opt(cache_life_time, Opts,
                               fun(I) when is_integer(I), I>0 -> I end,
                               timer:hours(24) div 1000),
    cache_tab:new(caps_features,
                  [{max_size, MaxSize}, {life_time, LifeTime}]),
    ejabberd_hooks:add(c2s_presence_in, Host, ?MODULE,
                       c2s_presence_in, 75),
    ejabberd_hooks:add(c2s_filter_packet, Host, ?MODULE,
                       c2s_filter_packet, 75),
    ejabberd_hooks:add(c2s_broadcast_recipients, Host,
                       ?MODULE, c2s_broadcast_recipients, 75),
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
                       user_send_packet, 75),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
                       user_receive_packet, 75),
    ejabberd_hooks:add(c2s_stream_features, Host, ?MODULE,
                       caps_stream_features, 75),
    ejabberd_hooks:add(s2s_stream_features, Host, ?MODULE,
                       caps_stream_features, 75),
    ejabberd_hooks:add(disco_local_features, Host, ?MODULE,
                       disco_features, 75),
    ejabberd_hooks:add(disco_local_identity, Host, ?MODULE,
                       disco_identity, 75),
    ejabberd_hooks:add(disco_info, Host, ?MODULE,
                       disco_info, 75),
    {ok, #state{host = Host}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    Host = State#state.host,
    ejabberd_hooks:delete(c2s_presence_in, Host, ?MODULE,
                          c2s_presence_in, 75),
    ejabberd_hooks:delete(c2s_filter_packet, Host, ?MODULE,
                          c2s_filter_packet, 75),
    ejabberd_hooks:delete(c2s_broadcast_recipients, Host,
                          ?MODULE, c2s_broadcast_recipients, 75),
    ejabberd_hooks:delete(user_send_packet, Host, ?MODULE,
                          user_send_packet, 75),
    ejabberd_hooks:delete(user_receive_packet, Host,
                          ?MODULE, user_receive_packet, 75),
    ejabberd_hooks:delete(c2s_stream_features, Host,
                          ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(s2s_stream_features, Host,
                          ?MODULE, caps_stream_features, 75),
    ejabberd_hooks:delete(disco_local_features, Host,
                          ?MODULE, disco_features, 75),
    ejabberd_hooks:delete(disco_local_identity, Host,
                          ?MODULE, disco_identity, 75),
    ejabberd_hooks:delete(disco_info, Host, ?MODULE,
                          disco_info, 75),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

feature_request(Host, From, Caps,
                [SubNode | Tail] = SubNodes) ->
    Node = Caps#caps.node,
    NodePair = {Node, SubNode},
    case cache_tab:lookup(caps_features, NodePair,
                          caps_read_fun(Host, NodePair))
    of
        {ok, Fs} when is_list(Fs) ->
            feature_request(Host, From, Caps, Tail);
        Other ->
            NeedRequest = case Other of
                              {ok, TS} -> now_ts() >= TS + (?BAD_HASH_LIFETIME);
                              _ -> true
                          end,
            F = fun (IQReply) ->
                        feature_response(IQReply, Host, From, Caps,
                                         SubNodes)
                end,
            case NeedRequest of
                true ->
                    IQ = #iq{type = get, xmlns = ?NS_DISCO_INFO,
                             sub_el =
                                 [#xmlel{name = <<"query">>,
                                         attrs =
                                             [{<<"xmlns">>, ?NS_DISCO_INFO},
                                              {<<"node">>,
                                               <<Node/binary, "#",
                                                 SubNode/binary>>}],
                                         children = []}]},
                    cache_tab:insert(caps_features, NodePair, now_ts(),
                                     caps_write_fun(Host, NodePair, now_ts())),
                    ejabberd_local:route_iq(jid:make(<<"">>, Host, <<"">>), From, IQ, F);
               false -> feature_request(Host, From, Caps, Tail)
            end
    end;
feature_request(Host, From, Caps, []) ->
    %% feature_request is never executed with empty SubNodes list
    %% so if we end up here, it means the caps are known
    ejabberd_hooks:run(caps_recognised, Host, [From, self(), get_features_list(Host, Caps)]).

feature_response(#iq{type = result,
                     sub_el = [#xmlel{children = Els}]},
                 Host, From, Caps, [SubNode | SubNodes]) ->
    NodePair = {Caps#caps.node, SubNode},
    case check_hash(Caps, Els) of
        true ->
            Features = lists:flatmap(fun (#xmlel{name =
                                                     <<"feature">>,
                                                 attrs = FAttrs}) ->
                                             [xml:get_attr_s(<<"var">>, FAttrs)];
                                         (_) -> []
                                     end,
                                     Els),
            cache_tab:insert(caps_features, NodePair,
                             Features,
                             caps_write_fun(Host, NodePair, Features));
        false -> ok
    end,
    feature_request(Host, From, Caps, SubNodes);
feature_response(_IQResult, Host, From, Caps,
                 [_SubNode | SubNodes]) ->
    feature_request(Host, From, Caps, SubNodes).

caps_read_fun(Host, Node) ->
    LServer = jid:nameprep(Host),
    DBType = db_type(LServer),
    caps_read_fun(LServer, Node, DBType).

caps_read_fun(_LServer, Node, mnesia) ->
    fun () ->
            case mnesia:dirty_read({caps_features, Node}) of
                [#caps_features{features = Features}] -> {ok, Features};
                _ -> error
            end
    end.

caps_write_fun(Host, Node, Features) ->
    LServer = jid:nameprep(Host),
    DBType = db_type(LServer),
    caps_write_fun(LServer, Node, Features, DBType).

caps_write_fun(_LServer, Node, Features, mnesia) ->
    fun () ->
            mnesia:dirty_write(#caps_features{node_pair = Node,
                                              features = Features})
    end.

delete_caps(Node) ->
    cache_tab:delete(caps_features, Node, caps_delete_fun(Node)).

caps_delete_fun(Node) ->
    fun () ->
            mnesia:dirty_delete(caps_features, Node)
    end.

make_my_disco_hash(Host) ->
    JID = jid:make(<<"">>, Host, <<"">>),
    case {ejabberd_hooks:run_fold(disco_local_features,
                                  Host, empty, [JID, JID, <<"">>, <<"">>]),
          ejabberd_hooks:run_fold(disco_local_identity, Host, [],
                                  [JID, JID, <<"">>, <<"">>]),
          ejabberd_hooks:run_fold(disco_info, Host, [],
                                  [Host, undefined, <<"">>, <<"">>])}
    of
        {{result, Features}, Identities, Info} ->
            Feats = lists:map(fun ({{Feat, _Host}}) ->
                                      #xmlel{name = <<"feature">>,
                                             attrs = [{<<"var">>, Feat}],
                                             children = []};
                                  (Feat) ->
                                      #xmlel{name = <<"feature">>,
                                             attrs = [{<<"var">>, Feat}],
                                             children = []}
                              end,
                              Features),
            make_disco_hash(Identities ++ Info ++ Feats, sha1);
        _Err -> <<"">>
    end.

make_disco_hash(DiscoEls, Algo) ->
    Concat = list_to_binary([concat_identities(DiscoEls),
                             concat_features(DiscoEls), concat_info(DiscoEls)]),
    jlib:encode_base64(case Algo of
                           md5 -> erlang:md5(Concat);
                           sha1 -> crypto:hash(sha, Concat);
                           sha224 -> crypto:hash(sha224, Concat);
                           sha256 -> crypto:hash(sha256, Concat);
                           sha384 -> crypto:hash(sha384, Concat);
                           sha512 -> crypto:hash(sha512, Concat)
                       end).

check_hash(Caps, Els) ->
    case Caps#caps.hash of
        <<"md5">> ->
            Caps#caps.version == make_disco_hash(Els, md5);
        <<"sha-1">> ->
            Caps#caps.version == make_disco_hash(Els, sha1);
        <<"sha-224">> ->
            Caps#caps.version == make_disco_hash(Els, sha224);
        <<"sha-256">> ->
            Caps#caps.version == make_disco_hash(Els, sha256);
        <<"sha-384">> ->
            Caps#caps.version == make_disco_hash(Els, sha384);
        <<"sha-512">> ->
            Caps#caps.version == make_disco_hash(Els, sha512);
        _ -> true
    end.

concat_features(Els) ->
    lists:usort(lists:flatmap(fun (#xmlel{name =
                                              <<"feature">>,
                                          attrs = Attrs}) ->
                                      [[xml:get_attr_s(<<"var">>, Attrs), $<]];
                                  (_) -> []
                              end,
                              Els)).

concat_identities(Els) ->
    lists:sort(lists:flatmap(fun (#xmlel{name =
                                             <<"identity">>,
                                         attrs = Attrs}) ->
                                     [[xml:get_attr_s(<<"category">>, Attrs),
                                       $/, xml:get_attr_s(<<"type">>, Attrs),
                                       $/,
                                       xml:get_attr_s(<<"xml:lang">>, Attrs),
                                       $/, xml:get_attr_s(<<"name">>, Attrs),
                                       $<]];
                                 (_) -> []
                             end,
                             Els)).

concat_info(Els) ->
    lists:sort(lists:flatmap(fun (#xmlel{name = <<"x">>,
                                         attrs = Attrs, children = Fields}) ->
                                     case {xml:get_attr_s(<<"xmlns">>, Attrs),
                                           xml:get_attr_s(<<"type">>, Attrs)}
                                     of
                                         {?NS_XDATA, <<"result">>} ->
                                             [concat_xdata_fields(Fields)];
                                         _ -> []
                                     end;
                                 (_) -> []
                             end,
                             Els)).

concat_xdata_fields(Fields) ->
    {FormType, Res} =
    lists:foldl(fun(#xmlel{name = <<"field">>, children = Els} = FieldEl,
                    {FormType0, VarFields} = Acc) ->
                        case exml_query:attr(FieldEl, <<"var">>, <<"">>) of
                            <<"">> -> Acc;
                            <<"FORM_TYPE">> ->
                                {exml_query:path(FieldEl, [{element, <<"value">>}, cdata]),
                                 VarFields};
                            Var ->
                                NewField = [[Var, $<], extract_values_sorted_cdatas(Els)],
                                {FormType0, [NewField | VarFields]}
                        end;
                   (_, Acc) -> Acc
                end,
                {<<"">>, []}, Fields),
    [FormType, $<, lists:sort(Res)].

extract_values_sorted_cdatas(Els) ->
    lists:sort(lists:flatmap(fun extract_value_cdata/1, Els)).

extract_value_cdata(#xmlel{name = <<"value">>} = ValueEl) ->
    [[exml_query:cdata(ValueEl), $<]];
extract_value_cdata(_) ->
    [].

gb_trees_fold(F, Acc, Tree) ->
    Iter = gb_trees:iterator(Tree),
    gb_trees_fold_iter(F, Acc, Iter).

gb_trees_fold_iter(F, Acc, Iter) ->
    case gb_trees:next(Iter) of
        {Key, Val, NewIter} ->
            NewAcc = F(Key, Val, Acc),
            gb_trees_fold_iter(F, NewAcc, NewIter);
        _ -> Acc
    end.

now_ts() ->
    {MS, S, _US} = os:timestamp(),
    MS * 1000000 + S.

is_valid_node(Node) ->
    case str:tokens(Node, <<"#">>) of
        [?MONGOOSE_URI|_] ->
            true;
        _ ->
            false
    end.

db_type(_Host) ->
    mnesia.
