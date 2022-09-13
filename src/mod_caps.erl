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
-behaviour(mongoose_module_metrics).

-export([read_caps/1, caps_stream_features/3,
         disco_local_features/3, disco_local_identity/1, disco_info/1]).

%% gen_mod callbacks
-export([start/2, start_link/2, stop/1, config_spec/0, supported_features/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
         handle_cast/2, terminate/2, code_change/3]).

-export([user_send_packet/4, user_receive_packet/5,
         c2s_presence_in/4, c2s_filter_packet/5,
         c2s_broadcast_recipients/5]).

%% for test cases
-export([delete_caps/1, make_disco_hash/2]).

-ignore_xref([c2s_broadcast_recipients/5, c2s_filter_packet/5, c2s_presence_in/4,
              caps_stream_features/3, delete_caps/1, disco_info/1, disco_local_features/3,
              disco_local_identity/1, make_disco_hash/2, read_caps/1, start_link/2,
              user_receive_packet/5, user_send_packet/4]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-include("jlib.hrl").

-define(PROCNAME, ejabberd_mod_caps).

-define(BAD_HASH_LIFETIME, 600).

-record(caps,
        {
          node = <<>> :: binary(),
          version = <<>> :: binary(),
          hash = <<>> :: binary(),
          exts = [] :: [binary()]
        }).

-type caps() :: #caps{}.
-type caps_resources() :: gb_trees:tree(jid:simple_jid(), caps()).

-export_type([caps/0]).

-type features() :: [binary()].
-type maybe_pending_features() :: features() | pos_integer().
-type node_pair() :: {binary(), binary()}.

-record(caps_features,
        {
          node_pair = {<<>>, <<>>} :: node_pair(),
          features = [] :: maybe_pending_features()
        }).

-record(state, {host_type :: mongooseim:host_type()}).

-type state() :: #state{}.

-spec start_link(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_link(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE,
                          [HostType, Opts], []).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    ChildSpec = {Proc, {?MODULE, start_link, [HostType, Opts]},
                 transient, 1000, worker, [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

-spec stop(mongooseim:host_type()) -> any().
stop(HostType) ->
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    gen_server:call(Proc, stop),
    ejabberd_sup:stop_child(Proc).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"cache_size">> => #option{type = integer,
                                              validate = positive},
                  <<"cache_life_time">> => #option{type = integer,
                                                   validate = positive}
                 },
        defaults = #{<<"cache_size">> => 1000,
                     <<"cache_life_time">> => timer:hours(24) div 1000}
       }.

supported_features() -> [dynamic_domains].

-spec get_features_list(mongooseim:host_type(), nothing | caps()) -> features().
get_features_list(HostType, Caps) ->
    case get_features(HostType, Caps) of
        unknown -> [];
        Features -> Features
    end.

-spec get_features(mongooseim:host_type(), nothing | caps()) -> unknown | features().
get_features(_HostType, nothing) -> [];
get_features(HostType, #caps{node = Node, version = Version, exts = Exts}) ->
    SubNodes = [Version | Exts],
    lists:foldl(fun (SubNode, Acc) ->
                        NodePair = {Node, SubNode},
                        case cache_tab:lookup(caps_features, NodePair,
                                              caps_read_fun(HostType, NodePair))
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

-spec read_caps([exml:element()]) -> nothing | caps().
read_caps(Els) -> read_caps(Els, nothing).

read_caps([#xmlel{name = <<"c">>, attrs = Attrs} | Tail], Result) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_CAPS ->
            Node = xml:get_attr_s(<<"node">>, Attrs),
            Version = xml:get_attr_s(<<"ver">>, Attrs),
            Hash = xml:get_attr_s(<<"hash">>, Attrs),
            Exts = mongoose_bin:tokens(xml:get_attr_s(<<"ext">>, Attrs), <<" ">>),
            read_caps(Tail, #caps{node = Node, hash = Hash, version = Version, exts = Exts});
        _ -> read_caps(Tail, Result)
    end;
read_caps([#xmlel{name = <<"x">>, attrs = Attrs} | Tail], Result) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_MUC_USER -> nothing;
        _ -> read_caps(Tail, Result)
    end;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) -> Result.

-spec user_send_packet(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
user_send_packet(Acc,
                 #jid{luser = User, lserver = LServer} = From,
                 #jid{luser = User, lserver = LServer, lresource = <<>>},
                 #xmlel{name = <<"presence">>, attrs = Attrs, children = Elements}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    handle_presence(Acc, LServer, From, Type, Elements);
user_send_packet(Acc, _From, _To, _Pkt) ->
    Acc.

-spec user_receive_packet(mongoose_acc:t(), jid:jid(), jid:jid(), jid:jid(), exml:element()) ->
          mongoose_acc:t().
user_receive_packet(Acc, #jid{lserver = LServer}, From, _To,
                    #xmlel{name = <<"presence">>, attrs = Attrs, children = Elements}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case mongoose_domain_api:get_host_type(From#jid.lserver) of
        {error, not_found} ->
            handle_presence(Acc, LServer, From, Type, Elements);
        {ok, _} ->
            Acc %% it was already handled in 'user_send_packet'
    end;
user_receive_packet(Acc, _JID, _From, _To, _Pkt) ->
    Acc.

-spec handle_presence(mongoose_acc:t(), jid:lserver(), jid:jid(), binary(), [exml:element()]) ->
          mongoose_acc:t().
handle_presence(Acc, LServer, From, Type, Elements) when Type =:= <<>>;
                                                         Type =:= <<"available">> ->
    case read_caps(Elements) of
        nothing ->
            Acc;
        #caps{version = Version, exts = Exts} = Caps ->
            feature_request(Acc, LServer, From, Caps, [Version | Exts])
    end;
handle_presence(Acc, _LServer, _From, _Type, _Elements) ->
    Acc.

-spec caps_stream_features([exml:element()], mongooseim:host_type(), jid:lserver()) ->
          [exml:element()].
caps_stream_features(Acc, HostType, LServer) ->
    case make_my_disco_hash(HostType, LServer) of
        <<>> ->
            Acc;
        Hash ->
            [#xmlel{name = <<"c">>,
                    attrs = [{<<"xmlns">>, ?NS_CAPS}, {<<"hash">>, <<"sha-1">>},
                             {<<"node">>, ?MONGOOSE_URI}, {<<"ver">>, Hash}],
                    children = []}
             | Acc]
    end.

-spec disco_local_features(mongoose_disco:feature_acc(),
                           map(),
                           map()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := Node}, _, _) ->
    NewAcc = case is_valid_node(Node) of
        true -> Acc#{node := <<>>};
        false -> Acc
    end,
    {ok, NewAcc}.

-spec disco_local_identity(mongoose_disco:identity_acc()) -> mongoose_disco:identity_acc().
disco_local_identity(Acc = #{node := Node}) ->
    case is_valid_node(Node) of
        true -> Acc#{node := <<>>};
        false -> Acc
    end.

-spec disco_info(mongoose_disco:info_acc()) -> mongoose_disco:info_acc().
disco_info(Acc = #{node := Node}) ->
    case is_valid_node(Node) of
        true -> Acc#{node := <<>>};
        false -> Acc
    end.

-spec c2s_presence_in(ejabberd_c2s:state(), jid:jid(), jid:jid(), exml:element()) ->
          ejabberd_c2s:state().
c2s_presence_in(C2SState, From, To, Packet = #xmlel{attrs = Attrs, children = Els}) ->
    ?LOG_DEBUG(#{what => caps_c2s_presence_in,
                 to => jid:to_binary(To), from => jid:to_binary(From),
                 exml_packet => Packet, c2s_state => C2SState}),
    Type = xml:get_attr_s(<<"type">>, Attrs),
    Subscription = ejabberd_c2s:get_subscription(From, C2SState),
    Insert = (Type == <<>> orelse Type == <<"available">>)
        and (Subscription == both orelse Subscription == to),
    Delete = Type == <<"unavailable">> orelse Type == <<"error">>,
    case Insert orelse Delete of
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
                            ?LOG_DEBUG(#{what => caps_set_caps, caps => Caps,
                                         to => jid:to_binary(To), from => jid:to_binary(From),
                                         exml_packet => Packet, c2s_state => C2SState}),
                            upsert_caps(LFrom, Caps, Rs);
                        _ -> gb_trees:delete_any(LFrom, Rs)
                    end,
            ejabberd_c2s:set_aux_field(caps_resources, NewRs,
                                       C2SState);
        false -> C2SState
    end.

-spec upsert_caps(jid:simple_jid(), caps(), caps_resources()) -> caps_resources().
upsert_caps(LFrom, Caps, Rs) ->
    case gb_trees:lookup(LFrom, Rs) of
        {value, Caps} -> Rs;
        none ->
            gb_trees:insert(LFrom, Caps, Rs);
        _ ->
            gb_trees:update(LFrom, Caps, Rs)
    end.

-spec c2s_filter_packet(Acc, ejabberd_c2s:state(), {atom(), binary()},
                        jid:jid(), exml:element()) -> Acc
              when Acc :: boolean().
c2s_filter_packet(InAcc, C2SState, {pep_message, Feature}, To, _Packet) ->
    case ejabberd_c2s:get_aux_field(caps_resources, C2SState) of
        {ok, Rs} ->
            ?LOG_DEBUG(#{what => caps_lookup, text => <<"Look for CAPS for To jid">>,
                         acc => InAcc, c2s_state => C2SState, caps_resources => Rs}),
            LTo = jid:to_lower(To),
            case gb_trees:lookup(LTo, Rs) of
                {value, Caps} ->
                    HostType = ejabberd_c2s_state:host_type(C2SState),
                    Drop = not lists:member(Feature, get_features_list(HostType, Caps)),
                    {stop, Drop};
                none ->
                    {stop, true}
            end;
        _ -> InAcc
    end;
c2s_filter_packet(Acc, _, _, _, _) -> Acc.

-spec c2s_broadcast_recipients(Acc, ejabberd_c2s:state(), {atom(), binary()},
                               jid:jid(), exml:element()) -> Acc
              when Acc :: [jid:simple_jid()].
c2s_broadcast_recipients(InAcc, C2SState, {pep_message, Feature}, _From, _Packet) ->
    HostType = ejabberd_c2s_state:host_type(C2SState),
    case ejabberd_c2s:get_aux_field(caps_resources, C2SState) of
        {ok, Rs} ->
            filter_recipients_by_caps(HostType, InAcc, Feature, Rs);
        _ -> InAcc
    end;
c2s_broadcast_recipients(Acc, _, _, _, _) -> Acc.

-spec filter_recipients_by_caps(mongooseim:host_type(), Acc,
                                {atom(), binary()}, caps_resources()) -> Acc
              when Acc :: [jid:simple_jid()].
filter_recipients_by_caps(HostType, InAcc, Feature, Rs) ->
    gb_trees_fold(fun(USR, Caps, Acc) ->
                          case lists:member(Feature, get_features_list(HostType, Caps)) of
                              true -> [USR | Acc];
                              false -> Acc
                          end
                  end,
                  InAcc, Rs).

init_db(mnesia) ->
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

-spec init(list()) -> {ok, state()}.
init([HostType, #{cache_size := MaxSize, cache_life_time := LifeTime}]) ->
    init_db(db_type(HostType)),
    cache_tab:new(caps_features, [{max_size, MaxSize}, {life_time, LifeTime}]),
    ejabberd_hooks:add(legacy_hooks(HostType)),
    gen_hook:add_handlers(hooks(HostType)),
    {ok, #state{host_type = HostType}}.

-spec handle_call(term(), any(), state()) ->
          {stop, normal, ok, state()} | {reply, {error, any()}, state()}.
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, #state{host_type = HostType}) ->
    ejabberd_hooks:delete(legacy_hooks(HostType)),
    gen_hook:delete_handlers(hooks(HostType)).

legacy_hooks(HostType) ->
    [{c2s_presence_in, HostType, ?MODULE, c2s_presence_in, 75},
     {c2s_filter_packet, HostType, ?MODULE, c2s_filter_packet, 75},
     {c2s_broadcast_recipients, HostType, ?MODULE, c2s_broadcast_recipients, 75},
     {user_send_packet, HostType, ?MODULE, user_send_packet, 75},
     {user_receive_packet, HostType, ?MODULE, user_receive_packet, 75},
     {c2s_stream_features, HostType, ?MODULE, caps_stream_features, 75},
     {s2s_stream_features, HostType, ?MODULE, caps_stream_features, 75},
     {disco_local_identity, HostType, ?MODULE, disco_local_identity, 1},
     {disco_info, HostType, ?MODULE, disco_info, 1}].

hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 1}].

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

-spec feature_request(mongoose_acc:t(), jid:lserver(), jid:jid(), caps(), [binary()]) ->
    mongoose_acc:t().
feature_request(Acc, LServer, From, Caps, [SubNode | Tail] = SubNodes) ->
    Node = Caps#caps.node,
    NodePair = {Node, SubNode},
    HostType = mongoose_acc:host_type(Acc),
    case cache_tab:lookup(caps_features, NodePair, caps_read_fun(HostType, NodePair)) of
        {ok, Fs} when is_list(Fs) ->
            feature_request(Acc, LServer, From, Caps, Tail);
        Other ->
            NeedRequest = case Other of
                              {ok, TS} -> os:system_time(second) >= TS + (?BAD_HASH_LIFETIME);
                              _ -> true
                          end,
            F = fun (_From, _To, Acc1, IQReply) ->
                        feature_response(Acc1, IQReply, LServer, From, Caps, SubNodes)
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
                    cache_tab:insert(caps_features, NodePair, os:system_time(second),
                                     caps_write_fun(HostType, NodePair, os:system_time(second))),
                    ejabberd_local:route_iq(jid:make_noprep(<<>>, LServer, <<>>), From, Acc, IQ, F),
                    Acc;
               false -> feature_request(Acc, LServer, From, Caps, Tail)
            end
    end;
feature_request(Acc, _LServer, From, Caps, []) ->
    %% feature_request is never executed with empty SubNodes list
    %% so if we end up here, it means the caps are known
    HostType = mongoose_acc:host_type(Acc),
    mongoose_hooks:caps_recognised(Acc, From, self(), get_features_list(HostType, Caps)).

-spec feature_response(mongoose_acc:t(), jlib:iq(), jid:lserver(), jid:jid(), caps(), [binary()]) ->
    mongoose_acc:t().
feature_response(Acc, #iq{type = result, sub_el = [#xmlel{children = Els}]},
                 LServer, From, Caps, [SubNode | SubNodes]) ->
    HostType = mongoose_acc:host_type(Acc),
    NodePair = {Caps#caps.node, SubNode},
    case check_hash(Caps, Els) of
        true ->
            Features = lists:flatmap(fun (#xmlel{name = <<"feature">>,
                                                 attrs = FAttrs}) ->
                                             [xml:get_attr_s(<<"var">>, FAttrs)];
                                         (_) -> []
                                     end,
                                     Els),
            cache_tab:insert(caps_features, NodePair,
                             Features,
                             caps_write_fun(HostType, NodePair, Features));
        false -> ok
    end,
    feature_request(Acc, LServer, From, Caps, SubNodes);
feature_response(Acc, _IQResult, LServer, From, Caps, [_SubNode | SubNodes]) ->
    feature_request(Acc, LServer, From, Caps, SubNodes).

-spec caps_read_fun(mongooseim:host_type(), node_pair()) ->
          fun(() -> {ok, maybe_pending_features()} | error).
caps_read_fun(HostType, Node) ->
    DBType = db_type(HostType),
    caps_read_fun(HostType, Node, DBType).

caps_read_fun(_HostType, Node, mnesia) ->
    fun () ->
            case mnesia:dirty_read({caps_features, Node}) of
                [#caps_features{features = Features}] -> {ok, Features};
                _ -> error
            end
    end.

-spec caps_write_fun(mongooseim:host_type(), node_pair(), maybe_pending_features()) ->
          fun(() -> ok).
caps_write_fun(HostType, Node, Features) ->
    DBType = db_type(HostType),
    caps_write_fun(HostType, Node, Features, DBType).

caps_write_fun(_HostType, Node, Features, mnesia) ->
    fun () ->
            mnesia:dirty_write(#caps_features{node_pair = Node,
                                              features = Features})
    end.

-spec delete_caps(node_pair()) -> ok.
delete_caps(Node) ->
    cache_tab:delete(caps_features, Node, caps_delete_fun(Node)).

-spec caps_delete_fun(node_pair()) -> fun(() -> ok).
caps_delete_fun(Node) ->
    fun () ->
            mnesia:dirty_delete(caps_features, Node)
    end.

-spec make_my_disco_hash(mongooseim:host_type(), jid:lserver()) -> binary().
make_my_disco_hash(HostType, LServer) ->
    JID = jid:make(<<>>, LServer, <<>>),
    case mongoose_disco:get_local_features(HostType, JID, JID, <<>>, <<>>) of
        empty ->
            <<>>;
        {result, FeaturesXML} ->
            IdentityXML = mongoose_disco:get_local_identity(HostType, JID, JID, <<>>, <<>>),
            InfoXML = mongoose_disco:get_info(HostType, mod_disco, <<>>, <<>>),
            make_disco_hash(IdentityXML ++ InfoXML ++ FeaturesXML, sha1)
    end.

-spec make_disco_hash([exml:element()], HashAlgorithm :: atom()) -> binary().
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

is_valid_node(Node) ->
    case mongoose_bin:tokens(Node, <<"#">>) of
        [?MONGOOSE_URI|_] ->
            true;
        _ ->
            false
    end.

db_type(_HostType) ->
    mnesia.
