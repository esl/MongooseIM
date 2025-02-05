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

-xep([{xep, 115}, {version, "1.6.0"}]).

-behaviour(gen_server).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-export([read_caps/1, caps_stream_features/3,
         disco_local_features/3, disco_local_identity/3, disco_info/3]).

%% gen_mod callbacks
-export([start/2, start_link/2, stop/1, config_spec/0, supported_features/0]).

%% gen_server callbacks
-export([init/1, handle_info/2, handle_call/3,
         handle_cast/2, terminate/2, code_change/3]).

-export([user_send_presence/3,
         user_receive_presence/3,
         get_pep_recipients/3,
         filter_pep_recipient/3]).

%% for test cases
-export([delete_caps/2, make_disco_hash/2]).
-ignore_xref([delete_caps/2, make_disco_hash/2, read_caps/1, start_link/2]).

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

-export_type([caps/0, node_pair/0, maybe_pending_features/0]).

-type features() :: [binary()].
-type maybe_pending_features() :: features() | pos_integer().
-type node_pair() :: {Node :: binary(), SubNode :: binary()}.

-record(state, {host_type :: mongooseim:host_type()}).

-type state() :: #state{}.

-spec start_link(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start_link(HostType, Opts) ->
    mod_caps_backend:init(HostType, Opts),
    Proc = gen_mod:get_module_proc(HostType, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [HostType, Opts], [{hibernate_after, 0}]).

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
                                                   validate = positive},
                  <<"backend">> => #option{type = atom,
                                           validate = {module, ?MODULE}}
                 },
        defaults = #{<<"cache_size">> => 1000,
                     <<"cache_life_time">> => timer:hours(24) div 1000,
                     <<"backend">> => mnesia}
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

read_caps([#xmlel{name = <<"c">>} = Element | Tail], Result) ->
    case exml_query:attr(Element, <<"xmlns">>, <<>>) of
        ?NS_CAPS ->
            Node = exml_query:attr(Element, <<"node">>, <<>>),
            Version = exml_query:attr(Element, <<"ver">>, <<>>),
            Hash = exml_query:attr(Element, <<"hash">>, <<>>),
            Exts = mongoose_bin:tokens(exml_query:attr(Element, <<"ext">>, <<>>), <<" ">>),
            read_caps(Tail, #caps{node = Node, hash = Hash, version = Version, exts = Exts});
        _ -> read_caps(Tail, Result)
    end;
read_caps([#xmlel{name = <<"x">>} = Element | Tail], Result) ->
    case exml_query:attr(Element, <<"xmlns">>, <<>>) of
        ?NS_MUC_USER -> nothing;
        _ -> read_caps(Tail, Result)
    end;
read_caps([_ | Tail], Result) ->
    read_caps(Tail, Result);
read_caps([], Result) -> Result.

-spec user_send_presence(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: map().
user_send_presence(Acc, _, _) ->
    {From, To, Packet} = mongoose_acc:packet(Acc),
    {ok, user_send_presence(Acc, From, To, Packet)}.

-spec user_send_presence(mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
user_send_presence(Acc,
                   #jid{luser = User, lserver = LServer} = From,
                   #jid{luser = User, lserver = LServer, lresource = <<>>},
                   #xmlel{children = Elements} = Element) ->
    Type = exml_query:attr(Element, <<"type">>, <<>>),
    handle_presence(Acc, LServer, From, Type, Elements);
user_send_presence(Acc, _, _, _) ->
    Acc.

-spec caps_stream_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [exml:element()],
    Params :: #{lserver := jid:lserver()},
    Extra :: #{host_type := mongooseim:host_type()}.
caps_stream_features(Acc, #{lserver := LServer}, #{host_type := HostType}) ->
    NewAcc = case make_my_disco_hash(HostType, LServer) of
        <<>> ->
            Acc;
        Hash ->
            [#xmlel{name = <<"c">>,
                    attrs = #{<<"xmlns">> => ?NS_CAPS,
                              <<"hash">> => <<"sha-1">>,
                              <<"node">> => ?MONGOOSE_URI,
                              <<"ver">> => Hash},
                    children = []}
             | Acc]
    end,
    {ok, NewAcc}.

-spec disco_local_features(mongoose_disco:feature_acc(),
                           map(),
                           map()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{node := Node}, _, _) ->
    NewAcc = case is_valid_node(Node) of
        true -> Acc#{node := <<>>};
        false -> Acc
    end,
    {ok, NewAcc}.

-spec disco_local_identity(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:identity_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_local_identity(Acc = #{node := Node}, _, _) ->
    NewAcc = case is_valid_node(Node) of
        true -> Acc#{node := <<>>};
        false -> Acc
    end,
    {ok, NewAcc}.

-spec disco_info(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_disco:identity_acc(),
    Params :: map(),
    Extra :: gen_hook:extra().
disco_info(Acc = #{node := Node}, _, _) ->
    NewAcc = case is_valid_node(Node) of
        true -> Acc#{node := <<>>};
        false -> Acc
    end,
    {ok, NewAcc}.

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

-spec user_receive_presence(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_receive_presence(Acc0, #{c2s_data := C2SData}, _Extra) ->
    {From, To, #xmlel{children = Els} = Packet} = mongoose_acc:packet(Acc0),
    ?LOG_DEBUG(#{what => user_receive_presence,
                 to => jid:to_binary(To), from => jid:to_binary(From),
                 exml_packet => Packet, c2s_state => C2SData}),
    Type = exml_query:attr(Packet, <<"type">>, <<>>),
    #jid{lserver = LServer} = mongoose_c2s:get_jid(C2SData),
    Acc = case mongoose_domain_api:get_host_type(From#jid.lserver) of
              {error, not_found} ->
                  handle_presence(Acc0, LServer, From, Type, Els);
              {ok, _} ->
                  Acc0 %% it was already handled in 'user_send_presence'
          end,
    case mongoose_c2s:get_mod_state(C2SData, mod_presence) of
        {ok, Presences} ->
            Subscription = get_subscription(From, Presences),
            Insert = (Type == <<>> orelse Type == <<"available">>)
                and (Subscription == both orelse Subscription == to),
            Delete = Type == <<"unavailable">> orelse Type == <<"error">>,
            case Insert orelse Delete of
                true ->
                    LFrom = jid:to_lower(From),
                    Rs = case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
                            {ok, Rs1} -> Rs1;
                            {error, not_found} -> gb_trees:empty()
                        end,
                    Caps = read_caps(Els),
                    NewRs = case Caps of
                                nothing when Insert == true ->
                                    Rs;
                                _ when Insert == true ->
                                    ?LOG_DEBUG(#{what => caps_set_caps,
                                                 caps => Caps,
                                                 to => jid:to_binary(To),
                                                 from => jid:to_binary(From),
                                                 exml_packet => Packet,
                                                 c2s_state => C2SData}),
                                    upsert_caps(LFrom, Caps, Rs);
                                _ ->
                                    gb_trees:delete_any(LFrom, Rs)
                            end,
                    {ok, mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewRs})};
                false ->
                    {ok, Acc}
            end;
        {error, not_found} ->
            {ok, Acc}
    end.

get_subscription(From, Presences) ->
    BareFrom = jid:to_bare(From),
    F = mod_presence:is_subscribed_to_my_presence(From, BareFrom, Presences),
    T = mod_presence:am_i_subscribed_to_presence(From, BareFrom, Presences),
    case {F, T} of
        {true, true} -> both;
        {true, false} -> from;
        {false, true} -> to;
        {false, false} -> none
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

-spec get_pep_recipients(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [jid:simple_jid()],
    Params :: #{c2s_data := mongoose_c2s:data(), feature := binary()},
    Extra :: map().
get_pep_recipients(InAcc, #{c2s_data := C2SData, feature := Feature}, _) ->
    HostType = mongoose_c2s:get_host_type(C2SData),
    NewAcc = case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
                 {ok, Rs} ->
                     filter_recipients_by_caps(HostType, InAcc, Feature, Rs);
                 _ -> InAcc
             end,
    {ok, NewAcc};
get_pep_recipients(Acc, _, _) -> {ok, Acc}.

-spec filter_recipients_by_caps(mongooseim:host_type(), Acc, binary(), caps_resources()) -> Acc
    when Acc :: [jid:simple_jid()].
filter_recipients_by_caps(HostType, InAcc, Feature, Rs) ->
    gb_trees_fold(fun(USR, Caps, Acc) ->
                          case lists:member(Feature, get_features_list(HostType, Caps)) of
                              true -> [USR | Acc];
                              false -> Acc
                          end
                  end,
                  InAcc, Rs).

-spec filter_pep_recipient(Acc, Params, Extra) -> {ok | stop, Acc} when
      Acc :: boolean(),
      Params :: #{c2s_data := mongoose_c2s:data(), feature := binary(), to := jid:jid()},
      Extra :: gen_hook:extra().
filter_pep_recipient(InAcc, #{c2s_data := C2SData, feature := Feature, to := To}, _) ->
    case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
        {ok, Rs} ->
            ?LOG_DEBUG(#{what => caps_lookup, text => <<"Look for CAPS for To jid">>,
                         acc => InAcc, c2s_state => C2SData, caps_resources => Rs}),
            LTo = jid:to_lower(To),
            case gb_trees:lookup(LTo, Rs) of
                {value, Caps} ->
                    HostType = mongoose_c2s:get_host_type(C2SData),
                    Drop = not lists:member(Feature, get_features_list(HostType, Caps)),
                    {stop, Drop};
                none ->
                    {stop, true}
            end;
        _ -> {ok, InAcc}
    end.

-spec init(list()) -> {ok, state()}.
init([HostType, #{cache_size := MaxSize, cache_life_time := LifeTime}]) ->
    cache_tab:new(caps_features, [{max_size, MaxSize}, {life_time, LifeTime}]),
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
    gen_hook:delete_handlers(hooks(HostType)).

hooks(HostType) ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 1},
     {get_pep_recipients, HostType, fun ?MODULE:get_pep_recipients/3, #{}, 75},
     {filter_pep_recipient, HostType, fun ?MODULE:filter_pep_recipient/3, #{}, 75},
     {user_send_presence, HostType, fun ?MODULE:user_send_presence/3, #{}, 75},
     {user_receive_presence, HostType, fun ?MODULE:user_receive_presence/3, #{}, 1},
     {c2s_stream_features, HostType, fun ?MODULE:caps_stream_features/3, #{}, 75},
     {s2s_stream_features, HostType, fun ?MODULE:caps_stream_features/3, #{}, 75},
     {disco_local_identity, HostType, fun ?MODULE:disco_local_identity/3, #{}, 1},
     {disco_info, HostType, fun ?MODULE:disco_info/3, #{}, 1}
    ].

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
            F = fun (_From, _To, _Acc1, timeout) ->
                        %% IQ request timed out, skip this node
                        feature_request(Acc, LServer, From, Caps, Tail);
                    (_From, _To, Acc1, IQReply) ->
                        feature_response(Acc1, IQReply, LServer, From, Caps, SubNodes)
                end,
            case NeedRequest of
                true ->
                    IQ = #iq{type = get, xmlns = ?NS_DISCO_INFO,
                             sub_el =
                                 [#xmlel{name = <<"query">>,
                                         attrs =
                                             #{<<"xmlns">> => ?NS_DISCO_INFO,
                                               <<"node">> => <<Node/binary, "#", SubNode/binary>>},
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
            Features = lists:flatmap(fun (#xmlel{name = <<"feature">>} = El) ->
                                             [exml_query:attr(El, <<"var">>, <<>>)];
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
    fun () ->
            mod_caps_backend:read(HostType, Node)
    end.

-spec caps_write_fun(mongooseim:host_type(), node_pair(), maybe_pending_features()) ->
          fun(() -> ok).
caps_write_fun(HostType, Node, Features) ->
    fun () ->
            mod_caps_backend:write(HostType, Node, Features)
    end.

-spec delete_caps(mongooseim:host_type(), node_pair()) -> ok.
delete_caps(HostType, Node) ->
    cache_tab:delete(caps_features, Node, caps_delete_fun(HostType, Node)).

-spec caps_delete_fun(mongooseim:host_type(), node_pair()) -> fun(() -> ok).
caps_delete_fun(HostType, Node) ->
    fun () ->
            mod_caps_backend:delete_node(HostType, Node)
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
    base64:encode(case Algo of
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
    lists:usort(lists:flatmap(fun (#xmlel{name = <<"feature">>} = Element) ->
                                      [[exml_query:attr(Element, <<"var">>, <<>>), $<]];
                                  (_) -> []
                              end,
                              Els)).

concat_identities(Els) ->
    lists:sort(lists:flatmap(fun (#xmlel{name = <<"identity">>} = Element) ->
                                     [[exml_query:attr(Element, <<"category">>, <<>>),
                                       $/, exml_query:attr(Element, <<"type">>, <<>>),
                                       $/,
                                       exml_query:attr(Element, <<"xml:lang">>, <<>>),
                                       $/, exml_query:attr(Element, <<"name">>, <<>>),
                                       $<]];
                                 (_) -> []
                             end,
                             Els)).

concat_info(Els) ->
    lists:sort(lists:flatmap(fun(El) ->
                                     concat_xdata_fields(mongoose_data_forms:parse_form(El))
                             end,
                             Els)).

concat_xdata_fields(#{type := <<"result">>, kvs := KVs, ns := NS}) ->
    Res = maps:fold(fun(Var, Values, VarFields) ->
                            NewField = [[V, $<] || V <- [Var | lists:sort(Values)]],
                            [NewField | VarFields]
                    end, [], KVs),
    [[NS, $<, lists:sort(Res)]];
concat_xdata_fields(_) ->
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
