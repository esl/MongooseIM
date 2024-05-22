-module(mongoose_debug).

%% The most simple use case possible is:
%% - add [modules.mongoose_debug] to mongooseim.toml
%% - from erlang shell, run recon_trace:calls([{mongoose_debug, traffic, '_'}], 100, [{scope, local}]).
%% - watch all the traffic coming in and out

-behaviour(gen_mod).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").

-type debug_entry() :: {client_to_server, jid:jid() | undefined, exml:element()}| {server_to_client, jid:jid(), exml:element()}.
-type direction() :: client_to_server | server_to_client.
-export_type([debug_entry/0, direction/0]).

%% API
-export([start/2, stop/1]).
-export([trace_traffic/3]).
-export([supported_features/0]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(Host, _Opts) ->
    gen_hook:add_handlers(hooks(Host)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(Host) ->
    gen_hook:delete_handlers(hooks(Host)),
    ok.

hooks(_Host) ->
    [{c2s_debug, global, fun ?MODULE:trace_traffic/3, #{}, 50}].


-spec trace_traffic(mongoose_acc:t(), #{arg => debug_entry()}, term()) ->
    {ok, mongoose_acc:t()}.
trace_traffic(Acc, #{arg := {client_to_server, From, El}}, _) ->
    Sfrom = binary_to_list(maybe_jid_to_binary(From)),
    Sto = binary_to_list(get_attr(El, <<"to">>)),
    St = exml:to_binary(El),
    Marker = " C >>>> MiM ",
    traffic(Sfrom, Marker, Sto, St),
    {ok, Acc};
trace_traffic(Acc, #{arg := {server_to_client, To, El}}, _) ->
    Sto = binary_to_list(maybe_jid_to_binary(To)),
    Sfrom = binary_to_list(get_attr(El, <<"from">>)),
    St = exml:to_binary(El),
    Marker = " C <<<< MiM ",
    traffic(Sfrom, Marker, Sto, St),
    {ok, Acc}.

traffic(_Sender, _Marker, _Recipient, _Stanza) -> ok.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

maybe_jid_to_binary(undefined) -> <<" ">>;
maybe_jid_to_binary(J) -> jid:to_binary(J).

get_attr(#xmlstreamstart{attrs = AttrList}, AttrName) ->
    proplists:get_value(AttrName, AttrList, <<" ">>);
get_attr(#xmlstreamend{}, _) ->
    <<" ">>;
get_attr(El, AttrName) ->
    exml_query:attr(El, AttrName, <<" ">>).
