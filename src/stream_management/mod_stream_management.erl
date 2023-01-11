-module(mod_stream_management).
-xep([{xep, 198}, {version, "1.6"}]).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% `gen_mod' callbacks
-export([start/2,
         stop/1,
         config_spec/0,
         supported_features/0,
         process_buffer_and_ack/1]).

%% hooks handlers
-export([c2s_stream_features/3,
         remove_smid/3,
         session_cleanup/3]).

%% API for `ejabberd_c2s'
-export([make_smid/0,
         get_session_from_smid/2,
         get_buffer_max/1,
         get_ack_freq/1,
         get_resume_timeout/1,
         register_smid/3]).

%% API for inspection and tests
-export([get_sid/2,
         get_stale_h/2,
         register_stale_smid_h/3,
         remove_stale_smid_h/2]).

-ignore_xref([get_sid/2,
              get_stale_h/2,
              register_stale_smid_h/3,
              remove_stale_smid_h/2]).

%% Type base64:ascii_binary() is not exported
-type smid() :: binary().

-export_type([smid/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-type buffer_max() :: pos_integer() | infinity | no_buffer.
-type ack_freq() :: pos_integer() | never.
%%
%% `gen_mod' callbacks
%%

start(HostType, Opts) ->
    mod_stream_management_backend:init(HostType, Opts),
    ?LOG_INFO(#{what => stream_management_starting}),
    gen_hook:add_handlers(hooks(HostType)),
    ok.

stop(HostType) ->
    ?LOG_INFO(#{what => stream_management_stopping}),
    gen_hook:delete_handlers(hooks(HostType)),
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{sm_remove_connection_hook, HostType, fun ?MODULE:remove_smid/3, #{}, 50},
     {c2s_stream_features, HostType, fun ?MODULE:c2s_stream_features/3, #{}, 50},
     {session_cleanup, HostType, fun ?MODULE:session_cleanup/3, #{}, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"backend">> => #option{type = atom, validate = {module, ?MODULE}},
                  <<"buffer">> => #option{type = boolean},
                  <<"buffer_max">> => #option{type = int_or_infinity,
                                              validate = positive},
                  <<"ack">> => #option{type = boolean},
                  <<"ack_freq">> => #option{type = integer,
                                            validate = positive},
                  <<"resume_timeout">> => #option{type = integer,
                                                  validate = positive},
                  <<"stale_h">> => stale_h_config_spec()
                 },
        process = fun ?MODULE:process_buffer_and_ack/1,
        defaults = #{<<"backend">> => mnesia,
                     <<"buffer">> => true,
                     <<"buffer_max">> => 100,
                     <<"ack">> => true,
                     <<"ack_freq">> => 1,
                     <<"resume_timeout">> => 600 % seconds
        }
      }.

supported_features() -> [dynamic_domains].

process_buffer_and_ack(Opts = #{buffer := Buffer, ack := Ack}) ->
    OptsWithBuffer = check_buffer(Buffer, Opts),
    check_ack(Ack, OptsWithBuffer).

check_buffer(false, Opts) ->
    Opts#{buffer_max => no_buffer};
check_buffer(_, Opts) ->
    Opts.

check_ack(false, Opts) ->
    Opts#{ack_freq => never};
check_ack(_, Opts) ->
    Opts.

stale_h_config_spec() ->
    #section{
        items = #{<<"enabled">> => #option{type = boolean},
                  <<"repeat_after">> => #option{type = integer,
                                                validate = positive},
                  <<"geriatric">> => #option{type = integer,
                                             validate = positive}},
        include = always,
        defaults = #{<<"enabled">> => false,
                     <<"repeat_after">> => 1800, % seconds
                     <<"geriatric">> => 3600 % seconds
        }
    }.

%%
%% hooks handlers
%%

-spec c2s_stream_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [exml:element()],
    Params :: map(),
    Extra :: gen_hook:extra().
c2s_stream_features(Acc, _, _) ->
    {ok, lists:keystore(<<"sm">>, #xmlel.name, Acc, sm())}.

sm() ->
    #xmlel{name = <<"sm">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

-spec remove_smid(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{sid := ejabberd_sm:sid()},
    Extra :: gen_hook:extra().
remove_smid(Acc, #{sid := SID}, #{host_type := HostType}) ->
    {ok, do_remove_smid(Acc, HostType, SID)}.

-spec session_cleanup(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{sid := ejabberd_sm:sid()},
    Extra :: gen_hook:extra().
session_cleanup(Acc, #{sid := SID}, #{host_type := HostType}) ->
    HostType = mongoose_acc:host_type(Acc),
    {ok, do_remove_smid(Acc, HostType, SID)}.

-spec do_remove_smid(mongoose_acc:t(), mongooseim:host_type(), ejabberd_sm:sid()) ->
    mongoose_acc:t().
do_remove_smid(Acc, HostType, SID) ->
    H = mongoose_acc:get(stream_mgmt, h, undefined, Acc),
    MaybeSMID = unregister_smid(HostType, SID),
    case MaybeSMID of
        {ok, SMID} when H =/= undefined ->
            register_stale_smid_h(HostType, SMID, H);
        _ ->
            ok
    end,
    mongoose_acc:set(stream_mgmt, smid, MaybeSMID, Acc).

%%
%% API for `ejabberd_c2s'
%%

-spec make_smid() -> smid().
make_smid() ->
    base64:encode(crypto:strong_rand_bytes(21)).

%% Getters
-spec get_session_from_smid(mongooseim:host_type(), smid()) ->
    {sid, ejabberd_sm:sid()} | {stale_h, non_neg_integer()} | {error, smid_not_found}.
get_session_from_smid(HostType, SMID) ->
    case get_sid(HostType, SMID) of
        {sid, SID} -> {sid, SID};
        {error, smid_not_found} -> get_stale_h(HostType, SMID)
    end.

-spec get_stale_h(mongooseim:host_type(), SMID :: smid()) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found}.
get_stale_h(HostType, SMID) ->
    case is_stale_h_enabled(HostType) of
        false -> {error, smid_not_found};
        true -> read_stale_h(HostType, SMID)
    end.

-spec get_buffer_max(mongooseim:host_type()) -> buffer_max().
get_buffer_max(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, buffer_max).

-spec get_ack_freq(mongooseim:host_type()) -> ack_freq().
get_ack_freq(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, ack_freq).

-spec get_resume_timeout(mongooseim:host_type()) -> pos_integer().
get_resume_timeout(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, resume_timeout).


register_stale_smid_h(HostType, SMID, H) ->
    case is_stale_h_enabled(HostType) of
        false -> ok;
        true -> write_stale_h(HostType, SMID, H)
    end.

remove_stale_smid_h(HostType, SMID) ->
    case is_stale_h_enabled(HostType) of
        false -> ok;
        true -> delete_stale_h(HostType, SMID)
    end.

is_stale_h_enabled(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, [stale_h, enabled]).

%% Backend operations

-spec register_smid(HostType, SMID, SID) ->
    ok | {error, term()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    SID :: ejabberd_sm:sid().
register_smid(HostType, SMID, SID) ->
    mod_stream_management_backend:register_smid(HostType, SMID, SID).

-spec unregister_smid(mongooseim:host_type(), ejabberd_sm:sid()) ->
    {ok, SMID :: mod_stream_management:smid()} | {error, smid_not_found}.
unregister_smid(HostType, SID) ->
    mod_stream_management_backend:unregister_smid(HostType, SID).

-spec get_sid(mongooseim:host_type(), mod_stream_management:smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.
get_sid(HostType, SMID) ->
    mod_stream_management_backend:get_sid(HostType, SMID).

%% stale_h

-spec write_stale_h(HostType, SMID, H) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid(),
    H :: non_neg_integer().
write_stale_h(HostType, SMID, H) ->
    mod_stream_management_backend:write_stale_h(HostType, SMID, H).

-spec delete_stale_h(HostType, SMID) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid().
delete_stale_h(HostType, SMID) ->
    mod_stream_management_backend:delete_stale_h(HostType, SMID).

-spec read_stale_h(HostType, SMID) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found} when
    HostType :: mongooseim:host_type(),
    SMID :: mod_stream_management:smid().
read_stale_h(HostType, SMID) ->
    mod_stream_management_backend:read_stale_h(HostType, SMID).
