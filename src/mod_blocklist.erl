-module(mod_blocklist).

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0, instrumentation/1]).

%% Hooks handlers
-export([session_opening_allowed_for_user/3, remove_user/3, remove_domain/3]).

%% API
-export_type([access/0, reason/0]).

-type access() :: allow | deny.
-type reason() :: nonempty_binary() | undefined.

-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_blocklist_backend:init(HostType, Opts),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{anonymous_purge, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {remove_domain, HostType, fun ?MODULE:remove_domain/3, #{}, 50},
     {remove_user, HostType, fun ?MODULE:remove_user/3, #{}, 50},
     {session_opening_allowed_for_user, HostType, fun ?MODULE:session_opening_allowed_for_user/3, #{}, 50}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"backend">> => #option{type = atom, validate = {module, ?MODULE}}},
        defaults = #{<<"backend">> => rdbms}
    }.

-spec supported_features() -> [gen_mod:module_feature()].
supported_features() ->
    [dynamic_domains].

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_blocklist_denied, #{host_type => HostType},
      #{metrics => #{count => spiral}}}].

%% Hook handlers

-spec session_opening_allowed_for_user(Acc, #{jid => jid:jid()}, gen_hook:extra()) -> gen_hook:hook_fn_ret(Acc)
    when Acc :: access().
session_opening_allowed_for_user(Acc, #{jid := JID}, #{host_type := HostType}) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    case mod_blocklist_backend:get_block(HostType, LUser, LServer) of
        {ok, _} ->
            mongoose_instrument:execute(mod_blocklist_denied, #{host_type => HostType}, #{count => 1}),
            {stop, deny};
        not_found ->
            {ok, Acc}
    end.

-spec remove_user(Acc, #{jid := jid:jid()}, gen_hook:extra()) -> gen_hook:hook_fn_ret(Acc)
    when Acc :: mongoose_acc:t().
remove_user(Acc, #{jid := JID}, #{host_type := HostType}) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    mod_blocklist_backend:remove_block(HostType, LUser, LServer),
    {ok, Acc}.

-spec remove_domain(Acc, #{domain := jid:lserver()}, gen_hook:extra()) -> gen_hook:hook_fn_ret(Acc)
    when Acc :: mongoose_acc:t().
remove_domain(Acc, #{domain := Domain}, #{host_type := HostType}) ->
    mod_blocklist_backend:remove_domain(HostType, Domain),
    {ok, Acc}.

