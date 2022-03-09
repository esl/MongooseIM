%%% @doc A proxy interface module between the main mod_global_distrib_mapping module
%%% and the backend modules.
%%% There is only one backend implementation (redis), so the backend module is global.
-module(mod_global_distrib_mapping_backend).

-export([start/1,
         stop/0,
         put_session/1,
         get_session/1,
         delete_session/1,
         put_domain/2,
         get_domain/1,
         delete_domain/1,
         get_domains/0,
         get_public_domains/0,
         get_endpoints/1,
         get_hosts/0]).

-define(MAIN_MODULE, mod_global_distrib_mapping).

-type endpoint() :: mod_global_distrib_utils:endpoint().

%% Callbacks

-callback start(gen_mod:module_opts()) -> any().
-callback stop() -> any().

%% Mapping from a JID to the local host
-callback put_session(jid:literal_jid()) -> ok.
-callback get_session(jid:literal_jid()) -> {ok, jid:lserver()} | error.
-callback delete_session(jid:literal_jid()) -> ok.

%% Mapping from a domain to the local host
-callback put_domain(jid:lserver(), IsHidden :: boolean()) -> ok.
-callback get_domain(jid:lserver()) -> {ok, jid:lserver()} | error.
-callback delete_domain(jid:lserver()) -> ok.

-callback get_domains() -> [jid:lserver()].
-callback get_public_domains() -> [jid:lserver()].
-callback get_endpoints(jid:lserver()) -> [endpoint()].
-callback get_hosts() -> [jid:lserver()].

%% API Functions

-spec start(gen_mod:module_opts()) -> any().
start(Opts) ->
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Opts]).

-spec stop() -> any().
stop() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec put_session(jid:literal_jid()) -> ok.
put_session(JID) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [JID]).

-spec get_session(jid:literal_jid()) -> {ok, jid:lserver()} | error.
get_session(JID) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [JID]).

-spec delete_session(jid:literal_jid()) -> ok.
delete_session(JID) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [JID]).

-spec put_domain(jid:lserver(), boolean()) -> ok.
put_domain(Domain, IsHidden) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Domain, IsHidden]).

-spec get_domain(jid:lserver()) -> {ok, jid:lserver()} | error.
get_domain(Domain) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Domain]).

-spec delete_domain(jid:lserver()) -> ok.
delete_domain(Domain) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Domain]).

-spec get_domains() -> [jid:lserver()].
get_domains() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec get_public_domains() -> [jid:lserver()].
get_public_domains() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).

-spec get_endpoints(jid:lserver()) -> [endpoint()].
get_endpoints(Domain) ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, [Domain]).

-spec get_hosts() -> [jid:lserver()].
get_hosts() ->
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, []).
