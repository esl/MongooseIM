-module(mongoose_credentials).

-export([make_opts/1,
         new/3,
         lserver/1,
         host_type/1,
         auth_modules/1,
         get/2, get/3,
         set/3,
         extend/2,
         register/3]).

-export_type([t/0, opts/0]).

-record(mongoose_credentials, {lserver, host_type, registry = [], extra = [], modules}).

-type auth_event() :: any().

-opaque t() ::
    #mongoose_credentials{ %% These values are always present.
                           lserver :: jid:lserver(),
                           host_type :: mongooseim:host_type(),
                           %% Authorization success / failure registry.
                           registry :: [{ejabberd_gen_auth:t(), auth_event()}],
                           %% These values are dependent on the ejabberd_auth backend in use.
                           %% Each backend may require different values to be present.
                           extra :: [proplists:property()],
                           modules :: [ejabberd_auth:authmodule()] }.

-type opts() :: #{}.

-spec make_opts(mongoose_listener:options()) -> opts().
make_opts(#{allowed_auth_methods := Methods}) ->
    #{allowed_modules => ejabberd_auth:methods_to_modules(Methods)};
make_opts(#{}) ->
    #{}.

-spec new(jid:lserver(), binary(), opts()) -> mongoose_credentials:t().
new(LServer, HostType, Opts) when is_binary(LServer), is_binary(HostType) ->
    HostTypeModules = ejabberd_auth:auth_modules_for_host_type(HostType),
    Modules = filter_modules(HostTypeModules, Opts),
    #mongoose_credentials{lserver = LServer, host_type = HostType,
                          modules = Modules}.

%% Allows to enable only some modules for some listeners
-spec filter_modules([ejabberd_auth:authmodule()], opts()) ->
    [ejabberd_auth:authmodule()].
filter_modules(Modules, #{allowed_modules := AllowedModules}) ->
    [M || M <- Modules, lists:member(M, AllowedModules)];
filter_modules(Modules, _) ->
    Modules.

-spec host_type(t()) -> mongooseim:host_type().
host_type(#mongoose_credentials{host_type = HostType}) -> HostType.

-spec lserver(t()) -> jid:lserver().
lserver(#mongoose_credentials{lserver = S}) -> S.

-spec auth_modules(t()) -> [ejabberd_auth:authmodule()].
auth_modules(#mongoose_credentials{modules = Modules}) -> Modules.

%% @doc Calls erlang:error/2 when Key is not found!
-spec get(t(), Key) -> Value when
      Key :: any(),
      Value :: any().
get(#mongoose_credentials{extra = Extra} = C, Key) ->
    case lists:keyfind(Key, 1, Extra) of
        false -> error({not_found, Key}, [C, Key]);
        {Key, Value} -> Value
    end.

%% @doc Returns Default when Key is not found.
-spec get(t(), Key, Default) -> Value when
      Key :: any(),
      Default :: any(),
      Value :: any().
get(#mongoose_credentials{extra = Extra}, Key, Default) ->
    case lists:keyfind(Key, 1, Extra) of
        false -> Default;
        {Key, Value} -> Value
    end.

-spec set(t(), Key, Value) -> t() when
      Key :: any(),
      Value :: any().
set(#mongoose_credentials{extra = Extra} = C, Key, Value) ->
    NewExtra = lists:keystore(Key, 1, Extra, {Key, Value}),
    C#mongoose_credentials{extra = NewExtra}.

-spec extend(t(), [{Key, Value}]) -> t() when
      Key :: any(),
      Value :: any().
extend(#mongoose_credentials{} = C, KVPairs) ->
    lists:foldl(fun ({K, V}, Creds) ->
                        ?MODULE:set(Creds, K, V)
                end, C, KVPairs).

-spec register(t(), ejabberd_gen_auth:t(), auth_event()) -> t().
register(#mongoose_credentials{} = C, Mod, Event) ->
    #mongoose_credentials{registry = R} = C,
    C#mongoose_credentials{registry = [{Mod, Event} | R]}.
