%% @doc Backend module for mod_jingle_sip
-module(mod_jingle_sip_backend).

-export([init/2]).
-export([read_session/1]).
-export([write_new_session/2]).
-export([update_session/2]).
-export([remove_session/1]).

-include("mongoose.hrl").

-type call_id() :: mod_jingle_sip_session:call_id().
-type session() :: mod_jingle_sip_session:session().
-type update_fun() :: mod_jingle_sip_session:update_fun().

-define(MAIN_MODULE, mod_jingle_sip).

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback read_session(call_id()) -> [session()].

-callback write_new_session(call_id(), session()) ->
    ok | {error, conflict}.

-callback update_session(call_id(), update_fun()) -> ok | {error, _}.

-callback remove_session(call_id()) -> ok.

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(Host, Opts) ->
    Args = [Host, Opts],
    mongoose_backend:init(global, ?MAIN_MODULE, [], Opts),
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec read_session(call_id()) -> [session()].
read_session(CallID) ->
    Args = [CallID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec write_new_session(call_id(), session()) ->
    ok | {error, conflict}.
write_new_session(CallID, Session) ->
    Args = [CallID, Session],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec update_session(call_id(), update_fun()) -> ok | {error, _}.
update_session(CallID, F) ->
    Args = [CallID, F],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec remove_session(call_id()) -> ok.
remove_session(CallID) ->
    Args = [CallID],
    mongoose_backend:call(global, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
