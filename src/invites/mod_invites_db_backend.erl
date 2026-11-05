%%%----------------------------------------------------------------------
%%% File    : mod_invites_db_backend.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Invites DB behaviour
%%% Created : 13 July 2016 by Stefan Strigler <stefan@strigler.de>
%%%----------------------------------------------------------------------

-module(mod_invites_db_backend).

-author('stefan@strigler.de').

-define(MAIN_MODULE, mod_invites_db).

-export([start/2, stop/1]).

-callback start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback stop(mongooseim:host_type()) -> ok.

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    TrackedFuns = [],
    mongoose_backend:init(HostType, ?MAIN_MODULE, TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Args = [HostType],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
