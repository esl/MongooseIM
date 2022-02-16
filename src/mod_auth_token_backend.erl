%%%----------------------------------------------------------------------
%%% @copyright 2021, Erlang Solutions Ltd.
%%% @doc A proxy interface module between the main mod_auth_token
%%% module and the backend modules.
%%% @end
%%%----------------------------------------------------------------------
-module(mod_auth_token_backend).
-export([start/2,
         revoke/2,
         get_valid_sequence_number/2,
         clean_tokens/2]).

-define(MAIN_MODULE, mod_auth_token).

%% ----------------------------------------------------------------------
%% Callbacks

-callback start(mongooseim:host_type()) -> ok.

-callback revoke(mongooseim:host_type(), jid:jid()) -> ok | not_found.

-callback get_valid_sequence_number(mongooseim:host_type(), jid:jid()) -> integer().

-callback clean_tokens(mongooseim:host_type(), jid:jid()) -> ok.

%% ----------------------------------------------------------------------
%% API Functions

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mongoose_backend:init(HostType, ?MAIN_MODULE, [], Opts),
    Args = [HostType],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec revoke(mongooseim:host_type(), jid:jid()) -> ok | not_found.
revoke(HostType, JID) ->
    Args = [HostType, JID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec get_valid_sequence_number(mongooseim:host_type(), jid:jid()) -> integer().
get_valid_sequence_number(HostType, JID) ->
    Args = [HostType, JID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).

-spec clean_tokens(mongooseim:host_type(), jid:jid()) -> ok.
clean_tokens(HostType, JID) ->
    Args = [HostType, JID],
    mongoose_backend:call(HostType, ?MAIN_MODULE, ?FUNCTION_NAME, Args).
