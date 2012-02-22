%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Escalus overrides for ESL fork - specific changes
%%% @end
%%%===================================================================

-module(esl_overrides).

-export([get_remote_sessions/0]).

get_remote_sessions() ->
    escalus_ejabberd:rpc(ejabberd_redis, cmd, [["KEYS", "s3:*"]]).
