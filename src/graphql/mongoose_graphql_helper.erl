-module(mongoose_graphql_helper).

-export([check_user/1]).

-export([format_result/2, make_error/2, make_error/3]).

-include("jlib.hrl").
-include("mongoose_graphql_types.hrl").

-spec format_result(InResult, Context) -> OutResult when
      InResult :: {atom(), iodata() | integer()},
      Context :: map(),
      OutResult :: {ok, binary() | integer()} | {error, resolver_error()}.
format_result(Result, Context) ->
    case Result of
        {ok, Val} when is_integer(Val) -> {ok, Val};
        {ok, Msg} -> {ok, iolist_to_binary(Msg)};
        {ErrCode, Msg} -> make_error(ErrCode, Msg, Context)
    end.

-spec make_error({atom(), iodata()}, map()) -> {error, resolver_error()}.
make_error({Reason, Msg}, Context) ->
    make_error(Reason, Msg, Context).

-spec make_error(atom(), iodata(), map()) -> {error, resolver_error()}.
make_error(Reason, Msg, Context) ->
    {error, #resolver_error{reason = Reason, msg = iolist_to_binary(Msg), context = Context}}.

-spec check_user(jid:jid()) -> {ok, mongooseim:host_type()} | {error, term()}.
check_user(#jid{lserver = LServer} = Jid) ->
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            check_auth(HostType, Jid);
        _ ->
            {error, #{what => unknown_domain, domain => LServer}}
    end.

check_auth(HostType, Jid) ->
   case ejabberd_auth:does_user_exist(HostType, Jid, stored) of
       true ->
           {ok, HostType};
       false ->
           {error, #{what => unknown_user, jid => jid:to_binary(Jid)}}
    end.
