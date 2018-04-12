%%%----------------------------------------------------------------------
%%% File    : mongoose_subhosts.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : SubHost->Host register
%%% Created : 9 Dec 2016
%%%----------------------------------------------------------------------

-module(mongoose_subhosts).

-include("mongoose.hrl").

-export([
         init/0,
         stop/0,

         'register'/2,
         'unregister'/1,
         get_host/1
        ]).

-record(subhost_mapping, {
          subhost :: jid:server(),
          host :: jid:server()
         }).

-define(TAB, subhost_mappings).

%%---------------------------------------------------------------
%% Init & teardown
%%---------------------------------------------------------------

-spec init() -> ok.
init() ->
    ets:new(
      ?TAB, [named_table, public, {read_concurrency, true}, {keypos, #subhost_mapping.subhost}]),
    ok.

-spec stop() -> ok.
stop() ->
    ets:delete(?TAB),
    ok.

%%---------------------------------------------------------------
%% API
%%---------------------------------------------------------------

-spec 'register'(Host :: jid:server(), SubHost :: jid:server()) ->
    ok | {error, exists}.
register(Host, SubHost) ->
    % Hidden subhosts are not supported yet
    ejabberd_hooks:run(register_subhost, [SubHost, false]),
    case ets:insert_new(?TAB, #subhost_mapping{ subhost = SubHost, host = Host }) of
        true -> ok;
        false -> {error, exists}
    end.

-spec 'unregister'(SubHost :: jid:server()) -> true.
unregister(SubHost) ->
    case get_host(SubHost) of
        {ok, _Host} -> ejabberd_hooks:run(unregister_subhost, [SubHost]);
        _ -> ok
    end,
    ets:delete(?TAB, SubHost).

-spec get_host(SubHost :: jid:server()) -> {ok, jid:server()} | undefined.
get_host(SubHost) ->
    case ets:lookup(?TAB, SubHost) of
        [#subhost_mapping{ host = Host }] -> {ok, Host};
        [] -> undefined
    end.
