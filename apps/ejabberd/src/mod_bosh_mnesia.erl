-module(mod_bosh_mnesia).

-behaviour(mod_bosh_backend).

%% mod_bosh_backend callbacks
-export([start/1,
         create_session/1,
         delete_session/1,
         get_session/1,
         get_sessions/0]).

-include("mod_bosh.hrl").

start(_Opts) ->
    mnesia:create_table(bosh_session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, bosh_session)}]),
    mnesia:add_table_copy(bosh_session, node(), ram_copies).

%% The choice of the operation context here (transaction vs dirty,
%% see man on mnesia:activity/4 for description of contexts) and the deletion
%% in delete_session/1 below depends on the availability of a load balancer
%% capable of doing server/session affiliation.
%%
%% With affiliation, it suffices for this write to be synchronous, since the
%% client can issue no subsequent request without a session ID and the ID is
%% returned to the client only after the dirty synchronous write returns.
%% Other nodes in the cluster eventually will have the current view of the
%% database, possibly (significantly) later than the write returns. However,
%% the only node serving the client in question always operates on valid data.
%%
%% Without affiliation, each BOSH/HTTP request may be handled by a different
%% node in the cluster. Hence, we must guarantee that once the write
%% operation returns, all nodes in the cluster will have access to currently
%% valid data -- that's why a transaction is used instead of a dirty write.
create_session(#bosh_session{} = Session) ->
    mnesia:sync_transaction(fun mnesia:write/1, [Session]).

-spec delete_session(bosh_sid()) -> any().
delete_session(Sid) ->
    mnesia:transaction(fun mnesia:delete/1, [{bosh_session, Sid}]).

-spec get_session(bosh_sid()) -> [#bosh_session{}].
get_session(Sid) ->
    mnesia:dirty_read(bosh_session, Sid).

-spec get_sessions() -> [#bosh_session{}].
get_sessions() ->
    mnesia:dirty_match_object(mnesia:table_info(bosh_session, wild_pattern)).
