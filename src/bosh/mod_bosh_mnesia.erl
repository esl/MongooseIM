-module(mod_bosh_mnesia).

-behaviour(mod_bosh_backend).

%% mod_bosh_backend callbacks
-export([start/1,
         create_session/2,
         delete_session/2,
         get_session/2,
         get_sessions/1,
         node_cleanup/2]).

-include("mod_bosh.hrl").

-spec start(mongooseim:host_type()) -> any().
start(HostType) ->
    mongoose_mnesia:create_table(table_name(HostType),
        [{ram_copies, [node()]},
         {attributes, record_info(fields, bosh_session)}]).

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

-spec create_session(mongooseim:host_type(), mod_bosh:session()) -> any().
create_session(HostType, #bosh_session{} = Session) ->
    mnesia:sync_transaction(fun mnesia:write/3, [table_name(HostType), Session, write]).

-spec delete_session(mongooseim:host_type(), mod_bosh:sid()) -> any().
delete_session(HostType, Sid) ->
    mnesia:transaction(fun mnesia:delete/3, [table_name(HostType), {bosh_session, Sid}, write]).

-spec get_session(mongooseim:host_type(), mod_bosh:sid()) -> [mod_bosh:session()].
get_session(HostType, Sid) ->
    mnesia:dirty_read(table_name(HostType), Sid).

-spec get_sessions(mongooseim:host_type()) -> [mod_bosh:session()].
get_sessions(HostType) ->
    mnesia:dirty_match_object(mnesia:table_info(table_name(HostType), wild_pattern)).

-spec node_cleanup(mongooseim:host_type(), atom()) -> any().
node_cleanup(HostType, Node) ->
    Table = table_name(HostType),
    F = fun() ->
                SIDs = mnesia:select(
                       bosh_session,
                       [{#bosh_session{sid = '$1', socket = '$2'},
                         [{'==', {node, '$2'}, Node}],
                         ['$1']}]),
                lists:foreach(fun(Sid) ->
                                      mnesia:delete({Table, Sid})
                              end, SIDs)
        end,
    mnesia:async_dirty(F).


%% Helpers

-spec table_name(mongooseim:host_type()) -> atom().
table_name(HostType) ->
    binary_to_atom(<<"mod_bosh_mnesia_", HostType/binary>>).
