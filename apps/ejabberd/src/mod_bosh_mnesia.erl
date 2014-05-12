-module(mod_bosh_mnesia).

-behaviour(mod_bosh_backend).

%% mod_bosh_backend callbacks
-export([start/1,
         create_session/1,
         delete_session/1,
         get_session/1,
         get_sessions/0]).

-include("mod_bosh.hrl").

-spec start(list()) -> any().
start(_Opts) ->
    mnesia:create_table(bosh_session,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, bosh_session)}]),
    mnesia:add_table_copy(bosh_session, node(), ram_copies).


-spec create_session(mod_bosh:session()) -> any().
create_session(#bosh_session{} = Session) ->
    mnesia:sync_dirty(fun mnesia:write/1, [Session]).


-spec delete_session(mod_bosh:sid()) -> any().
delete_session(Sid) ->
    mnesia:async_dirty(fun mnesia:delete/1, [{bosh_session, Sid}]).


-spec get_session(mod_bosh:sid()) -> [mod_bosh:session()].
get_session(Sid) ->
    mnesia:dirty_read(bosh_session, Sid).


-spec get_sessions() -> [mod_bosh:session()].
get_sessions() ->
    mnesia:dirty_match_object(mnesia:table_info(bosh_session, wild_pattern)).
