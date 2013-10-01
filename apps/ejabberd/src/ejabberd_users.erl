%% @doc Stores info about non-anonymous users using Mnesia table.
-module(ejabberd_users).
%% API
-export([start/1,
         stop/1,
         is_user_exists/2]).

%% Hooks.
-export([remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").

%%====================================================================
%% API
%%====================================================================

-record(cached_user, {host_name, xxx}).

start(Host) ->
    mnesia:create_table(cached_user,
            [{ram_copies, [node()]},
             {type, set},
             {attributes, record_info(fields, cached_user)}]),
    mnesia:add_table_copy(node, node(), ram_copies),
    ejabberd_hooks:add(remove_user, Host, ?MODULE, remove_user, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(remove_user, Host, ?MODULE, remove_user, 50),
    ok.

is_user_exists(LUser, LServer) ->
    case is_cached_user_exists(LUser, LServer) of
        true -> true;
        false ->
            case is_stored_user_exists(LUser, LServer) of
                true ->
                    put_user_into_cache(LUser, LServer),
                    true;
                false -> false
        end
    end.


%%====================================================================
%% Hooks
%%====================================================================

remove_user(LUser, LServer) ->
    {atomic, ok} = mnesia:transaction(fun() ->
        mnesia:delete(cached_user, {LUser, LServer}, write),
        ok
        end),
    ok.

%%====================================================================
%% Helpers
%%====================================================================
    
is_stored_user_exists(LUser, LServer) ->
    ejabberd_auth:is_user_exists(LUser, LServer)
    andalso not ejabberd_auth_anonymous:is_user_exists(LUser, LServer).

is_cached_user_exists(LUser, LServer) ->
    [] =/= mnesia:dirty_read(cached_user, {LServer, LUser}).

put_user_into_cache(LUser, LServer) ->
    mnesia:dirty_write(cached_user, #cached_user{host_name = {LServer, LUser}}),
    ok.
