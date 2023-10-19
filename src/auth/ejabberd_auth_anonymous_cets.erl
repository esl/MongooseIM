-module(ejabberd_auth_anonymous_cets).

-behaviour(ejabberd_auth_anonymous_backend).

-export([init/1, stop/1, does_anonymous_user_exist/2, add_connection/3, remove_connection/3]).

-spec init(mongooseim:host_type()) -> ok.
init(HostType) ->
    Tab = table_name(HostType),
    cets:start(Tab, #{type => bag}),
    cets_discovery:add_table(mongoose_cets_discovery, Tab),
    ok.

-spec does_anonymous_user_exist(mongooseim:host_type(), jid:simple_bare_jid()) -> boolean().
does_anonymous_user_exist(HostType, US) ->
    Tab = table_name(HostType),
    [] =/= ets:lookup(Tab, US).

-spec add_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.
add_connection(HostType, SID, US) ->
    Tab = table_name(HostType),
    cets:insert(Tab, {US, SID}).

-spec remove_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.
remove_connection(HostType, SID, US) ->
    Tab = table_name(HostType),
    cets:delete_object(Tab, {US, SID}).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    Tab = table_name(HostType),
    cets_discovery:delete_table(mongoose_cets_discovery, Tab),
    cets:stop(Tab),
    ok.

table_name(HostType) ->
    binary_to_atom(<<"cets_auth_anonymous_", HostType/binary>>).
