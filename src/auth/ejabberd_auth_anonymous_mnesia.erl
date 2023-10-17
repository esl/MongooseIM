-module(ejabberd_auth_anonymous_mnesia).

-behaviour(ejabberd_auth_anonymous_backend).

-export([init/1, stop/1, does_anonymous_user_exist/2, add_connection/3, remove_connection/3]).

-record(anonymous, {us  :: jid:simple_bare_jid(), sid :: ejabberd_sm:sid()}).

-spec init(mongooseim:host_type()) -> ok.
init(_HostType) ->
    mnesia:create_table(anonymous,
                        [{ram_copies, [node()]}, {type, bag},
                         {attributes, record_info(fields, anonymous)}]),
    mnesia:add_table_copy(anonymous, node(), ram_copies),
    ok.

-spec does_anonymous_user_exist(mongooseim:host_type(), jid:simple_bare_jid()) -> boolean().
does_anonymous_user_exist(_HostType, US) ->
    [] =/= catch mnesia:dirty_read({anonymous, US}).

-spec add_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.
add_connection(_HostType, SID, US) ->
    mnesia:sync_dirty(fun() -> mnesia:write(#anonymous{us = US, sid = SID}) end).

-spec remove_connection(mongooseim:host_type(), ejabberd_sm:sid(), jid:simple_bare_jid()) -> ok.
remove_connection(_HostType, SID, US) ->
    mnesia:transaction(fun() -> mnesia:delete_object({anonymous, US, SID}) end),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.
