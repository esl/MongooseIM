-module(mongoose_graphql_blocklist_admin_mutation).

-behaviour(mongoose_graphql).

%% mongoose_graphql callbacks
-export([execute/4]).

-include("jlib.hrl").

-import(mongoose_graphql_helper, [make_error/2, null_to_undefined/1]).

%% mongoose_graphql

execute(_Ctx, blocklist, <<"addUser">>, Args) ->
    add_user(Args);
execute(_Ctx, blocklist, <<"removeUser">>, Args) ->
    remove_user(Args).

%% Helpers

add_user(#{<<"user">> := User, <<"reason">> := Reason}) ->
    case mod_blocklist_api:add_user(User, null_to_undefined(Reason)) of
        ok -> {ok, true};
        Error -> make_error(Error, #{user => jid:to_binary(User)})
    end.

remove_user(#{<<"user">> := User}) ->
    case mod_blocklist_api:remove_user(User) of
        {ok, _} = Result -> Result;
        Error -> make_error(Error, #{user => jid:to_binary(User)})
    end.
