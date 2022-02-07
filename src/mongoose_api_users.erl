%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongoose_api_users).

%% mongoose_api callbacks
-export([prefix/0,
         routes/0,
         handle_options/2,
         handle_get/2,
         handle_put/3,
         handle_delete/2]).

-ignore_xref([handle_delete/2, handle_get/2, handle_options/2, handle_put/3,
              prefix/0, routes/0]).

-define(ERROR, {error, unprocessable}).

%%--------------------------------------------------------------------
%% mongoose_api callbacks
%%--------------------------------------------------------------------
-spec prefix() -> mongoose_api:prefix().
prefix() ->
    "/users".

-spec routes() -> mongoose_api:routes().
routes() ->
    [{"/host/:host",                    [host_users]},
     {"/host/:host/username/:username", [host_user]}].

-spec handle_options(mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:methods().
handle_options(_Bindings, [host_users]) ->
    [get];
handle_options(_Bindings, [host_user]) ->
    [put, delete].

-spec handle_get(mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:response().
handle_get(Bindings, [host_users]) ->
    get_users(Bindings).

-spec handle_put(term(), mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:response().
handle_put(Data, Bindings, [host_user]) ->
    put_user(Data, Bindings).

-spec handle_delete(mongoose_api:bindings(), mongoose_api:options()) ->
    mongoose_api:response().
handle_delete(Bindings, [host_user]) ->
    delete_user(Bindings).

%%--------------------------------------------------------------------
%% mongoose_api commands actual handlers
%%--------------------------------------------------------------------
get_users(Bindings) ->
    Host = proplists:get_value(host, Bindings),
    Users = ejabberd_auth:get_vh_registered_users(Host),
    Response = [{count, length(Users)},
                {users, users_to_proplist(Users)}],
    {ok, Response}.

put_user(Data, Bindings) ->
    Host = proplists:get_value(host, Bindings),
    Username = proplists:get_value(username, Bindings),
    case proplist_to_user(Data) of
        {ok, Password} ->
            maybe_register_user(Username, Host, Password);
        {error, _} ->
            ?ERROR
    end.

delete_user(Bindings) ->
    Host = proplists:get_value(host, Bindings),
    Username = proplists:get_value(username, Bindings),
    JID = jid:make(Username, Host, <<>>),
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            maybe_delete_user(JID);
        false ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
maybe_register_user(Username, Host, Password) ->
    JID = jid:make(Username, Host, <<>>),
    case ejabberd_auth:try_register(JID, Password) of
        {error, not_allowed} ->
            ?ERROR;
        {error, exists} ->
            maybe_change_password(JID, Password);
        _ ->
            ok
    end.

maybe_change_password(JID, Password) ->
    case ejabberd_auth:set_password(JID, Password) of
        {error, _} ->
            ?ERROR;
        ok ->
            ok
    end.

maybe_delete_user(JID) ->
    case ejabberd_auth:remove_user(JID) of
        ok ->
            ok;
        _ ->
            error
    end.

users_to_proplist(Users) ->
    [user_to_proplist(User) || User <- Users].

user_to_proplist({Username, Host}) ->
    {user, [{username, Username}, {host, Host}]}.

proplist_to_user([{<<"user">>, User}]) ->
    case proplists:get_value(<<"password">>, User, undefined) of
        undefined ->
            ?ERROR;
        Password ->
            {ok, Password}
    end;
proplist_to_user(_Other) ->
    ?ERROR.
