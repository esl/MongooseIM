%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via mnesia
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_internal).
-author('alexey@process-one.net').

%% External exports
-behaviour(mongoose_gen_auth).

-export([start/1,
         stop/1,
         set_password/4,
         authorize/1,
         try_register/4,
         get_registered_users/3,
         get_registered_users_number/3,
         get_password/3,
         get_password_s/3,
         does_user_exist/3,
         remove_user/3,
         remove_domain/2,
         supports_sasl_module/2,
         supported_features/0
        ]).

-export([scram_passwords/0]).

%% Internal
-export([check_password/4,
         check_password/6]).

%% Utilities
-export([dirty_get_registered_users/0]).

-ignore_xref([dirty_get_registered_users/0, scram_passwords/0]).

-include("mongoose.hrl").
-include("scram.hrl").

-record(passwd, {us, password}).

-type passwd() :: #passwd{
                     us :: jid:simple_bare_jid(),
                     password :: binary() | #scram{} | mongoose_scram:scram_map()
                    }.

-record(reg_users_counter, {vhost, count}).

-type users_counter() :: #reg_users_counter {
                            vhost :: binary(),
                            count :: integer()
                           }.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type()) -> ok.
start(HostType) ->
    mongoose_mnesia:create_table(passwd,
        [{disc_copies, [node()]},
         {attributes, record_info(fields, passwd)},
         {storage_properties, [{ets, [{read_concurrency, true}]}]}]),
    mongoose_mnesia:create_table(reg_users_counter,
        [{ram_copies, [node()]},
         {attributes, record_info(fields, reg_users_counter)}]),
    update_reg_users_counter_table(HostType),
    ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec update_reg_users_counter_table(Server :: jid:server()) -> any().
update_reg_users_counter_table(Server) ->
    Set = get_users(Server),
    Size = length(Set),
    LServer = jid:nameprep(Server),
    F = fun() ->
            write_counter(#reg_users_counter{vhost = LServer, count = Size})
        end,
    mnesia:sync_dirty(F).

-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_HostType, cyrsasl_plain) -> true;
supports_sasl_module(HostType, cyrsasl_digest) -> not mongoose_scram:enabled(HostType);
supports_sasl_module(HostType, Mechanism) -> mongoose_scram:enabled(HostType, Mechanism).

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(_HostType, LUser, LServer, Password) ->
    US = {LUser, LServer},
    case catch dirty_read_passwd(US) of
        [#passwd{password = Scram}] when is_map(Scram) orelse is_record(Scram, scram) ->
            mongoose_scram:check_password(Password, Scram);
        [#passwd{password = Password}] ->
            Password /= <<>>;
        _ ->
            false
    end.


-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(_HostType, LUser, LServer, Password, Digest, DigestGen) ->
    US = {LUser, LServer},
    case catch dirty_read_passwd(US) of
        [#passwd{password = Scram}] when is_record(Scram, scram) orelse is_map(Scram) ->
            mongoose_scram:check_digest(Scram, Digest, DigestGen, Password);
        [#passwd{password = Passwd}] ->
            ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd);
        _ ->
            false
    end.


-spec set_password(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()) -> ok | {error, not_allowed | invalid_jid}.
set_password(HostType, LUser, LServer, Password) ->
    US = {LUser, LServer},
    F = fun() ->
        Password2 = get_scram(HostType, Password),
        write_passwd(#passwd{us = US, password = Password2})
    end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

-spec try_register(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, exists | not_allowed}.
try_register(HostType, LUser, LServer, Password) ->
    US = {LUser, LServer},
    F = fun() ->
        case read_passwd(US) of
            [] ->
                Password2 = get_scram(HostType, Password),
                write_passwd(#passwd{us = US, password = Password2}),
                mnesia:dirty_update_counter(reg_users_counter, LServer, 1),
                ok;
            [_E] ->
                exists
        end
    end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            ok;
        {atomic, exists} ->
            {error, exists};
        Result ->
            ?LOG_ERROR(#{what => registration_error,
                         user => LUser, server => LServer, reason => Result}),
            {error, not_allowed}
    end.


%% @doc Get all registered users in Mnesia
-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    mnesia:dirty_all_keys(passwd).

-spec get_users(LServer :: jid:lserver()) -> [jid:simple_bare_jid()].
get_users(LServer) ->
    mnesia:dirty_select(
      passwd,
      [{#passwd{us = '$1', _ = '_'},
        [{'==', {element, 2, '$1'}, LServer}],
        ['$1']}]).

get_registered_users(_, LServer, Opts) ->
    get_users(LServer, Opts).

-type query_keyword() :: from | to | limit | offset | prefix.
-type query_value() :: integer() | binary().
-spec get_users(LServer :: jid:lserver(),
                Query :: [{query_keyword(), query_value()}]
               ) -> [jid:simple_bare_jid()].
get_users(LServer, [{from, Start}, {to, End}])
        when is_integer(Start) and is_integer(End) ->
    get_users(LServer, [{limit, End-Start+1}, {offset, Start}]);
get_users(LServer, [{limit, Limit}, {offset, Offset}])
        when is_integer(Limit) and is_integer(Offset) ->
    get_users_within_interval(get_users(LServer), Limit, Offset);
get_users(LServer, [{prefix, Prefix}])
        when is_binary(Prefix) ->
    Users = matching_users(Prefix, get_users(LServer)),
    lists:keysort(1, Users);
get_users(LServer, [{prefix, Prefix}, {from, Start}, {to, End}])
        when is_binary(Prefix) and is_integer(Start) and is_integer(End) ->
    get_users(LServer, [{prefix, Prefix}, {limit, End-Start+1}, {offset, Start}]);
get_users(LServer, [{prefix, Prefix}, {limit, Limit}, {offset, Offset}])
        when is_binary(Prefix) and is_integer(Limit) and is_integer(Offset) ->
    Users = matching_users(Prefix, get_users(LServer)),
    get_users_within_interval(Users, Limit, Offset);
get_users(LServer, _) ->
    get_users(LServer).

-spec get_users_number(LServer :: jid:server()) -> non_neg_integer().
get_users_number(LServer) ->
    Query = mnesia:dirty_select(
                reg_users_counter,
                [{#reg_users_counter{vhost = LServer, count = '$1'},
                  [],
                  ['$1']}]),
    case Query of
        [Count] ->
            Count;
        _ -> 0
    end.

get_registered_users_number(_, LServer, Query) ->
    get_users_number(LServer, Query).

-spec get_users_number(LServer :: jid:lserver(), Query :: [{prefix, binary()}]) -> integer().
get_users_number(LServer, [{prefix, Prefix}]) when is_binary(Prefix) ->
    length(matching_users(Prefix, get_users(LServer)));
get_users_number(LServer, _) ->
    get_users_number(LServer).

matching_users(Prefix, Users) ->
    lists:filter(fun({U, _S}) ->
                         binary:longest_common_prefix([U, Prefix]) =:= byte_size(Prefix)
                 end, Users).

-spec get_password(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          ejabberd_auth:passterm() | false.
get_password(_, LUser, LServer) ->
    US = {LUser, LServer},
    case catch dirty_read_passwd(US) of
        [#passwd{password = Scram}] when is_record(Scram, scram) ->
            mongoose_scram:scram_record_to_map(Scram);
        [#passwd{password = Params}] when is_map(Params)->
            Params;
        [#passwd{password = Password}] ->
            Password;
        _ ->
            false
    end.

-spec get_password_s(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary().
get_password_s(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    case catch dirty_read_passwd(US) of
        [#passwd{password = Scram}] when is_record(Scram, scram) ->
            <<"">>;
        [#passwd{password = Params}] when is_map(Params)->
            <<"">>;
        [#passwd{password = Password}] ->
            Password;
        _ ->
            <<"">>
    end.

-spec does_user_exist(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          boolean() | {error, atom()}.
does_user_exist(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    case catch dirty_read_passwd(US) of
        [] ->
            false;
        [_] ->
            true;
        Other ->
            {error, Other}
    end.


%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) -> ok | {error, not_allowed}.
remove_user(_HostType, LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() ->
                mnesia:delete({passwd, US}),
                mnesia:dirty_update_counter(reg_users_counter,
                                            LServer, -1)
        end,
    mnesia:transaction(F),
    ok.

-spec remove_domain(mongooseim:host_type(), jid:lserver()) -> ok.
remove_domain(_HostType, LServer) ->
    Users = get_users(LServer),
    F = fun() ->
                lists:foreach(fun(User) ->
                    mnesia:delete({passwd, User}),
                    mnesia:dirty_update_counter(reg_users_counter,
                                                LServer, -1)
                end, Users)
        end,
    mnesia:transaction(F),
    ok.

-spec scram_passwords() -> {atomic, ok}.
scram_passwords() ->
    ?LOG_INFO(#{what => <<"scram_passwords">>,
                text => <<"Converting the stored passwords into SCRAM bits">>}),
    Fields = record_info(fields, passwd),
    {atomic, ok} = mnesia:transform_table(passwd, fun scramming_function/1, Fields).

-spec scramming_function(passwd()) -> passwd().
scramming_function(#passwd{us = {_, Server}, password = Password} = P) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(Server),
    Scram = mongoose_scram:password_to_scram(HostType, Password, mongoose_scram:iterations(HostType)),
    P#passwd{password = Scram}.

-spec dirty_read_passwd(US :: jid:simple_bare_jid()) -> [passwd()].
dirty_read_passwd(US) ->
    mnesia:dirty_read(passwd, US).

-spec read_passwd(US :: jid:simple_bare_jid()) -> [passwd()].
read_passwd(US) ->
    mnesia:read({passwd, US}).

-spec write_passwd(passwd()) -> ok.
write_passwd(#passwd{} = Passwd) ->
    mnesia:write(Passwd).

-spec write_counter(users_counter()) -> ok.
write_counter(#reg_users_counter{} = Counter) ->
    mnesia:write(Counter).

-spec get_scram(binary(), binary()) -> mongoose_scram:scram() | binary().
get_scram(HostType, Password) ->
    case mongoose_scram:enabled(HostType) and is_binary(Password) of
        true ->
            Iterations = mongoose_scram:iterations(HostType),
            mongoose_scram:password_to_scram(HostType, Password, Iterations);
        false -> Password
    end.

-spec get_users_within_interval(list(), integer(), integer()) -> list().
get_users_within_interval([], _Limit, _Offset) -> [];
get_users_within_interval(Users, Limit, Offset) ->
    SortedUsers = lists:keysort(1, Users),
    lists:sublist(SortedUsers, Offset, Limit).

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].
