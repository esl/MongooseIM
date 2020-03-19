%%%----------------------------------------------------------------------
%%% File    : scram.erl
%%% Author  : Stephen Röttger <stephen.roettger@googlemail.com>
%%% Purpose : SCRAM (RFC 5802)
%%% Created : 7 Aug 2011 by Stephen Röttger <stephen.roettger@googlemail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
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

-module(mongoose_scram_sha256).

-author('stephen.roettger@googlemail.com').

-include("mongoose.hrl").
-include("scram.hrl").

%% External exports
%% ejabberd doesn't implement SASLPREP, so we use the similar RESOURCEPREP instead
-export([ % Core SCRAM functions
         salted_password/3,
         stored_key/1,
         server_key/1,
         server_signature/2,
         client_signature/2,
         client_key/1,
         client_key/2]).

-export([
         enabled/1,
         iterations/0,
         iterations/1,
         password_to_scram/1,
         password_to_scram/2,
         check_password/2,
         check_digest/4
        ]).

-export([serialize/1, deserialize/1]).

-export([scram_to_tuple/1]).

-type scram_tuple() :: { StoredKey :: binary(), ServerKey :: binary(),
                         Salt :: binary(), Iterations :: non_neg_integer() }.

-type scram() :: #scram{}.

-export_type([scram_tuple/0, scram/0]).

-define(SALT_LENGTH, 16).
-define(SCRAM_DEFAULT_ITERATION_COUNT, 4096).
-define(SCRAM_SERIAL_PREFIX, "==SCRAM==,").

-spec salted_password(binary(), binary(), non_neg_integer()) -> binary().
salted_password(Password, Salt, IterationCount) ->
    hi(jid:resourceprep(Password), Salt, IterationCount).

-spec client_key(binary()) -> binary().
client_key(SaltedPassword) ->
    crypto:hmac(sha256, SaltedPassword, <<"Client Key">>).

-spec stored_key(binary()) -> binary().
stored_key(ClientKey) -> crypto:hash(sha256, ClientKey).

-spec server_key(binary()) -> binary().
server_key(SaltedPassword) ->
    crypto:hmac(sha256, SaltedPassword, <<"Server Key">>).

-spec client_signature(binary(), binary()) -> binary().
client_signature(StoredKey, AuthMessage) ->
    crypto:hmac(sha256, StoredKey, AuthMessage).

-spec client_key(binary(), binary()) -> binary().
client_key(ClientProof, ClientSignature) ->
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
                                 binary_to_list(ClientProof),
                                 binary_to_list(ClientSignature))).

-spec server_signature(binary(), binary()) -> binary().
server_signature(ServerKey, AuthMessage) ->
    crypto:hmac(sha256, ServerKey, AuthMessage).

hi(Password, Salt, IterationCount) ->
    U1 = crypto:hmac(sha256, Password, <<Salt/binary, 0, 0, 0, 1>>),
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
                                 binary_to_list(U1),
                                 binary_to_list(hi_round(Password, U1,
                                                         IterationCount - 1)))).

hi_round(Password, UPrev, 1) ->
    crypto:hmac(sha256, Password, UPrev);
hi_round(Password, UPrev, IterationCount) ->
    U = crypto:hmac(sha256, Password, UPrev),
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
                                 binary_to_list(U),
                                 binary_to_list(hi_round(Password, U,
                                                         IterationCount - 1)))).


enabled(Host) ->
    ejabberd_auth:get_opt(Host, password_format) == scram.

%% This function is exported and used from other modules
iterations() -> ?SCRAM_DEFAULT_ITERATION_COUNT.

iterations(Host) ->
    ejabberd_auth:get_opt(Host, scram_iterations, ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(Password) ->
    password_to_scram(Password, ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(#scram{} = Password, _) ->
    Password;
password_to_scram(Password, IterationCount) ->
    Salt = crypto:strong_rand_bytes(?SALT_LENGTH),
    SaltedPassword = salted_password(Password, Salt, IterationCount),
    StoredKey = stored_key(client_key(SaltedPassword)),
    ServerKey = server_key(SaltedPassword),
    #scram{storedkey = base64:encode(StoredKey),
           serverkey = base64:encode(ServerKey),
           salt = base64:encode(Salt),
           iterationcount = IterationCount}.

check_password(Password, Scram) ->
    IterationCount = Scram#scram.iterationcount,
    Salt = base64:decode(Scram#scram.salt),
    SaltedPassword = salted_password(Password, Salt, IterationCount),
    StoredKey = stored_key(client_key(SaltedPassword)),
    (base64:decode(Scram#scram.storedkey) == StoredKey).

serialize(#scram{storedkey = StoredKey, serverkey = ServerKey,
                     salt = Salt, iterationcount = IterationCount})->
    IterationCountBin = integer_to_binary(IterationCount),
    << <<?SCRAM_SERIAL_PREFIX>>/binary,
       StoredKey/binary, $,, ServerKey/binary,
       $,, Salt/binary, $,, IterationCountBin/binary>>.

deserialize(<<?SCRAM_SERIAL_PREFIX, Serialized/binary>>) ->
    case catch binary:split(Serialized, <<",">>, [global]) of
        [StoredKey, ServerKey, Salt, IterationCount] ->
            {ok, #scram{storedkey = StoredKey,
                        serverkey = ServerKey,
                        salt = Salt,
                        iterationcount = binary_to_integer(IterationCount)}};
        _ ->
            ?WARNING_MSG("Incorrect serialized SCRAM: ~p, ~p", [Serialized]),
            {error, incorrect_scram}
    end;
deserialize(Bin) ->
    ?WARNING_MSG("Corrupted serialized SCRAM: ~p", [Bin]),
    {error, corrupted_scram}.

-spec scram_to_tuple(scram()) -> scram_tuple().
scram_to_tuple(Scram) ->
    {base64:decode(Scram#scram.storedkey),
     base64:decode(Scram#scram.serverkey),
     base64:decode(Scram#scram.salt),
     Scram#scram.iterationcount}.

-spec check_digest(scram(), binary(), fun(), binary()) -> boolean().
check_digest(#scram{storedkey = StoredKey}, Digest, DigestGen, Password) ->
    Passwd = base64:decode(StoredKey),
    ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd).
