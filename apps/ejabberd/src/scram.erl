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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(scram).

-author('stephen.roettger@googlemail.com').

-include("ejabberd.hrl").

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
         check_password/2
        ]).

-define(SALT_LENGTH, 16).
-define(SCRAM_DEFAULT_ITERATION_COUNT, 4096).

-spec salted_password(binary(), binary(), non_neg_integer()) -> binary().
salted_password(Password, Salt, IterationCount) ->
    hi(jlib:resourceprep(Password), Salt, IterationCount).

-spec client_key(binary()) -> binary().
client_key(SaltedPassword) ->
    crypto_hmac(sha, SaltedPassword, <<"Client Key">>).

-spec stored_key(binary()) -> binary().
stored_key(ClientKey) -> crypto:hash(sha, ClientKey).

-spec server_key(binary()) -> binary().
server_key(SaltedPassword) ->
    crypto_hmac(sha, SaltedPassword, <<"Server Key">>).

-spec client_signature(binary(), binary()) -> binary().
client_signature(StoredKey, AuthMessage) ->
    crypto_hmac(sha, StoredKey, AuthMessage).

-spec client_key(binary(), binary()) -> binary().
client_key(ClientProof, ClientSignature) ->
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
				 binary_to_list(ClientProof),
				 binary_to_list(ClientSignature))).

-spec server_signature(binary(), binary()) -> binary().
server_signature(ServerKey, AuthMessage) ->
    crypto_hmac(sha, ServerKey, AuthMessage).

hi(Password, Salt, IterationCount) ->
    U1 = crypto_hmac(sha, Password, <<Salt/binary, 0, 0, 0, 1>>),
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
				 binary_to_list(U1),
				 binary_to_list(hi_round(Password, U1,
							 IterationCount - 1)))).

hi_round(Password, UPrev, 1) ->
    crypto_hmac(sha, Password, UPrev);
hi_round(Password, UPrev, IterationCount) ->
    U = crypto_hmac(sha, Password, UPrev),
    list_to_binary(lists:zipwith(fun (X, Y) -> X bxor Y end,
				 binary_to_list(U),
				 binary_to_list(hi_round(Password, U,
							 IterationCount - 1)))).


enabled(Host) ->
    scram == ejabberd_config:get_local_option({auth_password_format, Host}).

iterations() -> ?SCRAM_DEFAULT_ITERATION_COUNT.

iterations(Host) ->
    case ejabberd_config:get_local_option({auth_scram_iterations, Host}) of
        undefined -> ?SCRAM_DEFAULT_ITERATION_COUNT;
        Iterations -> Iterations
    end.

password_to_scram(Password) ->
    password_to_scram(Password, ?SCRAM_DEFAULT_ITERATION_COUNT).

password_to_scram(#scram{} = Password, _) ->
    Password;
password_to_scram(Password, IterationCount) ->
    Salt = crypto:rand_bytes(?SALT_LENGTH),
    SaltedPassword = salted_password(Password, Salt, IterationCount),
    StoredKey = stored_key(scram:client_key(SaltedPassword)),
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


-ifdef(no_crypto_hmac).
crypto_hmac(sha, Key, Data) ->
    crypto:sha_mac(Key, Data).
-else.
crypto_hmac(sha, Key, Data) ->
    crypto:hmac(sha, Key, Data).
-endif.
