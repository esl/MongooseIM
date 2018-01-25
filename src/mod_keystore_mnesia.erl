-module(mod_keystore_mnesia).

-export([init/2,
         init_ram_key/1,
         get_key/1]).

-include("mod_keystore.hrl").

-spec init(Domain, Opts) -> ok when
      Domain :: jid:server(),
      Opts :: [any()].
init(_Domain, _Opts) ->
    mnesia:create_table(key,
                        [{ram_copies, [node()]},
                         {type, set},
                         {attributes, record_info(fields, key)}]),
    mnesia:add_table_copy(key, node(), ram_copies),
    ok.

-spec init_ram_key(ProposedKey) -> Result when
      ProposedKey :: mod_keystore:key(),
      Result :: {ok, ActualKey} | {error, any()},
      ActualKey :: mod_keystore:key().
init_ram_key(ProposedKey) ->
    case mnesia:transaction(fun init_ram_key_transaction/1, [ProposedKey]) of
        {aborted, Reason} -> {error, Reason};
        {atomic, Key} -> {ok, Key}
    end.

init_ram_key_transaction(#key{id = KeyID} = ProposedKey) ->
    case mnesia:wread({key, KeyID}) of
        [#key{} = ActualKey] ->
            ActualKey;
        [] ->
            mnesia:write(ProposedKey),
            ProposedKey
    end.

-spec get_key(ID :: mod_keystore:key_id()) -> mod_keystore:key_list().
get_key(ID) ->
    case mnesia:transaction(fun mnesia:read/1, [{key, ID}]) of
        {aborted, Reason} -> error(Reason, [ID]);
        {atomic, []} -> [];
        {atomic, [#key{id = ID, key = Key}]} -> [{ID, Key}]
    end.
