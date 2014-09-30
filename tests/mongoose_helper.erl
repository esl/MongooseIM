-module(mongoose_helper).

%% API
-export([total_offline_messages/0,
         total_active_users/0,
         total_privacy_items/0,
         total_private_items/0,
         total_vcard_items/0,
         total_roster_items/0]).

-define(RPC(M,F,A), escalus_ejabberd:rpc(M, F, A)).

total_offline_messages() ->
    generic_count(mod_offline_backend).

total_active_users() ->
    generic_count(mod_last_backend).

total_privacy_items() ->
    generic_count(mod_privacy_backend).

total_private_items() ->
    generic_count(mod_private_backend).

total_vcard_items() ->
    generic_count(mod_vcard_backend).

total_roster_items() ->
    Domain = escalus_ct:get_config(ejabberd_domain),
    RosterMnesia = ?RPC(gen_mod, is_loaded, [Domain, mod_roster]),
    RosterODBC = ?RPC(gen_mod, is_loaded, [Domain, mod_roster_odbc]),
    case {RosterMnesia, RosterODBC} of
        {true, _} ->
            generic_count_backend(mod_roster_mnesia);
        {_, true} ->
            generic_count_backend(mod_roster_odbc);
        _ ->
            false
    end.

get_backend(Module) ->
    try
        ?RPC(Module, backend, [])
    catch
        _:_  ->
            false
    end.

generic_count(Module) ->
    case get_backend(Module) of
        B when is_atom(B) ->
            generic_count_backend(B);
        false -> %% module disabled
            false
    end.

generic_count_backend(mod_offline_mnesia) -> count_wildpattern(offline_msg);
generic_count_backend(mod_offline_odbc) -> count_odbc(<<"offline_message">>);
generic_count_backend(mod_last_mnesia) -> count_wildpattern(last_activity);
generic_count_backend(mod_last_odbc) -> count_odbc(<<"last">>);
generic_count_backend(mod_privacy_mnesia) -> count_wildpattern(privacy);
generic_count_backend(mod_privacy_odbc) -> count_odbc(<<"privacy_list">>);
generic_count_backend(mod_private_mnesia) -> count_wildpattern(private_storage);
generic_count_backend(mod_private_odbc) -> count_odbc(<<"private_storage">>);
generic_count_backend(mod_vcard_mnesia) -> count_wildpattern(vcard);
generic_count_backend(mod_vcard_odbc) -> count_odbc(<<"vcard">>);
generic_count_backend(mod_roster_mnesia) -> count_wildpattern(roster);
generic_count_backend(mod_roster_odbc) -> count_odbc(<<"rosterusers">>).

count_wildpattern(Table) ->
    Pattern = ?RPC(mnesia, table_info, [Table, wild_pattern]),
    length(?RPC(mnesia, dirty_match_object, [Pattern])).

count_odbc(Table) ->
    {selected, _, [{N}]} =
        ?RPC(ejabberd_odbc,sql_query, [<<"localhost">>,<<"select count(*) from ", Table/binary, " ;">>]),
    list_to_integer(binary_to_list(N)).
