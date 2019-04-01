-module(service_admin_extra_gdpr).

-include("ejabberd_commands.hrl").

-export(
    [commands/0,
     retrieve_all/1]).

-type db() :: mnesia | sql. % TODO add if needed
-type tablename() :: atom() | binary().
-type binary_table_name() :: binary().
-type table() :: {db(), tablename()}.
-type guard() :: binary() |  any().
-type entity() :: [term()].
-type entities() :: [entity()].
-type schema() :: [binary()].
-type guard_data_id() :: username. % TODO add if needed
-type gurad_data() :: #{guard_data_id() => term()}.


-spec commands() -> [ejabberd_commands:cmd()].
commands() -> [
    #ejabberd_commands{name = retrieve, tags = [gdpr],
                       desc = "Retrieve user's presonal data.",
                       longdesc = "Retrieves all personal data from MongooseIM for a given user. Example:\n"
                       " %TODO ", % TODO add example
                       module = ?MODULE,
                       function = retrieve_all,
                       args = [{username, binary}], % TODO add arguments if needed
                       result = {records, binary}}  % TODO check if returned type is correct and convinient in use
].

-spec retrieve_all(Username :: binary()) -> RetrievedFilesInZipName :: binary().
retrieve_all(Username) ->
    DataFromTables = get_data_from_tables(#{username => Username}),
    CsvFiles = lists:map(
        fun({Tablename, Schema, Entitis}) ->
            to_csv_file(<<Tablename/binary, <<".csv">>/binary>>, Schema, Entitis) end,
        DataFromTables),
    zip:create(<<Username/binary, <<"retrived_data.zip">>/binary>>, CsvFiles).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                       Private funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_data_from_tables(gurad_data()) ->
    [{binary_table_name(), schema(), entities()}].
get_data_from_tables(GuardData) ->
    TablesWithGuards = get_tables_with_guards(GuardData),
    [get_data_from_table(Tab, Guard)|| {Tab, Guard} <- TablesWithGuards].

-spec to_csv_file(CsvFilename :: binary(), schema(), entities()) -> CsvFilename :: binary().
to_csv_file(Filename, DataSchema, DataRows) ->
    {ok, File} = file:open(Filename, [write]),
    csv_gen:row(File, DataSchema),
    lists:foreach(DataRows, fun(Row) -> csv_gen:row(File, Row) end),
    file:close(File),
    Filename.

-spec get_data_from_table(table(), guard()) -> {binary_table_name(), schema(), entities()}.
get_data_from_table({sql, TableName}, Guard) ->
    {TableName, [], sql_slelect_helper(TableName, Guard)}; % TODO implement getting schema

get_data_from_table({mnesia, TableName}, Guard) ->
    {TableName, [], mnesia:select(TableName, Guard)}.% TODO implement getting schema

%% TODO list all possible tables inside the list
-spec get_tables_with_guards(gurad_data()) -> [{db(), tablename(), guard()}].
get_tables_with_guards(_GuardData) ->
    [],
    erlang:error("Not implemented").

-spec sql_slelect_helper(binary_table_name(), guard()) -> entities().
sql_slelect_helper(TableName, Guard) ->
    SQL = ["SELECT * from ", TableName, " WHERE ", Guard],
    case mongoose_rdbms:sql_query_t(SQL) of
        {selected, []} ->
            {error, not_found};
        {selected, Rows} ->
            {ok, Rows}
    end.
