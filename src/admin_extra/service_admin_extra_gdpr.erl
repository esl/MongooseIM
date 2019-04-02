-module(service_admin_extra_gdpr).

-include("ejabberd_commands.hrl").

-export(
    [commands/0,
     retrieve_all/2]).

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

-spec retrieve_all(gdpr:username(), gdpr:domain()) -> RetrievedFilesInZipName :: binary().
retrieve_all(Username, Domain) ->
    DataFromTables = get_data_from_tables(Username, Domain),
    CsvFiles = lists:map(
        fun({Tablename, Schema, Entitis}) ->
            to_csv_file(<<Tablename/binary, <<".csv">>/binary>>, Schema, Entitis) end,
        DataFromTables),
    zip:create(<<Username/binary, <<"retrived_data.zip">>/binary>>, CsvFiles).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                       Private funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_data_from_tables(gdpr:username(), gdpr:domain()) ->
    [{gdpr:binary_table_name(), gdpr:schema(), gdpr:entities()}].
get_data_from_tables(Username, Domain) ->
    Modules = get_modules(),
    lists:flatten([M:get_personal_data(Username, Domain)|| M <- Modules]).

-spec to_csv_file(CsvFilename :: binary(), gdpr:schema(), gdpr:entities()) -> CsvFilename :: binary().
to_csv_file(Filename, DataSchema, DataRows) ->
    {ok, File} = file:open(Filename, [write]),
    csv_gen:row(File, DataSchema),
    lists:foreach(DataRows, fun(Row) -> csv_gen:row(File, Row) end),
    file:close(File),
    Filename.

-spec get_modules() -> [module()].
get_modules() ->
    erlang:error("Not implemented").
