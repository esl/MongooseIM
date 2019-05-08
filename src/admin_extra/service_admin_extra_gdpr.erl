-module(service_admin_extra_gdpr).

-include("ejabberd_commands.hrl").

-export([commands/0,
         retrieve_all/3]).

% Exported for RPC call
-export([retrieve_logs/2]).

-define(CMD_TIMEOUT, 300000).

-spec commands() -> [ejabberd_commands:cmd()].
commands() -> [
        #ejabberd_commands{name = retrieve_personal_data, tags = [gdpr],
            desc = "Retrieve user's presonal data.",
            longdesc = "Retrieves all personal data from MongooseIM for a given user. Example:\n"
                       " mongooseimctl alice localhost /home/mim/alice.smith.zip ",
            module = ?MODULE,
            function = retrieve_all,
            args = [{username, binary}, {domain, binary}, {path, binary}],
            result = {res, rescode}}

    ].

-spec retrieve_all(jid:user(), jid:server(), Path :: binary()) ->
    RetrievedFilesInZipName :: binary() | {error, Reason :: any()}.
retrieve_all(Username, Domain, ResultFilePath) ->
    case user_exists(Username, Domain) of
        true ->
            DataFromModules = get_data_from_modules(Username, Domain),
            TmpDir = make_tmp_dir(),

            CsvFiles = lists:map(
                fun({DataGroup, Schema, Entries}) ->
                    BinDataGroup = atom_to_binary(DataGroup, utf8),
                    FileName = <<BinDataGroup/binary, ".csv">>,
                    to_csv_file(FileName, Schema, Entries, TmpDir),
                    binary_to_list(FileName)
                end,
                DataFromModules),

            LogFiles = get_all_logs(Username, Domain, TmpDir),
            
            ZipFile = binary_to_list(ResultFilePath),
            {ok, ZipFile} = zip:create(ZipFile, CsvFiles ++ LogFiles, [{cwd, TmpDir}]),
            remove_tmp_dir(TmpDir),
            ok;
        false ->
            {error, "User does not exist"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                       Private funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec modules_with_personal_data() -> [module()].
modules_with_personal_data() ->
    mongoose_lib:find_behaviour_implementations(gdpr).

-spec get_data_from_modules(jid:user(), jid:server()) ->
    [{gdpr:data_group(), gdpr:schema(), gdpr:entries()}].
get_data_from_modules(Username, Domain) ->
    Modules = modules_with_personal_data(),
    lists:flatmap(fun(M) -> try_get_data_from_module(M, Username, Domain) end, Modules).

try_get_data_from_module(Module, Username, Domain) ->
    try Module:get_personal_data(Username, Domain) of
        [{_, _, []}] -> [];
        Val -> Val
    catch
        _:_ -> []
    end.

-spec to_csv_file(CsvFilename :: binary(), gdpr:schema(), gdpr:entities(), file:name()) -> ok.
to_csv_file(Filename, DataSchema, DataRows, TmpDir) ->
    FilePath = <<(list_to_binary(TmpDir))/binary, "/", Filename/binary>>,
    {ok, File} = file:open(FilePath, [write]),
    csv_gen:row(File, DataSchema),
    lists:foreach(fun(Row) -> csv_gen:row(File, Row) end, DataRows),
    file:close(File).

-spec user_exists(gdpr:username(), gdpr:domain()) -> boolean().
user_exists(Username, Domain) ->
    ejabberd_auth:is_user_exists(Username, Domain).

-spec make_tmp_dir() -> file:name().
make_tmp_dir() ->
    TmpDirName = lists:flatten(io_lib:format("/tmp/gdpr-~4.36.0b", [rand:uniform(36#zzzz)])),
    case file:make_dir(TmpDirName) of
        ok -> TmpDirName;
        {error, eexist} -> make_tmp_dir();
        {error, Error} -> {error, Error}
    end.

-spec remove_tmp_dir(file:name()) -> ok.
remove_tmp_dir(TmpDir) ->
    {ok, FileNames} = file:list_dir(TmpDir),
    [file:delete(TmpDir ++ "/" ++ File) || File <- FileNames],
    file:del_dir(TmpDir).

-type cmd() :: string() | binary().
-spec run(cmd(), [cmd()], timeout()) -> non_neg_integer() | timeout.
run(Cmd, Args, Timeout) ->
    Port = erlang:open_port({spawn_executable, Cmd}, [exit_status, {args, Args}]),
    receive
        {Port, {exit_status, ExitStatus}} -> ExitStatus
    after Timeout ->
        timeout
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                       Logs retrieval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec retrieve_logs(gdpr:username(), gdpr:domain()) -> {ok, ZippedLogs :: binary()}.
retrieve_logs(Username, Domain) ->
    TmpDir = make_tmp_dir(),
    LogFile = get_logs(Username, Domain, TmpDir),
    {ok, {_, ZippedLogs}} = zip:create("archive.zip", [LogFile], [memory, {cwd, TmpDir}]),
    remove_tmp_dir(TmpDir),
    {ok, ZippedLogs}.

-spec get_all_logs(gdpr:username(), gdpr:domain(), file:name()) -> [file:name()].
get_all_logs(Username, Domain, TmpDir) ->
    OtherNodes = ejabberd_config:other_cluster_nodes(),
    LogFile = get_logs(Username, Domain, TmpDir),
    LogFilesFromOtherNodes = [get_logs_from_node(Node, Username, Domain, TmpDir) || Node <- OtherNodes],
    [LogFile | LogFilesFromOtherNodes].

-spec get_logs(gdpr:username(), gdpr:domain(), file:name()) -> file:name().
get_logs(Username, Domain, TmpDir) ->
    FileList = [filename:absname(F) || F <- ejabberd_loglevel:get_log_files()],
    Cmd = code:priv_dir(mongooseim) ++ "/parse_logs.sh",
    FileName = "logs-" ++ atom_to_list(node()) ++ ".txt",
    FilePath = TmpDir ++ "/" ++ FileName,
    Args = [FilePath, Username, Domain | FileList],
    0 = run(Cmd, Args, ?CMD_TIMEOUT),
    FileName.

-spec get_logs_from_node(node(), gdpr:username(), gdpr:domain(), file:name()) -> file:name().
get_logs_from_node(Node, Username, Domain, TmpDir) ->
    {ok, ZippedData} = rpc:call(Node, ?MODULE, retrieve_logs, [Username, Domain]),
    {ok, [File]} = zip:unzip(ZippedData, [{cwd, TmpDir}]),
    filename:basename(File).
