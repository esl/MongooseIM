-module(gdpr_api).

-include("jlib.hrl").

% Exported for RPC call
-export([retrieve_all/3, retrieve_logs/2, get_data_from_modules/2]).

-ignore_xref([retrieve_all/3, retrieve_logs/2, get_data_from_modules/2]).

-define(CMD_TIMEOUT, 300000).

-type error_code() :: user_does_not_exist_error | wrong_filename_error |
                      file_creation_permission_denied_error | location_is_a_directory_error.

-spec retrieve_all(jid:user(), jid:server(), Path :: binary()) ->
    ok | {error_code(), Reason :: string()}.
retrieve_all(Username, Domain, ResultFilePath) ->
    JID = jid:make_bare(Username, Domain),
    case user_exists(JID) of
        true ->
            DataFromModules = get_data_from_modules(JID),
            % The contract is that we create personal data files only when there are any items
            % returned for the data group.
            DataToWrite = lists:filter(fun({_, _, Items}) -> Items /= [] end, DataFromModules),

            TmpDir = make_tmp_dir(),

            CsvFiles = lists:map(
                fun({DataGroup, Schema, Entries}) ->
                    BinDataGroup = atom_to_binary(DataGroup, utf8),
                    FileName = <<BinDataGroup/binary, ".csv">>,
                    to_csv_file(FileName, Schema, Entries, TmpDir),
                    binary_to_list(FileName)
                end,
                DataToWrite),

            LogFiles = get_all_logs(Username, Domain, TmpDir),
            ZipFile = binary_to_list(ResultFilePath),
            try
                {ok, ZipFile} = zip:create(ZipFile, CsvFiles ++ LogFiles, [{cwd, TmpDir}]),
                remove_tmp_dir(TmpDir),
                ok
            catch
                _:Reason ->
                    process_error(Reason, ZipFile)
            end;
        false ->
            {user_does_not_exist_error, "User does not exist"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                       Private funs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_error({badmatch, {error, enoent}}, ZipFile) ->
    ErrorMessage = "It is impossible to create file named '" ++ ZipFile ++ "'",
    {wrong_filename_error, ErrorMessage};
process_error({badmatch, {error, ErrorCode}}, ZipFile)
    when ErrorCode =:= eacces orelse ErrorCode =:= erofs ->
        ErrorMessage = "Permission to create file in location '" ++ ZipFile ++ "' denied",
        {file_creation_permission_denied_error, ErrorMessage};
process_error({badmatch, {error, eisdir}}, ZipFile) ->
    ErrorMessage = "Given location '" ++ ZipFile ++ "' is a directory",
    {location_is_a_directory_error, ErrorMessage};
process_error({badmatch, {error, eexist}}, ZipFile) ->
    ErrorMessage = "File '" ++ ZipFile ++ "' already exist",
    {file_already_exist_error, ErrorMessage};
process_error({badmatch, {error, {'EXIT', {{badmatch, {error, ErrorCode}}, _}}}}, ZipFile)
    when ErrorCode =:= eacces orelse ErrorCode =:= erofs ->
        ErrorMessage = "Permission to create file in location '" ++ ZipFile ++ "' denied",
        {file_creation_permission_denied_error, ErrorMessage}.

-spec get_data_from_modules(jid:user(), jid:server()) -> gdpr:personal_data().
get_data_from_modules(Username, Domain) ->
    JID = jid:make_bare(Username, Domain),
    get_data_from_modules(JID).

-spec get_data_from_modules(jid:jid()) -> gdpr:personal_data().
get_data_from_modules(JID) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(JID#jid.lserver),
    mongoose_hooks:get_personal_data(HostType, JID).

-spec to_csv_file(file:name_all(), gdpr:schema(), gdpr:entries(), file:name()) -> ok.
to_csv_file(Filename, DataSchema, DataRows, TmpDir) ->
    FilePath = <<(list_to_binary(TmpDir))/binary, "/", Filename/binary>>,
    {ok, File} = file:open(FilePath, [write]),
    Encoded = erl_csv:encode([DataSchema | DataRows]),
    file:write(File, Encoded),
    file:close(File).

-spec user_exists(jid:jid()) -> boolean().
user_exists(JID) ->
    ejabberd_auth:does_user_exist(JID).

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

-spec retrieve_logs(gdpr:username(), mongooseim:domain_name()) -> {ok, ZippedLogs :: binary()}.
retrieve_logs(Username, Domain) ->
    TmpDir = make_tmp_dir(),
    LogFile = get_logs(Username, Domain, TmpDir),
    {ok, {_, ZippedLogs}} = zip:create("archive.zip", [LogFile], [memory, {cwd, TmpDir}]),
    remove_tmp_dir(TmpDir),
    {ok, ZippedLogs}.

-spec get_all_logs(gdpr:username(), mongooseim:domain_name(), file:name()) -> [file:name()].
get_all_logs(Username, Domain, TmpDir) ->
    OtherNodes = mongoose_cluster:other_cluster_nodes(),
    LogFile = get_logs(Username, Domain, TmpDir),
    LogFilesFromOtherNodes = [get_logs_from_node(Node, Username, Domain, TmpDir) || Node <- OtherNodes],
    [LogFile | LogFilesFromOtherNodes].

-spec get_logs(gdpr:username(), mongooseim:domain_name(), file:name()) -> file:name().
get_logs(Username, Domain, TmpDir) ->
    FileList = [filename:absname(F) || F <- mongoose_logs:get_log_files()],
    Cmd = code:priv_dir(mongooseim) ++ "/parse_logs.sh",
    FileName = "logs-" ++ atom_to_list(node()) ++ ".txt",
    FilePath = TmpDir ++ "/" ++ FileName,
    Args = [FilePath, Username, Domain | FileList],
    0 = run(Cmd, Args, ?CMD_TIMEOUT),
    FileName.

-spec get_logs_from_node(node(), gdpr:username(), mongooseim:domain_name(), file:name()) -> file:name().
get_logs_from_node(Node, Username, Domain, TmpDir) ->
    {ok, ZippedData} = rpc:call(Node, ?MODULE, retrieve_logs, [Username, Domain]),
    {ok, [File]} = zip:unzip(ZippedData, [{cwd, TmpDir}]),
    filename:basename(File).
