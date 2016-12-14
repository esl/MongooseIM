#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa deps/cqerl/ebin deps/exml/ebin
%% This^ code path is needed to use v those include_lib
-include_lib("cqerl/include/cqerl.hrl").
-include_lib("exml/include/exml.hrl").

%% I/O macros
-define(STDOUT(MSG), ?STDOUT(MSG, [])).
-define(STDOUT(MSG, FMT), ?WRITE_DEV(standard_io, MSG, FMT)).
-define(STDERR(MSG), ?STDERR(MSG, [])).
-define(STDERR(MSG, FMT), ?WRITE_DEV(standard_error, MSG, FMT)).
-define(WRITE_DEV(DEV, MSG, FMT), io:format(DEV, MSG ++ "~n", FMT)).
-define(PROGRESS(MSG, FMT), io:format("~c" ++ MSG, [13 | FMT])).

%% Defaults
-define(DEFAULT_SOURCE_KEYSPACE, "mam").
-define(DEFAULT_TARGET_KEYSPACE, "mongooseim").
-define(DEFAULT_ENCODER_MODULE, "mam_message_eterm").

%% Types definitions
-type db_handle() :: term().
-type table() :: atom().
-type row() :: proplists:proplist().
-type script_options() :: #{OptionName :: atom() => OptionValue :: string()}.

%% Script has to be run from MIM project root after fetching and building all dependencies.
-spec usage(normal | any()) -> no_return().
usage(Reason) ->
    Usage =
        "Usage: " ++ escript:script_name() ++ " [options...]~n"
        "Available options are:~n"
        " [req] --source_host HOST:PORT - Source database configuration, e.g. '127.0.0.1:9042'~n"
        " [req] --target_host HOST:PORT - Target database configuration, e.g. '127.0.0.1:9042'~n"
        " [opt] --source_ks KEYSPACE    - Source database keyspace, defaults to: '"
        ++ ?DEFAULT_SOURCE_KEYSPACE ++ "'~n"
        " [opt] --target_ks KEYSPACE    - Target database keyspace, defaults to: '"
        ++ ?DEFAULT_TARGET_KEYSPACE ++ "'~n"
        " [opt] --msg_encoder MODULE    - Module used to encode/decode messages, defaults to: '"
        ++ ?DEFAULT_ENCODER_MODULE ++ "'~n"
        " [opt] -h | --help             - Show script usage~n",
    case Reason of
        normal ->
            ?STDOUT(Usage),
            halt(0);
        _ ->
            ?STDERR("Invalid script usage: ~p", [Reason]),
            ?STDERR(Usage),
            halt(1)
    end.

-spec main([ Arg :: string() ]) -> ok | no_return().
main(Args) ->
    ok = set_code_path("."), %% Include all deps of MIM application
    %% (since cqerl used in this script also have several dependencies)
    cqerl_app:start(normal, []),
    ets:new(options, [named_table, public, set]),


    {SourceClient, TargetClient} =
        try
            Options = parse_args(Args),

            SourceURI = maps:get(source_host, Options),
            TargetURI = maps:get(target_host, Options),
            SourceKeyspace = maps:get(source_ks, Options, ?DEFAULT_SOURCE_KEYSPACE),
            TargetKeyspace = maps:get(target_ks, Options, ?DEFAULT_TARGET_KEYSPACE),

            MessageEncoder = list_to_atom(maps:get(msg_encoder, Options, ?DEFAULT_ENCODER_MODULE)),
            ets:insert(options, {msg_encoder, MessageEncoder}),

            %% Connect to both source and target database
            {
              connect_to(SourceURI, SourceKeyspace),
              connect_to(TargetURI, TargetKeyspace)
            }
        catch
            error:{badkey, BadKey} ->
                usage({badkey, BadKey})
        end,

    %% Start data transfer
    ReportTo = self(),
    TablesToCopy = [mam_con_message, mam_muc_message],

    lists:foreach(
      fun(Table) ->
              async_copy_table(ReportTo, SourceClient, TargetClient, Table),
              ok = report_progress(Table)
      end, TablesToCopy).

%% Collect and report progress on stdout for given table
-spec report_progress(table()) -> ok.
report_progress(Table) ->
    report_progress(Table, #{inserted => 0, errors => []}).

-spec report_progress(table(), State :: #{atom() => term()}) -> ok.
report_progress(Table, #{inserted := Inserted, errors := Errors} = State) ->
    BaseMsg = "Copying table '~s', items copied: ~p... ",
    BaseFmt = [Table, Inserted],

    ?PROGRESS(BaseMsg, BaseFmt),
    receive
        {Table, done} ->
            case Errors of
                [] ->
                    ?PROGRESS(BaseMsg ++ "Finished successfully.~n", BaseFmt);
                [{Error, _} | _] ->
                    {_, AllRows0} = lists:unzip(Errors),
                    _AllRows = lists:concat(AllRows0), %% @todo: maybe dump those rows to disk
                    %%        for debugging purpose?
                    ?PROGRESS(BaseMsg ++ "Error: ~p~n", BaseFmt ++ [Error])
            end,
            ok;
        {Table, {insert, BatchSize}} ->
            report_progress(Table, maps:put(inserted, Inserted + BatchSize, State));
        {Table, {error, Error, Rows}} ->
            report_progress(Table, maps:put(errors, [{Error, Rows} | Errors]))
    after timer:minutes(5) ->
            report_progress(Table, maps:put(errors, [{timeout, []} | Errors])),
            self() ! {Table, done}
    end.

-spec async_copy_table(ReportTo :: pid(), SourceClient :: db_handle(),
                       TargetClient :: db_handle(), table()) -> Worker :: pid().
async_copy_table(ReportTo, SourceClient, TargetClient, Table) ->
    TableName = atom_to_list(Table),
    spawn_link(
      fun() ->
              %% Initial query
              {ok, Result} = cqerl:run_query(SourceClient,
                                             "SELECT * FROM " ++ TableName),
              copy_rows(ReportTo, TargetClient, Table, Result)
      end).

%% Process each page (defult is 100 rows) of SELECT results
-spec copy_rows(ReportTo :: pid(), TargetClient :: db_handle(), table(), DBResult :: term()) ->
                       ok.
copy_rows(ReportTo, TargetClient, Table, DBResult) ->
    Rows = cqerl:all_rows(DBResult),
    ok = batch_copy_rows(ReportTo, TargetClient, Table, Rows),
    case cqerl:has_more_pages(DBResult) of
        true ->
            {ok, DBResult2} = cqerl:fetch_more(DBResult),
            copy_rows(ReportTo, TargetClient, Table, DBResult2);
        false ->
            ReportTo ! {Table, done},
            ok
    end.

%% Insert given, previously selected set of rows
-spec batch_copy_rows(ReportTo :: pid(), TargetClient :: db_handle(), table(), [row()]) ->
                             ok.
batch_copy_rows(_ReportTo, _TargetClient, _Table, []) ->
    ok;
batch_copy_rows(ReportTo, TargetClient, Table, Rows) ->
    %% Convert rows to new schema
    NewRows0 = pmap(fun(Row) -> convert_row(Table, Row) end, Rows),

    %% Each row proplist is wrraped in {} to enable use of lists:flatten
    [Hd | _] = NewRows = lists:map(fun({Row}) -> Row end, lists:flatten(NewRows0)),

    %% Prepare INSERT statement
    TargetTableName = atom_to_list(map_table(Table)),
    Columns = proplists:get_keys(Hd),
    {ColumnNames, ColumnValues} = lists:unzip([{atom_to_list(Column), "?"}
                                               || Column <- Columns]),
    ColumnNamesString = string:join(ColumnNames, ", "),
    ColumnValuesString = string:join(ColumnValues, ", "),
    InsertStatement = "INSERT INTO " ++ TargetTableName
        ++ "(" ++ ColumnNamesString ++ ")"
        ++ " VALUES("++ ColumnValuesString ++");",


    InsertQuery = #cql_query{statement = InsertStatement},
    InitialBatchSize = 15,
    %% length(NewRows) should be max 400, since cassandra's
    %% default 'get' page size is 100.
    %% Therefore there will be ~400/InitialBatchSize concurrent insert batches.
    RowBatches = make_batch(NewRows, InitialBatchSize),

    %% Process each batch in parallel and report progress
    pmap(fun(RowBatch) ->
                 case catch insert_rows(TargetClient, InsertQuery, RowBatch, length(RowBatch)) of
                     ok ->
                         ReportTo ! {Table, {insert, length(RowBatch)}};
                     Error ->
                         ReportTo ! {Table, {error, Error, RowBatch}}
                 end
         end, RowBatches),
    ok.

insert_rows(_, _, [], _) ->
    ok;
insert_rows(TargetClient, InsertQuery, Rows, BatchSize) ->
    {NewRows, Tail} = lists:split(min(BatchSize, length(Rows)), Rows),
    Result =
        cqerl:run_query(TargetClient, #cql_query_batch{
                                         mode = unlogged,
                                         queries = [InsertQuery#cql_query{values = NewRow}
                                                    || NewRow <- NewRows]
                                        }),
    case Result of
        {error, {8704, _, _}} -> %% Batch too large
            NewBatchSize = max(1, round(BatchSize * 0.75)),
            insert_rows(TargetClient, InsertQuery, Rows, NewBatchSize);
        {ok, _} ->
            insert_rows(TargetClient, InsertQuery, Tail, BatchSize)
    end.


map_table(mam_con_message) -> mam_message;
map_table(mam_muc_message) -> mam_muc_message.

convert_row(mam_con_message, Row) ->
    Id = proplists:get_value(id, Row),
    IsFromLower = proplists:get_value(is_from_lower, Row),
    Message = proplists:get_value(message, Row),
    LowerJID = proplists:get_value(lower_jid, Row),
    UpperJID = proplists:get_value(upper_jid, Row),

    {ToJID, FromJID} = case IsFromLower of
                           true -> {UpperJID, LowerJID};
                           false -> {LowerJID, UpperJID}
                       end,

    JIDs = [
            %UserJID | FromJID | RemoteJID | WithJID
            {FromJID, FromJID,  ToJID,      <<>>},     %% Outgoing message
            {ToJID,   FromJID,  FromJID,    <<>>},     %% Incoming message
            {FromJID, FromJID,  ToJID,      FromJID},  %% Outgoing message
            {ToJID,   FromJID,  FromJID,    FromJID}   %% Incoming message
           ],


    [{[
       {id, Id},
       {user_jid, UserJID},
       {from_jid, SFromJID},
       {remote_jid, RemoteJID},
       {with_jid, WithJID},
       {message, Message}
      ]} || {UserJID, SFromJID, RemoteJID, WithJID} <- JIDs];
convert_row(mam_muc_message, Row) ->
    Id = proplists:get_value(id, Row),
    Nickname = proplists:get_value(nick_name, Row),
    Message = proplists:get_value(message, Row),
    RoomId = proplists:get_value(room_id, Row),

    [{_, Encoder}] = ets:lookup(options, msg_encoder),
    #xmlel{attrs = MessageAttrs} = Encoder:decode(Message),
    RoomJID = proplists:get_value(<<"to">>, MessageAttrs),

    [{[
       {id, Id},
       {room_jid, RoomJID},
       {nick_name, Nickname},
       {with_nick, WithJID},
       {message, Message}
      ]} || WithJID <- [<<"">>, Nickname]].


%% SCRIPT SETUP FUNCTIONS

%% Load code of all rebar deps of current project
-spec set_code_path(Prefix :: string()) -> ok | no_return().
set_code_path(Prefix) ->
    DepsPath = filename:join(Prefix, "deps"),
    {ok, Deps} = file:list_dir(DepsPath),
    [code:add_patha(filename:join([DepsPath, DepName, "ebin"])) || DepName <- Deps],
    code:add_patha(filename:join([Prefix, "apps", "ejabberd", "ebin"])),
    ok.

%% Parse command line arguments into option map
-spec parse_args([Arg :: any()]) -> script_options().
parse_args(Args) ->
    BinArgs = [list_to_binary(Arg) || Arg <- Args],
    try parse_args(BinArgs, #{}) of
        Options -> Options
    catch
        _:Reason ->
            usage(Reason)
    end.

-spec parse_args([Arg :: any()], Acc :: script_options()) -> Options :: script_options().
parse_args([], Options) ->
    Options;
parse_args([<<"--help">> | _], _Options) ->
    usage(normal);
parse_args([<<"-h">> | _], _Options) ->
    usage(normal);
parse_args([<<"--", OptName/binary>> | Tail], Options) ->
    case binary:split(OptName, <<"=">>) of
        [OptName1, OptValue1] ->
            parse_args(Tail,
                       maps:put(binary_to_atom(OptName1, utf8),
                                binary_to_list(OptValue1), Options));
        [OptName] ->
            [OptValue | Tail1] = Tail,
            parse_args(Tail1,
                       maps:put(binary_to_atom(OptName, utf8),
                                binary_to_list(OptValue), Options))

    end.

%% Connect to given cassandra master node
-spec connect_to(URI :: string(), Keyspace :: string()) ->
                        ConnectionHandle :: term().
connect_to(URI, Keyspace) ->
    case cqerl:get_client(URI, [{keyspace, list_to_atom(Keyspace)}]) of
        {ok, Client} ->
            Client;
        {error, {shutdown, {_, _, {connection_error, ConnError}}}} ->
            ?STDERR("Error while connecting to ~p: ~p", [URI, ConnError]),
            halt(1);
        {error, shutdown} ->
            ?STDERR("Unknown error while connecting to ~p. "
                    "Make sure that selected keyspace (~p) is valid.", [URI, Keyspace]),
            halt(1)
    end.


%% UTILITY FUNCTIONS

%% Split given list into batches with size BatchSize
-spec make_batch([any()], BatchSize :: non_neg_integer()) ->
                        [[any()]].
make_batch(List, BatchSize) ->
    make_batch(List, BatchSize, []).

-spec make_batch([any()], BatchSize :: non_neg_integer(), Acc :: term()) ->
                        [[any()]].
make_batch([], _, Acc) ->
    lists:reverse(Acc);
make_batch(List, BatchSize, Acc) ->
    {Batch, Tail} = lists:split(min(BatchSize, length(List)), List),
    make_batch(Tail, BatchSize, [Batch | Acc]).

%% Parallel map implementation
-spec pmap(fun((term()) -> term()), [any()]) -> [term()].
pmap(Fun, List) ->
    Host = self(),
    Pids = [spawn_link(fun() -> Host ! {self(), Fun(Arg)} end) || Arg <- List],
    [receive {Pid, Res} -> Res end || Pid <- Pids].
