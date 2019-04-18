#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Example usage:
%% add attribute `xep` to relevant module as below:
%% -xep([{xep, 313}, {version, "0.2"}]).
%% or
%% -xep([{xep, 313}, {version, "0.2"},
%% { url, "http://xmpp.org/extensions/attic/xep-0313-0.2.html"}]).
%%
%% you can add also comment to the xep like:
%% -xep([{xep, 313}, {version, "0.2"},
%% {comment, "Some example comment, for example: Partial Implemented}]).
%%
%% and run:
%% $> escript xep_tool.escript <PATH_TO_BEAM> <OUTPUT_FILE>
%% Escript will produce markdown with list of available XEPs.
-define(HTTP_PREFIX, "http://www.xmpp.org/extensions/xep-").
-define(HTTP_PREFIX_VER, "http://xmpp.org/extensions/attic/xep-").
-define(HTTP_SUFIX, ".html").
-define(XEP, xep).
-define(VERSION, version).
-define(URL, url).
-define(FILE_NAME, "Xep_List.md").
-define(MAX_LENGTH, 100).
-type ver() :: string().
-type xep() :: integer().
-type url() :: string().
-type name() :: string().
-record(xep, {xep :: xep(), name :: name(), url :: url(), version :: ver()}).
-record(module_xep, {xeps = [], module :: module()}).

-spec get_xep(module()) -> {module(), list({atom(), any()})}.
get_xep(Module) ->
    Info = Module:module_info(attributes),
    Values = [Value || {xep, Value} <- Info],
    {Module, Values}.

-spec match_zeros(xep()) -> iolist().
match_zeros(Number) when Number > 99 ->
    ["0", integer_to_list(Number)];
match_zeros(Number) when Number > 9 ->
    ["00", integer_to_list(Number)];
match_zeros(Number) ->
    ["000", integer_to_list(Number)].

main(Args) ->
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(inets),
    run(Args).

-spec run(list(iolist())) -> ok | no_return().
run([Command, InDir]) ->
    code:set_path(code:get_path() ++ [InDir]),
    do(Command, ?FILE_NAME);

run([Command, InDir, Output]) ->
    code:set_path(code:get_path() ++ [InDir]),
    do(Command, Output);

run(_) ->
    usage().

-spec usage() -> no_return().
usage() ->
    io:format("***  Bad Arguments. "
                "Try:~nxep_tool.escript list|markdown <path_to_ebin_files>"
                " [output_file]~n~nCommand could be one of the following: ~n"
                "list -  prints the list of the supported XEPs to the console~nmarkdown - creates markdown"
                " of supported XEPs to a file. This command requires also to indicate the output file.~n~n"
                "Example usage (from mongooseim repo root):~n"
                "./tools/xep_tool/xep_tool.escript apps/ejabberd/ebin/ "
                "Xep_list.md~n"),
    erlang:halt(1).

-spec do(string(), string()) -> ok | no_return().
do(Command, Output) ->
    MongooseApp = code:where_is_file("mongooseim.app"),
    case file:consult(MongooseApp) of
        {ok, [{application, mongooseim, FileStruct}]} ->
            case Command of
                "markdown" ->
                    Table = filestruct_to_table(FileStruct),
                    file:write_file(Output, Table),
                    io:format("~n ***  Markdown with unique xep names "
                              "and urls saved to file ~p~n",
                              [Output]);
                "list" ->
                    ToPrint = filestruct_to_output_list(FileStruct),
                    io:format("~nList of supported XEPs with versions: ~n"),
                    io:format(ToPrint);
                _ ->
                    usage()
            end;
        _ ->
            io:format("~n ***  Beam files not found."
                      " First compile MongooseIM to generate beam files~n")
    end.

-spec to_usorted_tuple_list(list(#module_xep{})) ->
    list({{xep(), ver()}, url()}).
to_usorted_tuple_list(ModuleXepList) ->
    F = fun(#module_xep{xeps = Xeps}) ->
        [{{X#xep.xep, X#xep.version}, X#xep.url} || X <- Xeps]
    end,
    XepList = lists:flatmap(F, ModuleXepList),
    lists:ukeysort(1, XepList).

-spec tuple_to_full_record({{xep(), ver()}, url()}) -> #xep{}.
tuple_to_full_record({{Xep, Ver}, Url}) ->
    {Name, MasterVer} = get_title_and_ver(Url),
    NewUrl = case MasterVer of
                 Ver ->
                     Url;
                 _ ->
                     case Ver of
                         undefined ->
                             Url;
                         SomeVer ->
                             [?HTTP_PREFIX_VER,
                              match_zeros(Xep),
                              "-", SomeVer, ?HTTP_SUFIX]
                     end
             end,
    #xep{xep = Xep, version = Ver, url = NewUrl, name = Name}.

-spec proplist_to_half_record(list({atom(), any()})) -> #xep{}.
proplist_to_half_record(Proplist) ->
    XEP = case lists:keyfind(xep, 1, Proplist) of
              {?XEP, V} ->
                  V;
              false ->
                  undefined
          end,
    VER = case lists:keyfind(version, 1, Proplist) of
              {?VERSION, V1} ->
                  V1;
              false ->
                  undefined
          end,
    MasterURL = case lists:keyfind(?URL, 1, Proplist) of
                    {url, Value} ->
                        Value;
                    false ->
                        [?HTTP_PREFIX, match_zeros(XEP), ?HTTP_SUFIX]
                end,
    #xep{xep = XEP, version = VER, url = MasterURL}.

-spec generate_full_record_list(list({{xep(), ver()}, url()})) -> list(#xep{}).
generate_full_record_list(HalfRecordList) ->
    [tuple_to_full_record(HalfRecord) || HalfRecord <- HalfRecordList].

-spec generate_module_xep_list({module(), list(#xep{})}) -> #module_xep{}.
generate_module_xep_list({Module, List}) ->
    HalfRecords = [proplist_to_half_record(X) || X <- List],
    #module_xep{module = Module, xeps   = HalfRecords}.

-spec filestruct_to_record_list(list({modules, module()})) -> list(#xep{}).
filestruct_to_record_list(FileStruct) ->
    [Modules] = [M || {modules, M} <- FileStruct],
    XEPs = [get_xep(M) || M <- Modules],
    FilteredXeps = [X || X = {_, List} <- XEPs, [] =/= List],
    ToRecord = [generate_module_xep_list(M)|| M <- FilteredXeps],
    UsortedTupleList = to_usorted_tuple_list(ToRecord),
    generate_full_record_list(UsortedTupleList).

-spec filestruct_to_table(list({modules, module()})) -> iolist().
filestruct_to_table(FileStruct) ->
    RecordList = filestruct_to_record_list(FileStruct),
    generate_table(RecordList).

-spec filestruct_to_output_list(list({modules, module()})) -> iolist().
filestruct_to_output_list(FileStruct) ->
    RecordList = filestruct_to_record_list(FileStruct),
    record_list_to_output_list(RecordList).

-spec record_list_to_output_list(list(#xep{})) -> iolist().
record_list_to_output_list(RecordList) ->
    SimpleList = [{Name, Version}|| #xep{name=Name, version=Version} <- RecordList],
    io_lib:format("~p~n", [SimpleList]).


-spec generate_table(list(#xep{})) -> iolist().
generate_table(List) ->
    F = fun(#xep{name = Name, url = Url}, {Num, BuildingTable}) ->
        Add = case Num rem 4 of
                  0 ->
                      "\n";
                  _ ->
                      " "
              end,
        {Num + 1, [BuildingTable, "[", Name, "](", Url, ") |", Add]}
    end,
    {_, TableListElement} = lists:foldl(F, {1, ""}, List),
    [generate_prefix(), TableListElement].

-spec get_title_and_ver(url()) -> {name(), ver()}.
get_title_and_ver(URL) ->
    io:format("Getting XEP name for ~s~n", [URL]),
    LUrl = lists:flatten(URL),
    {ok, {{_, 200, _}, _, Body}} = httpc:request(LUrl),
    {match, [Name]} = re:run(Body, "<title>(?<FOO>.*)</title>",
                             [{capture, ['FOO'], list}]),
    {match, [Ver]} = re:run(Body, "Version: (?<FOO>.*)<br",
                            [{capture, ['FOO'], list}]),
    {Name, Ver}.

-spec generate_prefix() -> string().
generate_prefix() ->
    "|||||\n"
    "|-------------|-------------|-------------|-------------|\n"
    "|".
