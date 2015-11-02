#!/usr/bin/env escript
%%!  -s inets start
%% -*- erlang -*-
%%
%% Example usage:
%% add attribute `xep` to relevant module as below:
%% -xep([{xep, 313}, {version, "0.2"}]).
%% or
%% -xep([{xep, 313}, {version, "0.2"}, {url, "http://xmpp.org/extensions/attic/xep-0313-0.2.html"}]).
%% you can add also comment to the xep like:
%% -xep([{xep, 313}, {version, "0.2"}, {comment, "Some example comment, for example: Partial Implemented}]).
%% and run:
%% $> escript xep_tool.escript <PATH_TO_BEAM> <OUTPUT_FILE>
%% Escript will produce markdown with list of available XEPs.

-export([get_xep/1]).
-mode(compile).
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

-spec match_zeros(xep()) -> string().
match_zeros(Number) when Number > 99 ->
    "0" ++ integer_to_list(Number);
match_zeros(Number) when Number > 9 ->
    "00" ++ integer_to_list(Number);
match_zeros(Number) ->
    "000" ++ integer_to_list(Number).

-spec main(list(string())) -> ok.
main([InDir]) ->
    code:set_path(code:get_path() ++ [InDir]),
    do(?FILE_NAME);

main([InDir, Output]) ->
    code:set_path(code:get_path() ++ [InDir]),
    do(Output);

main(_) ->
    io:format("~n ***  Bad Arguments. Try:~nxep_tool.escript <path_to_ebin_files>"
    "[output_file]~n~nExample usage (from mongooseim repo root):
./tools/xep_tool/xep_tool.escript apps/ejabberd/ebin/ Xep_list.md~n").

-spec do(string()) -> ok.
do(Output) ->
    EjabberdAPP = code:where_is_file("ejabberd.app"),
    case file:consult(EjabberdAPP) of
        {ok, [{application, ejabberd, FileStruct}]} ->
            [Modules] = [M || {modules, M} <- FileStruct],
            XEPs = lists:map(fun get_xep/1, Modules),
            FilteredXeps = lists:filtermap(fun({_, []}) -> false;
                                              ({Module, List}) -> {true, {Module, List}}
                                           end, XEPs),
            ToRecord = lists:map(fun({Module, List}) ->
                #module_xep{module = Module, xeps = [proplist_to_half_record(X) || X <- List]}
                                 end, FilteredXeps),
            UsortedTupleList = usort_all(ToRecord),
            RecordList = generate_full_record_list(UsortedTupleList),
            Table = generate_table(RecordList),
            file:write_file(Output, Table),
            io:format("~n ***  Markdown with unique xep names and urls saved to file ~p~n",
                      [Output]);
        _ ->
            io:format("~n ***  Beam files not found. First compile MongooseIM to generate beam files~n")
    end.

-spec usort_all(list(#module_xep{})) -> list({{xep(), ver()}, url()}).
usort_all(ModuleXepList) ->
    F = fun(Rec, Dict) ->
        List = [{{XepNumber, Version}, Url} ||
            #xep{xep = XepNumber, version = Version, url = Url} <- Rec#module_xep.xeps],
        NoDup = lists:ukeysort(1, List),
        lists:umerge([Dict, NoDup])
    end,
    lists:foldl(F, [], ModuleXepList).

-spec generate_full_record_list(list({{xep(), ver()}, url()})) -> list(#xep{}).
generate_full_record_list(HalfRecordList) ->
    F = fun({{Xep, Ver}, Url}) ->
        {Name, MasterVer} = get_title_and_ver(Url),
        NewUrl = if MasterVer == Ver ->
            Url;
                     true ->
                         case Ver of
                             undefined ->
                                 Url;
                             SomeVer ->
                                 ?HTTP_PREFIX_VER ++
                                 match_zeros(Xep) ++
                                 "-" ++ SomeVer ++ ?HTTP_SUFIX
                         end
                 end,
        #xep{xep = Xep, version = Ver, url = NewUrl, name = Name}
        end,
        lists:map(F, HalfRecordList).

-spec generate_table(list(#xep{})) -> string().
generate_table(List) ->
    TablePrefix = "|||||\n"
                  "| ------------- | ------------- | ------------- |------------- |\n"
                  "|",
    F = fun(#xep{name=Name, url=Url}, {Num, BuildingTable}) ->
        Add = case Num rem 4 of
                  0 ->
                      "\n";
                  _ ->
                      " "
              end,
        {Num + 1, BuildingTable ++ "[" ++ Name ++ "](" ++ Url ++ ") |" ++ Add}
    end,
    {_, TableListElement} = lists:foldl(F, {1, ""}, List),
    TablePrefix ++ TableListElement.

-spec get_title_and_ver(url()) -> {name(), ver()}.
get_title_and_ver(URL) ->
    io:format("Getting XEP name for ~p~n", [URL]),
    {ok, {{_, 200, _}, _, Body}} =
    httpc:request(URL),
    {match, [Name]} = re:run(Body, "<title>(?<FOO>.*)</title>", [{capture, ['FOO'], list}]),
    {match, [Ver]} = re:run(Body, "Version: (?<FOO>.*)<br", [{capture, ['FOO'], list}]),
    {Name, Ver}.


-spec proplist_to_half_record(list({atom(), any()})) -> #xep{}.
proplist_to_half_record(Proplist) ->
    XEP = case lists:keysearch(xep, 1, Proplist) of
              {value, {?XEP, V}} ->
                  V;
              false -> undefined
          end,
    VER = case lists:keysearch(version, 1, Proplist) of
              {value, {?VERSION, V1}} ->
                  V1;
              false -> undefined
          end,
    MasterURL = case lists:keysearch(?URL, 1, Proplist) of
                    {value, {url, Value}} ->
                        Value;
                    false ->
                        ?HTTP_PREFIX ++ match_zeros(XEP) ++ ?HTTP_SUFIX

                end,
    #xep{xep = XEP, version = VER, url = MasterURL}.

