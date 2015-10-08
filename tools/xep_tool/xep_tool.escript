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
-record(xep, {xep = none, name, url, version}).
-record(module_xep, {xeps = [], module}).

get_xep(Module) ->
    Info = Module:module_info(attributes),
    Values = [Value || {xep, Value} <- Info],
    {Module, Values}.

match_zeros(Number) when Number > 99 ->
    "0" ++ integer_to_list(Number);
match_zeros(Number) when Number > 9 ->
    "00" ++ integer_to_list(Number);
match_zeros(Number) ->
    "000" ++ integer_to_list(Number).

main([InDir]) ->
    code:set_path(code:get_path() ++ [InDir]),
    do(?FILE_NAME);

main([InDir, Output]) ->
    code:set_path(code:get_path() ++ [InDir]),
    do(Output);

main(_) ->
    io:format("~n ***  Set path to ebin files as an argument~n").

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
                #module_xep{module = Module, xeps = [proplist_to_xep_record(X) || X <- List]}
                                 end, FilteredXeps),
            Map = generate_map(ToRecord),
            Table = generate_table(Map),
            file:write_file(Output, Table),
            io:format("~n ***  Markdown with unique xep names and urls saved to file ~p~n", [Output]);
        _ ->
            io:format("~n ***  Beam files not found. First compile MongooseIM to generate beam files~n")
    end.

generate_map(ModuleXepList) ->
    F = fun(Rec, Dict) ->
        XEPTuple = [{Number, Name, Url} || #xep{xep = Number, name = Name, url = Url} <- (Rec#module_xep.xeps)],
        lists:foldl(fun({A, B, C}, Acc) -> maps:put(A, {B, C}, Acc) end, Dict, XEPTuple)
    end,
    lists:foldl(F, maps:new(), ModuleXepList).

generate_table(Map) ->
    TablePrefix = "|||||\n"
                  "| ------------- | ------------- | ------------- |------------- |\n"
                  "|",
    F = fun(_, {Name, Url}, {Num, BuildingTable}) ->
        Add = case Num rem 4 of
                  0 ->
                      "\n";
                  _ ->
                      " "
              end,
        {Num + 1, BuildingTable ++ "[" ++ Name ++ "](" ++ Url ++ ") |" ++ Add}
    end,
    {_, TableListElement} = maps:fold(F, {1, ""}, Map),
    TablePrefix ++ TableListElement.

get_title_and_ver(URL) ->
    io:format("Getting XEP name for ~p~n", [URL]),
    {ok, {{_, 200, _}, _, Body}} =
    httpc:request(URL),
    {match, [Title]} = re:run(Body, "<title>(?<FOO>.*)</title>", [{capture, ['FOO'], list}]),
    {match, [Ver]} = re:run(Body, "Version: (?<FOO>.*)<br", [{capture, ['FOO'], list}]),
    {Title, Ver}.



proplist_to_xep_record(Proplist) ->
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
    {NAME, XepMasterVersion} = get_title_and_ver(MasterURL),
    URL = if XepMasterVersion == VER -> MasterURL; true ->
        case VER of
            undefined ->
                MasterURL;
            SomeVer ->
                ?HTTP_PREFIX_VER ++ match_zeros(XEP) ++ "-" ++ SomeVer ++ ?HTTP_SUFIX
        end
          end,
    #xep{xep = XEP, name = NAME, url = URL, version = VER}.

