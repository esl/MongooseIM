#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% For each module implementing a XEP, add a `xep` attribute as below:
%% -xep([{xep, 313}, {version, "0.6"}]).
%%
%% For a ProtoXEP, provide the identifier as an atom:
%% -xep([{xep, 'muc-light'}, {version, "0.0.1"}]).
%%
%% If the implementation is partial, add '{status, partial}' to the list.
%% You can add supported legacy versions as well, e.g. '{legacy_versions, ["0.2", "0.4.1"]}'.
%%
%% Usage:
%% $> escript xep_tool.escript <FORMAT> [<OUTPUT_FILE>]
%% Escript will produce a list of XEPs in the specified format ('markdown', 'list' or 'doap').

-define(HTTP_PREFIX, "https://xmpp.org/extensions/").
-define(HTTP_SUFFIX, ".html").

-type ver() :: string().
-type xep() :: 1..9999 | atom().
-type url() :: string().
-type name() :: string().
-type status() :: complete | partial. % subset of the values from XEP-0453

-include_lib("kernel/include/file.hrl").

-record(xep, {xep :: xep(),
              name :: name(),
              url :: url(),
              version :: ver(),
              modules :: [module()],
              status :: status()}).

main(Args) ->
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(inets),
    run(Args).

-spec run([iolist()]) -> ok.
run([OutputFormat | Rest]) when length(Rest) =< 1 ->
    code:add_paths(ebin_paths()),
    case Rest of
        [] ->
            do(OutputFormat, standard_io);
        [FileName] ->
            case file:open(FileName, [write]) of
                {ok, Output} ->
                    do(OutputFormat, Output),
                    io:format("Output saved to file ~p~n", [FileName]),
                    file:close(Output);
                {error, Error} ->
                    io:format("Could not open file ~p~nError: ~p~n~n", [FileName, Error]),
                    usage()
            end
    end;
run(_) ->
    usage().

-spec ebin_paths() -> [file:filename_all()].
ebin_paths() ->
    ["xep_tool.escript", "xep_tool", "tools" | RevPath] =
        lists:reverse(filename:split(escript:script_name())),
    Dir = filename:join(lists:reverse(RevPath) ++ ["_build", "prod", "lib"]),
    case file:list_dir(Dir) of
        {ok, SubDirs} ->
            [filename:join([Dir, SubDir, "ebin"]) || SubDir <- SubDirs];
        {error, enoent} ->
            io:format("There is no 'prod' release. Run 'make' first.~n"),
            usage()
    end.

-spec usage() -> no_return().
usage() ->
    io:format("Usage:~n"
              "xep_tool.escript <output_format> [<output_file>]~n~n"
              "Output format could be one of the following: ~n"
              "  doap - DOAP (Description Of A Project)~n"
              "  list - Erlang list~n"
              "  markdown - Markdown table~n"
              "  json - JSON~n~n"
              "Example usage (from mongooseim repo root):~n"
              "tools/xep_tool/xep_tool.escript doap doc/mongooseim.doap~n"),
    erlang:halt(1).

-spec do(string(), file:io_device() | standard_io) -> ok.
do(OutputFormat, Output) ->
    MongooseApp = code:where_is_file("mongooseim.app"),
    case file:consult(MongooseApp) of
        {ok, [{application, mongooseim, FileStruct}]} ->
            Modules = proplists:get_value(modules, FileStruct),
            Records = modules_to_record_list(Modules),
            Content = generate_output(OutputFormat, Records),
            file:write(Output, Content);
        _ ->
            io:format("There is no 'mongooseim.app' file. Run 'make' first.~n"),
            usage()
    end.

-spec modules_to_record_list([module()]) -> [#xep{}].
modules_to_record_list(Modules) ->
    AllXeps = all_xep_map(),
    XepRecords = lists:flatmap(fun(M) -> get_module_xeps(M, AllXeps) end, Modules),
    Groups = maps:values(group_xeps_by_id(XepRecords)),
    lists:keysort(#xep.xep, merge_xep_groups(Groups)).

-spec all_xep_map() -> #{xep() => {name(), ver()}}.
all_xep_map() ->
    {ok, Root} = exml:parse(iolist_to_binary(get_xep_list())),
    maps:from_list([extract_xep(XepElem) || XepElem <- exml_query:subelements(Root, <<"xep">>)]).

-spec get_xep_list() -> iodata().
get_xep_list() ->
    Dir = filename:dirname(escript:script_name()),
    FileName = filename:join(Dir, "xeplist.xml"),
    case file:read_file_info(FileName) of
        {ok, #file_info{mtime = {Date, _Time}}} ->
            case date() of
                Date ->
                    {ok, Content} = file:read_file(FileName),
                    Content;
                _ ->
                    download_xep_list(FileName) % XEP list is updated daily, download the new one
            end;
        {error, enoent} ->
            download_xep_list(FileName)
    end.

-spec download_xep_list(file:filename_all()) -> iodata().
download_xep_list(FileName) ->
    {ok, {{_, 200, _}, _, Body}} = httpc:request("https://xmpp.org/extensions/xeplist.xml"),
    file:write_file(FileName, Body),
    Body.

-spec extract_xep(exml:element()) -> {xep(), {name(), ver()}}.
extract_xep(Element) ->
    Name = binary_to_list(exml_query:path(Element, [{element, <<"title">>}, cdata])),
    Version = binary_to_list(exml_query:path(Element, [{element, <<"last-revision">>},
                                                       {element, <<"version">>}, cdata])),
    XepId = case exml_query:attr(Element, <<"accepted">>) of
                <<"true">> ->
                    binary_to_integer(exml_query:path(Element, [{element, <<"number">>}, cdata]));
                <<"false">> ->
                    binary_to_atom(exml_query:path(Element, [{element, <<"proto-name">>}, cdata]))
            end,
    {XepId, {Name, Version}}.

-spec group_xeps_by_id([#xep{}]) -> #{xep() => [#xep{}]}.
group_xeps_by_id(XepRecords) ->
    lists:foldl(fun(#xep{xep = XepId} = XepRec, M) ->
                        PrevRecs = maps:get(XepId, M, []),
                        maps:put(XepId, [XepRec | PrevRecs], M)
                end, #{}, XepRecords).

-spec merge_xep_groups([[#xep{}]]) -> [#xep{}].
merge_xep_groups(Groups) ->
    Results = [merge_xep_group(Group) || Group <- Groups],
    case [Conflict || {conflict, Conflict} <- Results] of
        [] ->
            Results;
        Conflicts ->
            error(#{msg => "Conflicting XEP attributes", conflicts => Conflicts})
    end.

%% It is valid to specify the same XEP in several modules, but only with the same version.
%% If the same XEP is specified as 'partial' and 'complete', the resulting status is 'complete'.
-spec merge_xep_group([#xep{}]) -> #xep{} | {conflict, [#xep{}]}.
merge_xep_group(Group = [First | _]) ->
    Modules = lists:usort(lists:flatmap(fun(Xep) -> Xep#xep.modules end, Group)),
    Versions = lists:usort(lists:map(fun(Xep) -> Xep#xep.version end, Group)),
    Statuses = lists:usort(lists:map(fun(Xep) -> Xep#xep.status end, Group)),
    case Versions of
        [_] -> case Statuses of
                   [_]  ->
                       First#xep{modules = Modules};
                   [complete, partial] ->
                       First#xep{modules = Modules, status = complete}
               end;
        [_|_] -> {conflict, Group}
    end.

get_module_xeps(Module, AllXeps) ->
    [xep_proplist_to_record(Module, XepAttr, AllXeps) || XepAttr <- get_xep_attrs(Module)].

-spec get_xep_attrs(module()) -> [proplists:proplist()].
get_xep_attrs(Module) ->
    Info = Module:module_info(attributes),
    [Value || {xep, Value} <- Info].

-spec xep_proplist_to_record(module(), proplists:proplist(), #{xep() => {name(), ver()}}) -> #xep{}.
xep_proplist_to_record(Module, Proplist, XepMap) ->
    #{xep := XepId, version := Version} = M = maps:from_list(Proplist),
    #{XepId := {Name, _LatestVersion}} = XepMap,
    #xep{xep = XepId, version = Version, url = generate_xep_url(M),
         name = Name, modules = [Module], status = get_xep_status(M)}.

-spec generate_xep_url(#{xep := xep(), _ => _}) -> string().
generate_xep_url(#{xep := XepId}) ->
    Path = xep_id_to_url_path(XepId),
    lists:flatten([?HTTP_PREFIX, Path, ?HTTP_SUFFIX]).

xep_id_to_url_path(XepId) when is_integer(XepId), XepId >= 1, XepId =< 9999 ->
    io_lib:format("xep-~4..0B", [XepId]);
xep_id_to_url_path(XepId) when is_atom(XepId) ->
    ["inbox/", atom_to_list(XepId)].

-spec get_xep_status(#{status => status(), _ => _}) -> status().
get_xep_status(#{status := complete}) -> complete;
get_xep_status(#{status := partial}) -> partial;
get_xep_status(#{status := _} = Attr) -> error(#{msg => "Invalid XEP status", attr => Attr});
get_xep_status(#{}) -> complete.

%% Output rendering

-spec generate_output(string(), [#xep{}]) -> iodata().
generate_output("markdown", Records) -> generate_table(Records);
generate_output("list", Records) -> generate_list(Records);
generate_output("doap", Records) -> generate_doap(Records);
generate_output("json", Records) -> generate_json(Records);
generate_output(_, _Records) -> usage().

-spec generate_table([#xep{}]) -> iodata().
generate_table(List) ->
    [generate_prefix(), [generate_row(Record) || Record <- List]].

-spec generate_prefix() -> string().
generate_prefix() ->
    "|XEP|Name|Version|Status|Modules|\n"
    "|---|----|-------|------|-------|\n".

-spec generate_row(#xep{}) -> iodata().
generate_row(#xep{xep = XepId, url = URL, name = Name, version = Version,
                  status = Status, modules = Modules}) ->
    FormatStr = "|`~s`|[~s](~s)|~s|~p|`~s`|~n",
    ModuleStr = [string:join(lists:map(fun atom_to_list/1, Modules), "`, `")],
    io_lib:format(FormatStr, [format_xep_id(XepId), Name, URL, Version, Status, ModuleStr]).

-spec generate_list([#xep{}]) -> iodata().
generate_list(RecordList) ->
    SimpleList = [{XEP, Name, Version} ||
                     #xep{xep = XEP, name = Name, version = Version} <- RecordList],
    io_lib:format("~p~n", [SimpleList]).

-spec generate_doap([#xep{}]) -> iodata().
generate_doap(Records) ->
    MapList = [#{"url" => URL, "version" => Version, "status" => Status} ||
                  #xep{url = URL, version = Version, status = Status} <- Records],
    Dir = filename:dirname(escript:script_name()),
    {ok, Template} = file:read_file(Dir ++ "/mongooseim.doap.mustache"),
    bbmustache:render(Template, #{"xeps" => MapList}).

-spec generate_json([#xep{}]) -> iodata().
generate_json(Records) ->
    MapList = [record_to_json_map(Record) || Record <- Records],
    jiffy:encode(MapList, [pretty]).

-spec record_to_json_map(#xep{}) -> jiffy:json_object().
record_to_json_map(#xep{xep = XepId, url = URL, name = Name, version = Version,
                        status = Status, modules = Modules}) ->
    #{xep => iolist_to_binary(format_xep_id(XepId)),
      url => iolist_to_binary(URL),
      name => iolist_to_binary(Name),
      version => iolist_to_binary(Version),
      status => Status,
      modules => Modules}.

-spec format_xep_id(xep()) -> iolist().
format_xep_id(XepId) when is_integer(XepId), XepId >= 1, XepId =< 9999 ->
    io_lib:format("~4..0B", [XepId]);
format_xep_id(XepId) ->
    atom_to_list(XepId).
