#!/usr/bin/env escript
%%! -pa deps/mustache/ebin

main([OutputFilePath, TemplateFile]) ->
    {ok, Template} = file:read_file(TemplateFile),
    {ok, Vars} = file:consult("rel/vars.config"),
    CfgFile = mustache:render(binary_to_list(Template), dict:from_list(Vars)),
    file:write_file(OutputFilePath, CfgFile).
