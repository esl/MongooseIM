#!/usr/bin/env escript
%%! -pa _build/default/lib/bbmustache/ebin

main([OutputFilePath, TemplateFile]) ->
    {ok, Template} = file:read_file(TemplateFile),
    {ok, Vars} = file:consult("rel/vars.config"),
    CfgFile = bbmustache:render(Template, maps:from_list(Vars),
	                        [{key_type, atom}]),
    file:write_file(OutputFilePath, CfgFile).
