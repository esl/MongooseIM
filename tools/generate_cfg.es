#!/usr/bin/env escript
%%! -pa deps/mustache/ebin

main([OutputFilePath]) ->
    {ok, Template} = file:read_file("rel/files/ejabberd.cfg"),
    {ok, Vars} = file:consult("rel/vars.config"),
    CfgFile = mustache:render(binary_to_list(Template), dict:from_list(Vars)),
    file:write_file(OutputFilePath, CfgFile).