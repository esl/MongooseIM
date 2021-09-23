-module(common_helper).
-compile(export_all).
-compile(nowarn_export_all).

get_bjid(UserSpec) ->
    User = proplists:get_value(username, UserSpec),
    Server = proplists:get_value(server, UserSpec),
    <<User/binary,"@",Server/binary>>.

