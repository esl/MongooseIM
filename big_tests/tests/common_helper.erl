-module(common_helper).
-compile([export_all, nowarn_export_all]).

get_bjid(UserSpec) ->
    User = proplists:get_value(username, UserSpec),
    Server = proplists:get_value(server, UserSpec),
    <<User/binary,"@",Server/binary>>.

unprep(Bin) when is_binary(Bin) ->
    string:titlecase(Bin).
