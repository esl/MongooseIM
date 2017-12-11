-module(mongoose_api_common_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


-define(aq(E, V), (
    [ct:fail("ASSERT EQUAL~n\tExpected ~p~n\tActual   ~p~n", [(E), (V)])
     || (E) =/= (V)]
    )).

all() ->
    [url_is_correct_for_create_command,
     url_is_correct_for_read_command,
     url_is_correct_for_read_command_with_subcategory].

url_is_correct_for_create_command(_) ->
    Cmd = create_cmd(),
    Url = mongoose_api_common:create_admin_url_path(Cmd),
    ?aq(["/",<<"users">>,["/:host"], []], Url).

url_is_correct_for_read_command(_) ->
    Cmd = read_cmd(),
    Url = mongoose_api_common:create_admin_url_path(Cmd),
    ?aq(["/",<<"users">>,["/:host"],[]], Url).

url_is_correct_for_read_command_with_subcategory(_) ->
    Cmd = read_cmd2(),
    Url = mongoose_api_common:create_admin_url_path(Cmd),
    ?aq(["/",<<"users">>,["/:host"],["/", <<"rosters">>]], Url).

create_cmd() ->
    Props = [
             {name, registeruser},
             {category, <<"users">>},
             {desc, <<"Register a user">>},
             {module, ?MODULE},
             {function, register},
             {action, create},
             {args, [{user, binary}, {host, binary}, {password, binary}]},
             {identifiers, [host]},
             {result, {msg, binary}}
            ],
    mongoose_commands:new(Props).

read_cmd() ->
    Props = [
            {name, listusers},
            {category, <<"users">>},
            {desc, <<"List registered users on this host">>},
            {module, ?MODULE},
            {function, registered_users},
            {action, read},
            {args, [{host, binary}]},
            {result, []}
        ],
    mongoose_commands:new(Props).

read_cmd2() ->
    Props = [
            {name, listusers},
            {category, <<"users">>},
            {subcategory, <<"rosters">>},
            {desc, <<"List registered users on this host">>},
            {module, ?MODULE},
            {function, registered_users},
            {action, read},
            {args, [{host, binary}]},
            {result, []}
        ],
    mongoose_commands:new(Props).

