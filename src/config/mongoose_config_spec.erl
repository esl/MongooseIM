-module(mongoose_config_spec).

-compile(export_all).

-include("ejabberd_config.hrl").

-type config_node() :: #section{} | #option{} | #list{}.

handler(Path) ->
    handler(Path, root()).

handler([Node], #section{items = Items}) when is_binary(Node) ->
    case maps:is_key(Node, Items) of
        true -> maps:get(Node, Items);
        false -> maps:get(default, Items)
    end;
handler([item], #list{items = Item}) ->
    Item;
handler([Node|Rest], #section{items = Items}) when is_binary(Node) ->
    Item = case maps:is_key(Node, Items) of
               true -> maps:get(Node, Items);
               false -> maps:get(default, Items)
           end,
    handler(Rest, Item);
handler([item|Rest], #list{items = Items}) ->
    handler(Rest, Items).

root() ->
    #section{
       items = #{<<"general">> => general()},
       process = fun ?MODULE:process_root/1
      }.

general() ->
    #section{
       items = #{<<"loglevel">> => #option{type = atom,
                                           validate = loglevel,
                                           format = local_config},
                 <<"hosts">> => #list{items = #option{type = binary,
                                                      validate = non_empty,
                                                      process = fun ?MODULE:prepare_host/1},
                                      validate = unique_non_empty,
                                      format = config},
                 <<"registration_timeout">> => #option{type = int_or_infinity,
                                                       validate = timeout,
                                                       format = local_config},
                 <<"language">> => #option{type = binary,
                                           validate = non_empty,
                                           format = config},
                 <<"all_metrics_are_global">> => #option{type = boolean,
                                                         format = local_config},
                 <<"sm_backend">> => #option{type = atom,
                                             validate = {module, ejabberd_sm_},
                                             process = fun ?MODULE:process_sm_backend/1,
                                             format = config},
                 <<"max_fsm_queue">> => #option{type = integer,
                                                validate = positive,
                                                format = local_config},
                 <<"http_server_name">> => #option{type = string,
                                                   format = {local_config, cowboy_server_name}},
                 <<"rdbms_server_type">> => #option{type = atom,
                                                    validate = {enum, [mssql, pgsql]},
                                                    format = local_config},
                 <<"override">> => #list{items = #option{type = atom,
                                                         validate = {enum, [local, global, acls]},
                                                         format = override},
                                         validate = unique_non_empty,
                                         format = none},
                 <<"pgsql_users_number_estimate">> => #option{type = boolean,
                                                              format = host_local_config},
                 <<"route_subdomains">> => #option{type = atom,
                                                   validate = {enum, [s2s]},
                                                   format = host_local_config},
                 <<"mongooseimctl_access_commands">> => #section{
                                                           items = #{default => ctl_access_rule()},
                                                           format = local_config},
                 <<"routing_modules">> => #list{items = #option{type = atom,
                                                                validate = module},
                                                format = local_config},
                 <<"replaced_wait_timeout">> => #option{type = integer,
                                                        validate = positive,
                                                        format = host_local_config},
                 <<"hide_service_name">> => #option{type = boolean,
                                                    format = host_local_config}
                }
      }.

ctl_access_rule() ->
    #section{
       items = #{<<"commands">> => #list{items = #option{type = string}},
                 <<"argument_restrictions">> => #section{
                                                   items = #{default => #option{type = string}}
                                                  }
                },
       process = fun ?MODULE:process_ctl_access_rule/1,
       format = prepend_key
      }.

process_root(KVs) ->
    true = lists:any(fun(#local_config{key = hosts}) -> true;
                        (_) -> false
                     end, KVs),
    KVs.

process_ctl_access_rule(KVs) ->
    Commands = proplists:get_value(commands, KVs, all),
    ArgRestrictions = proplists:get_value(argument_restrictions, KVs, []),
    {Commands, ArgRestrictions}.

process_sm_backend(Backend) ->
    {Backend, []}.

prepare_host(Host) ->
    Node = jid:nodeprep(Host),
    true = Node =/= error,
    Node.
