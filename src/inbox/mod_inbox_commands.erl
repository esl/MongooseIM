-module(mod_inbox_commands).

-behaviour(gen_mod).

%% gen_mod
-export([start/2, stop/1, supported_features/0]).

-export([flush_user_bin/3, flush_global_bin/2]).
-ignore_xref([flush_user_bin/3, flush_global_bin/2]).

%% Initialisation
-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_, _) ->
    mongoose_commands:register(commands()).

stop(_) ->
    mongoose_commands:unregister(commands()).

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%% Clean commands
commands() ->
    [
     [{name, inbox_flush_user_bin},
      {category, <<"inbox">>},
      {subcategory, <<"bin">>},
      {desc, <<"Empty the bin for a user">>},
      {module, ?MODULE},
      {function, flush_user_bin},
      {action, delete},
      {identifiers, [domain, name, since]},
      {args, [{domain, binary},
              {name, binary},
              {since, integer}]},
      {result, {num, integer}}],
     [{name, inbox_flush_global_bin},
      {category, <<"inbox">>},
      {subcategory, <<"bin">>},
      {desc, <<"Empty the inbox bin globally">>},
      {module, ?MODULE},
      {function, flush_global_bin},
      {action, delete},
      {identifiers, [host_type, since]},
      {args, [{host_type, binary},
              {since, integer}]},
      {result, {num, integer}}]
    ].

flush_user_bin(Domain, Name, Days) ->
    JID = jid:make_bare(Name, Domain),
    Res = mod_inbox_api:flush_user_bin(JID, Days),
    format_result(Res).

flush_global_bin(HostType, Days) ->
    Res = mod_inbox_api:flush_global_bin(HostType, Days),
    format_result(Res).

format_result({ok, Count}) -> Count;
format_result({_, ErrMsg}) -> {error, bad_request, ErrMsg}.
