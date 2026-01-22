-module(mod_graphql_test).
-behaviour(gen_mod).

-export([start/2, stop/1, reported_module_options/2, supported_features/0]).

start(_HostType, _Opts) -> ok.
stop(_HostType) -> ok.

reported_module_options(_HostType, _Opts) ->
    [
     {atom_opt, atom_val},
     {int_opt, 123},
     {float_opt, 123.456},
     {bin_opt, <<"binary">>},
     {str_opt, "string"},
     {unicode_opt, [1050, 1051]},
     {list_opt, [1, 2]},
     {map_opt, #{key => val}},
     {tuple_opt, {a, b}}
    ].

supported_features() ->
    [dynamic_domains].
