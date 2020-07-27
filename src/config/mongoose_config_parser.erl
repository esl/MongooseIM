%% @doc Parsing and processing of MongooseIM config files
%%   - parser backends: 'cfg' and 'toml'
%%   - config state management
-module(mongoose_config_parser).

%% parser API
-export([parse_file/1]).

%% state API
-export([new_state/0,
         allow_override_all/1,
         allow_override_local_only/1,
         override_global/1,
         override_local/1,
         override_acls/1,
         set_opts/2,
         set_hosts/2,
         get_opts/1,
         state_to_opts/1,
         state_to_host_opts/1,
         state_to_global_opt/3,
         state_to_required_files/1,
         can_override/2]).

%% config post-processing
-export([dedup_state_opts/1,
         add_dep_modules/1]).

-callback parse_file(FileName :: string()) -> state().

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-type key() :: atom()
             | {key(), jid:server() | atom() | list()}
             | {atom(), atom(), atom()}
             | binary(). % TODO: binary is questionable here

-type value() :: atom()
               | binary()
               | integer()
               | string()
               | [value()]
               | tuple().

-export_type([state/0, key/0, value/0]).

-record(state, {opts = [] :: list(),
                hosts = [] :: [host()],
                override_local = false :: boolean(),
                override_global = false :: boolean(),
                override_acls = false :: boolean()}).

-type host() :: any(). % TODO: specify this
-type state() :: #state{}.

%% Parser API

parse_file(FileName) ->
    ParserModule = parser_module(filename:extension(FileName)),
    ParserModule:parse_file(FileName).

parser_module(".toml") -> mongoose_config_parser_toml;
parser_module(".cfg") -> mongoose_config_parser_cfg.

%% State API

new_state() ->
    #state{}.

allow_override_all(State = #state{}) ->
    State#state{override_global = true,
                override_local  = true,
                override_acls   = true}.

allow_override_local_only(State = #state{}) ->
    State#state{override_global = false,
                override_local  = true,
                override_acls   = false}.

override_global(State) ->
    State#state{override_global = true}.

override_local(State) ->
    State#state{override_local = true}.

override_acls(State) ->
    State#state{override_acls = true}.

set_opts(Opts, State) ->
    State#state{opts = Opts}.

set_hosts(Hosts, State) ->
    State#state{hosts = Hosts}.

get_opts(State) ->
    State#state.opts.

%% final getter - reverses the accumulated options
state_to_opts(#state{opts = Opts}) ->
    lists:reverse(Opts).

state_to_host_opts(#state{hosts = Hosts}) ->
    Hosts.

can_override(global, #state{override_global = Override}) ->
    Override;
can_override(local, #state{override_local = Override}) ->
    Override;
can_override(acls, #state{override_acls = Override}) ->
    Override.

state_to_global_opt(OptName, State, Default) ->
    Opts = state_to_opts(State),
    opts_to_global_opt(Opts, OptName, Default).

%% @doc Files, that are required to be present on disc.
-spec state_to_required_files(state()) -> list(file:filename()).
state_to_required_files(State) ->
    Opts = state_to_opts(State),
    opts_to_global_opt(Opts, required_files, []).

opts_to_global_opt([{config, OptName, OptValue}|_], OptName, _Default) ->
    OptValue;
opts_to_global_opt([_|Opts], OptName, Default) ->
    opts_to_global_opt(Opts, OptName, Default);
opts_to_global_opt([], _OptName, Default) ->
    Default.

%% Config post-processing

dedup_state_opts(State = #state{opts = RevOpts}) ->
    {RevOpts2, _Removed} = dedup_state_opts_list(RevOpts, [], [], sets:new()),
    State#state{opts = RevOpts2}.

dedup_state_opts_list([{Type, K, _V} = H|List], Removed, Keep, Set)
  when Type =:= config; Type =:= local_config ->
    Element = {Type, K},
    case sets:is_element(Element, Set) of
        true ->
            dedup_state_opts_list(List, [H|Removed], Keep, Set);
        false ->
            Set1 = sets:add_element(Element, Set),
            dedup_state_opts_list(List, Removed, [H|Keep], Set1)
    end;
dedup_state_opts_list([H|List], Removed, Keep, Set) ->
    dedup_state_opts_list(List, Removed, [H|Keep], Set);
dedup_state_opts_list([], Removed, Keep, _Set) ->
    {Keep, Removed}.

add_dep_modules(State = #state{opts = Opts}) ->
    Opts2 = add_dep_modules_opts(Opts),
    State#state{opts = Opts2}.

add_dep_modules_opts(Opts) ->
    lists:map(fun add_dep_modules_opt/1, Opts).

add_dep_modules_opt({local_config, {modules, Host}, Modules}) ->
    Modules2 = gen_mod_deps:add_deps(Host, Modules),
    {local_config, {modules, Host}, Modules2};
add_dep_modules_opt(Other) ->
    Other.
