%%%===================================================================
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Rule matching generator for mod_revproxy
%%% @end
%%%===================================================================
-module(mod_revproxy_rules).

-export([compile/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
compile(Module, Rules) ->
    Rules1 = prepare_rules(Rules, []),
    Filename = atom_to_list(Module) ++ ".erl",
    Forms = header(Module, Filename) ++ clauses(Rules1) ++ footer(),
    {ok, Module, Binary} = compile:forms(Forms),
    {module, Module} = code:load_binary(Module, Filename, Binary).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
header(Module, Filename) ->
    [{attribute,1,file,{Filename,1}},
     {attribute,6,module,Module},
     {attribute,10,export,[{match,3}]}].

clauses(Rules) ->
    LastClause = {clause,0,
                  [{var,0,'_'},{var,0,'_'},{var,0,'_'}],
                  [],
                  [{call,0,
                    {remote,0,{atom,0,erlang},{atom,0,error}},
                    [{atom,0,no_matching_rule}]}]},
    Clauses = [rule_to_clause(Rule) || Rule <- Rules] ++ [LastClause],
    [{function,15,match,3,Clauses}].

footer() ->
    [{eof,0}].

rule_to_clause({Host, Path, Method, Upstream}) ->
    {clause,0,
     [part_to_clause(Host),
      part_to_clause(Path),
      part_to_clause(Method)],
     [],
     [part_to_clause(Upstream)]}.

part_to_clause('_')   -> {var,0,'_'};
part_to_clause(Other) -> erl_parse:abstract(Other).

prepare_rules([], Acc) ->
    lists:reverse(Acc);
prepare_rules([{Host,Method,Upstream}|Rest], Acc) ->
    prepare_rules([{Host,'_',Method,Upstream}|Rest], Acc);
prepare_rules([{_,_,_,_}=Rule|Rest], Acc) ->
    Prepared = prepare_rule(Rule),
    error_logger:info_msg("~p~n", [Prepared]),
    prepare_rules(Rest, [prepare_rule(Rule)|Acc]);
prepare_rules([_|Rest], Acc) ->
    prepare_rules(Rest, Acc).

prepare_rule({Host,Path,Method,Upstream}) ->
    {prepare_field(Host),
     prepare_field(Path),
     cowboy_bstr:to_upper(prepare_field(Method)),
     prepare_field(Upstream)}.

prepare_field("_")  ->
    '_';
prepare_field('_') ->
    '_';
prepare_field(Bin) when is_binary(Bin) ->
    Bin;
prepare_field(List) when is_list(List) ->
    list_to_binary(List);
prepare_field(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8).
