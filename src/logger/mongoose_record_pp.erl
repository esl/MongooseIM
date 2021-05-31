%% Pretty prints a record including field names.
%% It's useful to print records with many fields.
-module(mongoose_record_pp).
-export([format/3]).

%% Takes a record tuple, record name, and record_info(fields, Name)
%% Returns `#state{field1 = Value, field2 = Value2}'.
%% usage:
%% `mongoose_record_pp:format(State, state, record_info(fields, state))'
-spec format(tuple(), atom(), [atom()]) -> binary().
format(Record, Name, FieldDefs) when element(1, Record) =:= Name ->
    FieldVals = tl(tuple_to_list(Record)),
    FieldKV = print_pairs(FieldVals, FieldDefs),
    iolist_to_binary(["#", atom_to_list(Name), "{", FieldKV, "}"]);
format(Other, _, _) ->
    iolist_to_binary(io_lib:format("~500p", [Other])).

print_pairs([Val], [Def]) ->
    %% trailing comma
    [pair(Val, Def)];
print_pairs([Val | VRest], [Def | DRest]) ->
    [pair(Val, Def), ", " | print_pairs(VRest, DRest)];
print_pairs([], []) -> [].

pair(Val, Def) ->
    io_lib:format("~p = ~500p", [Def, Val]).
