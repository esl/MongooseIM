%% Produces filters based on lookup params
-module(mam_filter).
-export([produce_filter/2]).
-include("mongoose_mam.hrl").

-type column() :: atom().

-type filter_field() :: {like, column(), binary()}
    | {le, column(), integer()}
    | {ge, column(), integer()}
    | {equal, column(), integer() | binary()}
    | {less, column(), integer()}
    | {greater, column(), integer()}.

-type filter() :: [filter_field()].
-type fields() :: [#lookup_field{}].
-type params() :: map().

-export_type([filter_field/0]).
-export_type([filter/0]).

-define(SEARCH_WORDS_LIMIT, 10).

-spec produce_filter(params(), fields()) -> list(filter_field()).
produce_filter(Params, Fields) ->
    [new_filter(Field, Value)
     || Field <- Fields,
        Value <- field_to_values(Field, Params)].

field_to_values(#lookup_field{param = Param, value_maker = ValueMaker, required = Required} = Field, Params) ->
    case maps:find(Param, Params) of
        {ok, Value} when Value =/= undefined ->
            make_value(ValueMaker, Value);
        Other when Required ->
            error(#{reason => missing_required_field, field => Field, params => Params, result => Other});
        _ ->
            []
    end.

make_value(search_words, Value) -> search_words(Value);
make_value(undefined, Value) -> [Value]. %% Default value_maker

new_filter(#lookup_field{op = Op, column = Column}, Value) ->
    {Op, Column, Value}.

%% Constructs a separate LIKE filter for each word.
%% SearchText example is "word1%word2%word3".
%% Order of words does not matter (they can go in any order).
-spec search_words(binary()) -> list(binary()).
search_words(SearchText) ->
    Words = binary:split(SearchText, <<"%">>, [global]),
    [<<"%", Word/binary, "%">> || Word <- lists:sublist(Words, ?SEARCH_WORDS_LIMIT)].
