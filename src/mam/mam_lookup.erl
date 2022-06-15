%% RSM logic lives here
-module(mam_lookup).
-export([lookup/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-type filter() :: mam_filter:filter().
-type env_vars() :: mod_mam_rdbms_arch:env_vars().
-type params() :: map().
-type message_id() :: mod_mam:message_id().
-type maybe_rsm() :: #rsm_in{} | undefined.
-type opt_count_type() :: last_page | by_offset | none.

%% Public logic
%% We use two fields from Env:
%% - lookup_fn
%% - decode_row_fn
-spec lookup(env_vars(), filter(), params()) ->
    {ok, mod_mam:lookup_result()} | {error, item_not_found}.
lookup(Env = #{}, Filter, Params = #{rsm := RSM}) when is_list(Filter) ->
    OptParams = Params#{opt_count_type => opt_count_type(RSM)},
    choose_lookup_messages_strategy(Env, Filter, OptParams).

lookup_query(QueryType, #{lookup_fn := LookupF} = Env, Filters, Order, OffsetLimit) ->
    LookupF(QueryType, Env, Filters, Order, OffsetLimit).

decode_row(Row, #{decode_row_fn := DecodeF} = Env) ->
    DecodeF(Row, Env).

%% Private logic below

%% There are no optimizations for these queries yet:
%% - #rsm_in{direction = aft, id = ID}
%% - #rsm_in{direction = before, id = ID}
-spec opt_count_type(RSM :: maybe_rsm()) -> opt_count_type().
opt_count_type(#rsm_in{direction = before, id = undefined}) ->
    last_page; %% last page is supported
opt_count_type(#rsm_in{direction = undefined}) ->
    by_offset; %% offset
opt_count_type(undefined) ->
    by_offset; %% no RSM
opt_count_type(_) ->
    none. %% id field is defined in RSM

%% There are several strategies how to extract messages:
%% - we can use regular query that requires counting;
%% - we can reduce number of queries if we skip counting for small data sets;
%% - sometimes we want not to count at all
%%   (for example, our client side counts ones and keep the information)
choose_lookup_messages_strategy(Env, Filter,
                                Params = #{rsm := RSM, page_size := PageSize}) ->
    case Params of
        #{is_simple := true} ->
            %% Simple query without calculating offset and total count
            simple_lookup_messages(Env, RSM, PageSize, Filter);
        #{opt_count_type := last_page} when PageSize > 0 ->
            %% Extract messages before calculating offset and total count
            %% Useful for small result sets
            lookup_last_page(Env, PageSize, Filter);
        #{opt_count_type := by_offset} when PageSize > 0 ->
            %% Extract messages before calculating offset and total count
            %% Useful for small result sets
            lookup_by_offset(Env, RSM, PageSize, Filter);
        _ ->
            %% Calculate offset and total count first before extracting messages
            lookup_messages_regular(Env, RSM, PageSize, Filter)
    end.

%% Just extract messages without count and offset information
simple_lookup_messages(Env, RSM, PageSize, Filter) ->
    {Filter2, Offset, Order} = rsm_to_filter(RSM, Filter),
    Messages = extract_messages(Env, Filter2, Offset, PageSize, Order),
    {ok, {undefined, undefined, Messages}}.

rsm_to_filter(RSM, Filter) ->
    case RSM of
        %% Get last rows from result set
        #rsm_in{direction = aft, id = ID} ->
            {after_id(ID, Filter), 0, asc};
        #rsm_in{direction = before, id = undefined} ->
            {Filter, 0, desc};
        #rsm_in{direction = before, id = ID} ->
            {before_id(ID, Filter), 0, desc};
        #rsm_in{direction = undefined, index = Index} ->
            {Filter, Index, asc};
        undefined ->
            {Filter, 0, asc}
    end.

%% This function handles case: #rsm_in{direction = before, id = undefined}
%% Assumes assert_rsm_without_id(RSM)
lookup_last_page(Env, PageSize, Filter) ->
    Messages = extract_messages(Env, Filter, 0, PageSize, desc),
    Selected = length(Messages),
    Offset =
        case Selected < PageSize of
            true ->
                0; %% Result fits on a single page
            false ->
                FirstID = decoded_row_to_message_id(hd(Messages)),
                calc_count(Env, before_id(FirstID, Filter))
        end,
    {ok, {Offset + Selected, Offset, Messages}}.

lookup_by_offset(Env, RSM, PageSize, Filter) ->
    assert_rsm_without_id(RSM),
    Offset = rsm_to_index(RSM),
    Messages = extract_messages(Env, Filter, Offset, PageSize, asc),
    Selected = length(Messages),
    TotalCount =
        case Selected < PageSize of
            true ->
                Offset + Selected; %% Result fits on a single page
            false ->
                LastID = decoded_row_to_message_id(lists:last(Messages)),
                CountAfterLastID = calc_count(Env, after_id(LastID, Filter)),
                Offset + Selected + CountAfterLastID
        end,
    {ok, {TotalCount, Offset, Messages}}.

assert_rsm_without_id(undefined) -> ok;
assert_rsm_without_id(#rsm_in{id = undefined}) -> ok.

rsm_to_index(#rsm_in{direction = undefined, index = Offset})
    when is_integer(Offset) -> Offset;
rsm_to_index(_) -> 0.

lookup_messages_regular(Env, RSM, PageSize, Filter) ->
    TotalCount = calc_count(Env, Filter),
    Offset = calc_offset(Env, Filter, PageSize, TotalCount, RSM),
    {LookupById, Filter2, Offset2, PageSize2, Order} =
        rsm_to_regular_lookup_vars(RSM, Filter, Offset, PageSize),
    Messages = extract_messages(Env, Filter2, Offset2, PageSize2, Order),
    Result = {TotalCount, Offset, Messages},
    case LookupById of
        true -> %% check if we've selected a message with #rsm_in.id
            mod_mam_utils:check_for_item_not_found(RSM, PageSize, Result);
        false ->
            {ok, Result}
    end.

rsm_to_regular_lookup_vars(RSM, Filter, Offset, PageSize) ->
    case RSM of
        #rsm_in{direction = aft, id = ID} when ID =/= undefined ->
            %% Set extra flag when selecting PageSize + 1 messages
            {true, from_id(ID, Filter), 0, PageSize + 1, asc};
        #rsm_in{direction = before, id = ID} when ID =/= undefined ->
            {true, to_id(ID, Filter), 0, PageSize + 1, desc};
        _ ->
            {false, Filter, Offset, PageSize, asc}
    end.

decode_rows(MessageRows, Env) ->
    [decode_row(Row, Env) || Row <- MessageRows].

-spec decoded_row_to_message_id(mod_mam:message_row()) -> mod_mam:message_id().
decoded_row_to_message_id(#{id := MessId}) -> MessId.

-spec extract_messages(Env :: env_vars(),
                       Filter :: filter(), Offset :: non_neg_integer(), Max :: pos_integer(),
                       Order :: asc | desc) -> [mod_mam:message_row()].
extract_messages(_Env, _Filter, _Offset, 0 = _Max, _Order) ->
    [];
extract_messages(Env, Filter, Offset, Max, Order) ->
    {selected, MessageRows} = extract_rows(Env, Filter, Offset, Max, Order),
    Rows = maybe_reverse(Order, MessageRows),
    decode_rows(Rows, Env).

maybe_reverse(asc, List) -> List;
maybe_reverse(desc, List) -> lists:reverse(List).

extract_rows(Env, Filters, Offset, Max, Order) ->
    lookup_query(lookup, Env, Filters, Order, {Offset, Max}).

%% @doc Get the total result set size.
%% SELECT COUNT(*) as count FROM mam_message
-spec calc_count(env_vars(), filter()) -> non_neg_integer().
calc_count(Env, Filter) ->
    Result = lookup_query(count, Env, Filter, unordered, all),
    mongoose_rdbms:selected_to_integer(Result).

%% @doc Calculate a zero-based index of the row with UID in the result test.
%%
%% If the element does not exists, the ID of the next element will
%% be returned instead.
%% @end
%% SELECT COUNT(*) as index FROM mam_message WHERE id <= ?
-spec calc_index(env_vars(), filter(), message_id()) -> non_neg_integer().
calc_index(Env, Filter, ID) ->
    calc_count(Env, to_id(ID, Filter)).

%% @doc Count of elements in RSet before the passed element.
%%
%% The element with the passed UID can be already deleted.
%% @end
%% SELECT COUNT(*) as count FROM mam_message WHERE id < ?
-spec calc_before(env_vars(), filter(), message_id()) -> non_neg_integer().
calc_before(Env, Filter, ID) ->
    calc_count(Env, before_id(ID, Filter)).

-spec calc_offset(Env :: env_vars(),
                  Filter :: filter(), PageSize :: non_neg_integer(),
                  TotalCount :: non_neg_integer(), RSM :: jlib:rsm_in()) -> non_neg_integer().
calc_offset(Env, Filter, PageSize, TotalCount, RSM) ->
  case RSM of
      #rsm_in{direction = undefined, index = Index} when is_integer(Index) ->
          Index;
      #rsm_in{direction = before, id = undefined} ->
          %% Requesting the Last Page in a Result Set
          max(0, TotalCount - PageSize);
      #rsm_in{direction = before, id = ID} when is_integer(ID) ->
          max(0, calc_before(Env, Filter, ID) - PageSize);
      #rsm_in{direction = aft, id = ID} when is_integer(ID) ->
          calc_index(Env, Filter, ID);
      _ ->
          0
  end.

-spec after_id(message_id(), filter()) -> filter().
after_id(ID, Filter) ->
    [{greater, id, ID}|Filter].

-spec before_id(message_id(), filter()) -> filter().
before_id(ID, Filter) ->
    [{less, id, ID}|Filter].

-spec from_id(message_id(), filter()) -> filter().
from_id(ID, Filter) ->
    [{ge, id, ID}|Filter].

-spec to_id(message_id(), filter()) -> filter().
to_id(ID, Filter) ->
    [{le, id, ID}|Filter].
