%%%-------------------------------------------------------------------
%%% @doc Creates a new accumulator instance setting location arguments to the caller of this macro.
%%% @end
%%%-------------------------------------------------------------------
-define(new_acc(), mongoose_acc:new(?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(new_acc(Element), mongoose_acc:new(Element, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(new_acc(Element, From), mongoose_acc:new(Element, From, ?MODULE, ?FUNCTION_NAME, ?LINE)).
-define(new_acc(Element, From, To), mongoose_acc:new(Element, From, To, ?MODULE, ?FUNCTION_NAME, ?LINE)).
