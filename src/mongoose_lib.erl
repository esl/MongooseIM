-module(mongoose_lib).

-export([find_behaviour_implementations/1]).
-export([log_if_backend_error/4]).
%% Maps
-export([maps_append/3]).
-export([maps_foreach/2]).
-export([pairs_foreach/2]).
-export([maps_or_pairs_foreach/2]).

-include("mongoose.hrl").

%% ------------------------------------------------------------------
%% Behaviour util
%% ------------------------------------------------------------------

%% WARNING! For simplicity, this function searches only MongooseIM code dir
-spec find_behaviour_implementations(Behaviour :: module()) -> [module()].
find_behaviour_implementations(Behaviour) ->
    {ok, EbinFiles} = file:list_dir(code:lib_dir(mongooseim, ebin)),
    Mods = [ list_to_atom(filename:rootname(File))
             || File <- EbinFiles, filename:extension(File) == ".beam" ],
    lists:filter(fun(M) ->
                         try lists:keyfind([Behaviour], 2, M:module_info(attributes)) of
                             {behavior, _} -> true;
                             {behaviour, _} -> true;
                             _ -> false
                         catch
                             _:_ -> false
                         end
                 end, Mods).

%% ------------------------------------------------------------------
%% Logging
%% ------------------------------------------------------------------

%% @doc Database backends for various modules return ok, {atomic, ok}
%% or {atomic, []} on success, and usually {error, ...} on failure.
%% All we need is to log an error if such occurred, and proceed normally.
-spec log_if_backend_error(V :: any(), % value return by called backend fun
                           Module :: atom(), % caller
                           Line :: integer(),
                           Args :: any() ) -> ok.
log_if_backend_error(V, Module, Line, Args) ->
    case V of
        ok -> ok;
        {atomic, _} -> ok;
        {updated, _} -> ok; % rdbms
        L when is_list(L) -> ok; % riak
        {error, E} ->
            make_msg("Error calling backend", E, Module, Line, Args);
        E ->
            make_msg("Unexpected return from backend", E, Module, Line, Args)
    end,
    ok.

make_msg(Msg, Error, Module, Line, Args) ->
    ?ERROR_MSG("~p:~p module=~p line=~p arguments=~p",
                  [Msg, Error, Module, Line, Args]).

%% ------------------------------------------------------------------
%% Maps
%% ------------------------------------------------------------------

%% Appends a new Value to the current list of values associated with Key.
maps_append(Key, Value, Map) ->
    Values = maps:get(Key, Map, []),
    maps:put(Key, Values ++ [Value], Map).

-spec maps_foreach(fun(), map()) -> ok.
maps_foreach(Fun, Map) when is_function(Fun, 1) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun({Key, Value}), Acc
              end, ok, Map);
maps_foreach(Fun, Map) when is_function(Fun, 2) ->
    maps:fold(fun(Key, Value, Acc) ->
                      Fun(Key, Value), Acc
              end, ok, Map).

-spec pairs_foreach(Fun, [{Key, Value}]) -> ok
    when
      Fun :: fun((Key, Value) -> term())
           | fun(({Key, Value}) -> term()),
      Key :: term(),
      Value :: term().
pairs_foreach(Fun, List) when is_function(Fun, 1) ->
    lists:foreach(Fun, List);
pairs_foreach(Fun, List) when is_function(Fun, 2) ->
    lists:foreach(fun({K,V}) -> Fun(K,V) end, List).

maps_or_pairs_foreach(Fun, Map) when is_map(Map) ->
    maps_foreach(Fun, Map);
maps_or_pairs_foreach(Fun, List) when is_list(List) ->
    pairs_foreach(Fun, List).
