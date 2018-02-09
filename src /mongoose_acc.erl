%%%-------------------------------------------------------------------
%%% @doc
%%% This module encapsulates a data type which will is instantiated when stanza
%%% enters the system and is passed all the way along processing chain.
%%% Its interface is map-like, and you can put there whatever you want, bearing in mind two things:
%%% 1. It is read-only, you can't change value once you wrote it (accumulator is to accumulate)
%%% 2. Whatever you put will be removed before the acc is sent to another c2s process
%%%
%%% There are three caveats to the above:
%%% 1. Although you can not put to an existing key, you can append as many times as you like
%%% 2. A special key 'result' is writeable and is meant to be used to get return value from hook calls
%%% 3. If you want to pass something to another c2s process you can use add_prop/3 - values
%%%    put there are not stripped.
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_acc).
-author("bartek").

-include("jlib.hrl").
-include("mongoose.hrl").

%% API
-export([new/0, from_kv/2, put/3, get/2, get/3, append/3, remove/2]).
-export([add_prop/3, get_prop/2]).
-export([from_element/1, from_map/1, update_element/4, update/2, is_acc/1, require/2]).
-export([strip/1, strip/2, record_sending/4, record_sending/6]).
-export([dump/1, to_binary/1]).
-export([to_element/1]).
-export_type([t/0]).
-export([from_element/3]).

%% if it is defined as -opaque then dialyzer fails
-type t() :: map().

%%% This module encapsulates implementation of mongoose_acc
%%% its interface is map-like but implementation might change
%%% it is passed along many times, and relatively rarely read or written to
%%% might be worth reimplementing as binary

dump(Acc) ->
    dump(Acc, lists:sort(maps:keys(Acc))).

to_binary(#xmlel{} = Packet) ->
    exml:to_binary(Packet);
to_binary({broadcast, Payload}) ->
    case is_acc(Payload) of
        true ->
            to_binary(Payload);
        false ->
            list_to_binary(io_lib:format("~p", [Payload]))
    end;
to_binary(Acc) ->
    % replacement to exml:to_binary, for error logging
    case is_acc(Acc) of
        true ->
            El = get(element, Acc),
            case El of
                #xmlel{} -> exml:to_binary(El);
                _ -> list_to_binary(io_lib:format("~p", [El]))
            end;
        false ->
            list_to_binary(io_lib:format("~p", [Acc]))
    end.

%% This function is for transitional period, eventually all hooks will use accumulator
%% and we will not have to check
is_acc(A) when is_map(A) ->
    maps:get(mongoose_acc, A, false);
is_acc(_) ->
    false.

%% this is a temporary hack - right now processes receive accumulators and stanzas, it is all
%% mixed up so we have to cater for this
-spec to_element(exml:element() | t()) -> exml:element().
to_element(A) ->
    case is_acc(A) of
        true -> get(element, A, undefined);
        false -> A
    end.


%%%%% API %%%%%

-spec new() -> t().
new() ->
    #{mongoose_acc => true, timestamp => os:timestamp(), ref => make_ref()}.

-spec from_kv(atom(), any()) -> t().
from_kv(K, V) ->
    M = maps:put(K, V, #{}),
    maps:put(mongoose_acc, true, M).

%% @doc This one has an alternative form because normally an acc carries an xml element, as
%% received from client, but sometimes messages are generated internally (broadcast) by sm
%% and sent to c2s
-spec from_element(exml:element() | jlib:iq() | tuple()) -> t().
from_element(#xmlel{name = Name, attrs = Attrs} = El) ->
    Type = exml_query:attr(El, <<"type">>, undefined),
    #{element => El, mongoose_acc => true, name => Name, attrs => Attrs, type => Type,
        timestamp => os:timestamp(), ref => make_ref()
        };
from_element(#iq{type = Type} = El) ->
    #{element => El, mongoose_acc => true, name => <<"iq">>, type => Type,
        timestamp => os:timestamp(), ref => make_ref()
    };
from_element(El) when is_tuple(El) ->
    Name = <<"broadcast">>,
    Type = element(1, El),
    % ref and timestamp will be filled in by strip/2
    #{element => El, name => Name, type => Type, mongoose_acc => true}.

-spec from_element(exml:element() | jlib:iq(), jid:jid(), jid:jid()) -> t().
from_element(El, From, To) ->
    Acc = from_element(El),
    M = #{from_jid => From, to_jid => To, from => jid:to_binary(From), to => jid:to_binary(To)},
    update(Acc, M).

-spec from_map(map()) -> t().
from_map(M) ->
    maps:put(mongoose_acc, true, M).

-spec update_element(t(), exml:element(), jid:jid(), jid:jid()) -> t().
update_element(Acc, Element, From, To) ->
    update(Acc, from_element(Element, From, To)).

-spec update(t(), map() | t()) -> t().
update(Acc, M) ->
    maps:merge(Acc, M).

-spec put(any(), any(), t()) -> t().
put(from_jid, Val, Acc) ->
    % used only when we have to manually construct an acc (instead of calling from_element)
    % namely: in c2s terminate, since it is not triggered by stanza, and in some
    % deprecated functions
    A = maps:put(from_jid, Val, Acc),
    maps:put(from, jid:to_binary(Val), A);
put(result, Val, Acc) ->
    maps:put(result, Val, Acc);
put(Key, Val, Acc) ->
    % check whether we are replacing existing value (warning now, later it will become
    % read-only; accumulator is for accumulating)
    case maps:is_key(Key, Acc) of
        true ->
%%            ?WARNING_MSG("Overwriting existing key \"~p\" in accumulator,"
%%                         "are you sure you have to do it?",
%%                         [Key]);
            ok;
        false ->
            ok
    end,
    maps:put(Key, Val, Acc).

-spec get(any()|[any()], t()) -> any().
get(send_result, Acc) ->
    hd(maps:get(send_result, Acc));
get(Key, P) ->
    maps:get(Key, P).

-spec get(any(), t(), any()) -> any().
get(Key, P, Default) ->
    maps:get(Key, P, Default).

-spec append(any(), any(), t()) -> t().
append(Key, Val, P) ->
    L = get(Key, P, []),
    maps:put(Key, append(Val, L), P).

-spec remove(Key :: any(), Accumulator :: t()) -> t().
remove(Key, Accumulator) ->
    maps:remove(Key, Accumulator).

%% @doc adds a persistent property to an accumulator
-spec add_prop(atom(), any(), t()) -> t().
add_prop(Key, Value, Acc) ->
    append(persistent_properties, {Key, Value}, Acc).

-spec get_prop(atom(), t()) -> any().
get_prop(Key, Acc) ->
    proplists:get_value(Key, get(persistent_properties, Acc, [])).

%% @doc Make sure the acc has certain keys - some of them require expensive computations and
%% are therefore not calculated upon instantiation, this func is saying "I will need these,
%% please prepare them for me"
-spec require(any() | [any()], t()) -> t().
require([], Acc) ->
    Acc;
require([Key|Tail], Acc) ->
    Acc1 = case maps:is_key(Key, Acc) of
               true -> Acc;
               false -> produce(Key, Acc)
           end,
    require(Tail, Acc1);
require(Key, Acc) ->
    require([Key], Acc).

%% @doc Convert the acc before routing it out to another c2s process - remove everything except
%% the very bare minimum
%% We get rid of all caches, traces etc.
%% There is a special key "persistent_properties" which is also kept, use add_prop to store
%% persistent stuff you want to pass between processes there.
-spec strip(t()) -> t().
strip(Acc) ->
    strip(Acc, get(element, Acc)).

%% @doc alt version in case we want to replace the xmlel we are sending
-spec strip(t(), exml:element()) -> t().
strip(Acc, El) ->
    Acc1 = from_element(El),
    Attributes = [ref, timestamp],
    NewAcc = lists:foldl(fun(Attrib, AccIn) ->
                           Val = maps:get(Attrib, Acc),
                           maps:put(Attrib, Val, AccIn)
                       end,
                       Acc1, Attributes),
    OptionalAttributes = [from, from_jid, to, to_jid, persistent_properties, global_distrib],
    lists:foldl(fun(Attrib, AccIn) ->
                    case maps:get(Attrib, Acc, undefined) of
                        undefined -> AccIn;
                        Val -> maps:put(Attrib, Val, AccIn)
                    end
                end,
        NewAcc, OptionalAttributes).

%% @doc Recording info about sending out a stanza/accumulator
%% There are two versions because when we send xml element from c2s then
%% there is no From and To args available, everything is already in the stanza
%% while from ejabberd_router:route we get bare stanza and two jids.
-spec record_sending(t(), exml:element(), atom(), any()) -> t().
record_sending(Acc, Stanza, Module, Result) ->
    record_sending(Acc, none, none, Stanza, Module, Result).
-spec record_sending(t(),jid:jid()|none,jid:jid()|none, exml:element(), atom(), any()) -> t().
record_sending(Acc, _From, _To, _Stanza, _Module, Result) ->
    mongoose_acc:append(send_result, Result, Acc).

%%%%% internal %%%%%

append(Val, L) when is_list(L), is_list(Val) ->
    L ++ Val;
append(Val, L) when is_list(L) ->
    [Val | L].

dump(_, []) ->
    ok;
dump(Acc, [K|Tail]) ->
    ?ERROR_MSG("~p = ~p", [K, maps:get(K, Acc)]),
    dump(Acc, Tail).


%% @doc pattern-match to figure out (a) which attrs can be 'required' (b) how to cook them
produce(iq_query_info, Acc) ->
    Iq = jlib:iq_query_info(get(element, Acc)), % it doesn't change
    put(iq_query_info, Iq, Acc);
produce(xmlns, Acc) ->
    read_children(Acc);
produce(command, Acc) ->
    read_children(Acc).


%% @doc scan xml children to look for namespace; the name of xml element containing namespace
%% defines the purpose of a stanza, we store it as 'command'; if absent, we store 'undefined'.
read_children(Acc) ->
    Acc1 = put(command, undefined, put(xmlns, undefined, Acc)),
    #xmlel{children = Children} = get(element, Acc1),
    read_children(Acc, Children).

read_children(Acc, []) ->
    Acc;
read_children(Acc, [#xmlel{} = Chld|Tail]) ->
    case exml_query:attr(Chld, <<"xmlns">>, undefined) of
        undefined ->
            read_children(Acc, Tail);
        Ns ->
            #xmlel{name = Name} = Chld,
            put(command, Name, put(xmlns, Ns, Acc))
    end;
read_children(Acc, [_|Tail]) ->
    read_children(Acc, Tail).
