%%%-------------------------------------------------------------------
%%%
%%% This module encapsulates a data type which will initially be passed to
%%% hookhandlers as accumulator, and later will be passed all the way along
%%% processing chain.
%%%
%%%-------------------------------------------------------------------
-module(mongoose_acc).
-author("bartek").

-include("jlib.hrl").
-include("ejabberd.hrl").

%% API
-export([new/0, from_kv/2, put/3, get/2, get/3, append/3, remove/2]).
-export([from_element/1, from_map/1, update/2, is_acc/1, require/2]).
-export([strip/1, record_sending/4, record_sending/6]).
-export([initialise/3, terminate/3, terminate/4, dump/1, to_binary/1]).
-export([to_element/1]).
-export_type([t/0]).
-export([from_element/3]).

%% if it is defined as -opaque then dialyzer fails
-type t() :: map().

%%% This module encapsulates implementation of mongoose_acc
%%% its interface is map-like but implementation might change
%%% it is passed along many times, and relatively rarely read or written to
%%% might be worth reimplementing as binary

%%%%% devel API %%%%%

%%% Eventually, we'll call initialise when a stanza enters MongooseIM and terminate
%%% when it leaves. During development we can call both in arbitrary places, provided that
%%% the code which is executed between them is rewritten. We will proceed by moving
%%% both points further apart until they reach their respective ends of processing chain.

initialise(El, _F, _L) ->
%%    ?ERROR_MSG("AAA initialise accumulator ~p ~p", [F, L]),
    from_element(El).

terminate(M, _F, _L) ->
%%    ?ERROR_MSG("ZZZ terminate accumulator ~p ~p", [F, L]),
    get(element, M).

terminate(M, received, _F, _L) ->
%%    ?ERROR_MSG("ZZZ terminate accumulator ~p ~p", [F, L]),
    get(to_send, M, get(element, M, undefined)).

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
            exml:to_binary(get(to_send, Acc));
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
-spec to_element(xmlel() | t()) -> xmlel().
to_element(A) ->
    case is_acc(A) of
        true -> get(to_send, A, get(element, A, undefined));
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

-spec from_element(xmlel()) -> t().
from_element(El) ->
    #xmlel{name = Name, attrs = Attrs} = El,
    Type = exml_query:attr(El, <<"type">>, undefined),
    #{element => El, mongoose_acc => true, name => Name, attrs => Attrs, type => Type,
        timestamp => os:timestamp(), ref => make_ref()
        }.

-spec from_element(xmlel(), ejabberd:jid(), ejabberd:jid()) -> t().
from_element(El, From, To) ->
    Acc = from_element(El),
    M = #{from_jid => From, to_jid => To, from => jid:to_binary(From), to => jid:to_binary(To)},
    update(Acc, M).

-spec from_map(map()) -> t().
from_map(M) ->
    maps:put(mongoose_acc, true, M).

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
            ?WARNING_MSG("Overwriting existing key \"~p\" in accumulator,"
                         "are you sure you have to do it?",
                         [Key]);
        false ->
            ok
    end,
    maps:put(Key, Val, Acc).

-spec get(any()|[any()], t()) -> any().
get(send_result, Acc) ->
    hd(maps:get(send_result, Acc));
get(to_send, Acc) ->
    get(to_send, Acc, get(element, Acc));
get(Key, P) ->
    maps:get(Key, P).

-spec get(any(), t(), any()) -> any().
get(to_send, P, Default) ->
    maps:get(to_send, P, maps:get(element, P, Default));
get(Key, P, Default) ->
    maps:get(Key, P, Default).

-spec append(any(), any(), t()) -> t().
append(Key, Val, P) ->
    L = get(Key, P, []),
    maps:put(Key, append(Val, L), P).

-spec remove(Key :: any(), Accumulator :: t()) -> t().
remove(Key, Accumulator) ->
    maps:remove(Key, Accumulator).

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
%% the very bare minimum (all caches, records etc), replace element with to_send
-spec strip(t()) -> t().
strip(Acc) ->
    El = get(to_send, Acc),
    Acc1 = from_element(El),
%%    Ats = [name, type, attrs, from, from_jid, to, to_jid, ref, timestamp],
    Attributes = [from, from_jid, ref, timestamp],
    NewAcc = lists:foldl(fun(Attrib, AccIn) ->
                           Val = maps:get(Attrib, Acc),
                           maps:put(Attrib, Val, AccIn)
                       end,
                       Acc1, Attributes),
    OptionalAttributes = [to, to_jid],
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
-spec record_sending(t(), xmlel(), atom(), any()) -> t().
record_sending(Acc, Stanza, Module, Result) ->
    record_sending(Acc, none, none, Stanza, Module, Result).
-spec record_sending(t(), jid()|none, jid()|none, xmlel(), atom(), any()) -> t().
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

