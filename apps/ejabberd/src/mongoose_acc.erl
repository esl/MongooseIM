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
-export([new/0, from_kv/2, put/3, get/2, get/3, append/3, to_map/1, remove/2]).
-export([from_element/1, from_map/1, update/2, is_acc/1, require/2]).
-export([flush/1]).
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
    ?DEPRECATED,
    exml:to_binary(Packet);
to_binary({broadcast, Payload}) ->
    case mongoose_acc:is_acc(Payload) of
        true ->
            to_binary(Payload);
        false ->
            list_to_binary(io_lib:format("~p", [Payload]))
    end;
to_binary(Acc) ->
    % replacement to exml:to_binary, for error logging
    case mongoose_acc:is_acc(Acc) of
        true ->
            exml:to_binary(mongoose_acc:get(element, Acc));
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
    #{mongoose_acc => true}.

-spec from_kv(atom(), any()) -> t().
from_kv(K, V) ->
    M = maps:put(K, V, #{}),
    maps:put(mongoose_acc, true, M).

-spec from_element(xmlel()) -> t().
from_element(El) ->
    #xmlel{name = Name, attrs = Attrs} = El,
    Type = exml_query:attr(El, <<"type">>, undefined),
    #{element => El, mongoose_acc => true, name => Name, attrs => Attrs, type => Type}.

-spec from_element(xmlel(), ejabberd:jid(), ejabberd:jid()) -> t().
from_element(El, From, To) ->
    #xmlel{name = Name, attrs = Attrs} = El,
    Type = exml_query:attr(El, <<"type">>, undefined),
    #{element => El, mongoose_acc => true, name => Name, attrs => Attrs, type => Type,
        from_jid => From, to_jid => To, from => jid:to_binary(From), to => jid:to_binary(To)}.

-spec from_map(map()) -> t().
from_map(M) ->
    maps:put(mongoose_acc, true, M).

-spec update(t(), map() | t()) -> t().
update(Acc, M) ->
    maps:merge(Acc, M).

%% @doc convert to map so that we can pattern-match on it
-spec to_map(t()) -> map()|{error, cant_convert_to_map}.
to_map(P) when is_map(P) ->
    P;
to_map(_) ->
    {error, cant_convert_to_map}.

-spec put(atom(), any(), t()) -> t().
put(to_send, Val, Acc) ->
    % stanza to be sent out may change a few times, and sometimes it carries its own type
    % (e.g. presence probe), we have to clear previouse value
    A1 = maps:remove(send_type, Acc),
    maps:put(to_send, Val, A1);
put(Key, Val, Acc) ->
    maps:put(Key, Val, Acc).

-spec get(atom()|[atom()], t()) -> any().
get(to_send, Acc) ->
    get(to_send, Acc, get(element, Acc));
get([], _) ->

    undefined;
get([Key|Keys], P) ->
    case maps:is_key(Key, P) of
        true ->
            maps:get(Key, P);
        _ ->
            get(Keys, P)
    end;
get(Key, P) ->
    maps:get(Key, P).

-spec get(atom(), t(), any()) -> any().
get(Key, P, Default) ->
    maps:get(Key, P, Default).

-spec append(atom(), any(), t()) -> t().
append(Key, Val, P) ->
    L = get(Key, P, []),
    maps:put(Key, append(Val, L), P).

-spec remove(Key :: atom(), Accumulator :: t()) -> t().
remove(Key, Accumulator) ->
    maps:remove(Key, Accumulator).

%% @doc Make sure the acc has certain keys - some of them require expensive computations and
%% are therefore not calculated upon instantiation, this func is saying "I will need these,
%% please prepare them for me"
-spec require(atom() | [atom()], t()) -> t().
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

%% @doc Remove all data we cached
%% Personally I think we should consider a more flexible implementation of
%% in-accumulator cache, so that attrs are not hardcoded here.
-spec flush(t()) -> t().
flush(Acc) ->
    remove(privacy_check, Acc).

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
produce(send_type, Acc) ->
    % 'type' is from original stanza
    El = mongoose_acc:get(to_send, Acc),
    SType = exml_query:attr(El, <<"type">>, undefined),
    mongoose_acc:put(send_type, SType, Acc);
produce(xmlns, Acc) ->
    read_children(Acc);
produce(command, Acc) ->
    read_children(Acc).


%% @doc scan xml children to look for namespace; the name of xml element containing namespace
%% defines the purpose of a stanza, we store it as 'command'; if absent, we store 'undefined'.
read_children(Acc) ->
    Acc1 = mongoose_acc:put(command, undefined, mongoose_acc:put(xmlns, undefined, Acc)),
    #xmlel{children = Children} = mongoose_acc:get(element, Acc1),
    read_children(Acc, Children).

read_children(Acc, []) ->
    Acc;
read_children(Acc, [#xmlel{} = Chld|Tail]) ->
    case exml_query:attr(Chld, <<"xmlns">>, undefined) of
        undefined ->
            read_children(Acc, Tail);
        Ns ->
            #xmlel{name = Name} = Chld,
            mongoose_acc:put(command, Name, mongoose_acc:put(xmlns, Ns, Acc))
    end;
read_children(Acc, [_|Tail]) ->
    read_children(Acc, Tail).

