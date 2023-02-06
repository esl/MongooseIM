%%%-------------------------------------------------------------------
%%% File    : mongoose_acc.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Author  : Bartlomiej Gorny <bartlomiej.gorny@erlang-solutions.com>
%%% Purpose : Mongoose accumulator implementation
%%% Created : 11 Sep 2018 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%
%%% NS:Key conventions:
%%% * hook:result should be used to return hook processing result
%%% * iq:* contains useful IQ metadata but must be provided by mongoose_iq.erl
%%%-------------------------------------------------------------------
-module(mongoose_acc).
-author("bartlomiej.gorny@erlang-solutions.com").
-author("piotr.nosek@erlang-solutions.com").

-include("jlib.hrl").

%% API
% Constructor
-export([new/1]).
% Access to built-in fields
-export([
         ref/1,
         timestamp/1,
         lserver/1,
         host_type/1,
         element/1,
         to_jid/1,
         from_jid/1,
         packet/1,
         stanza_name/1,
         stanza_type/1,
         stanza_ref/1
        ]).
% Stanza update
-export([update_stanza/2]).
% C2S accumulator
-export([get_statem_acc/1, set_statem_acc/2]).
% Access to namespaced fields
-export([
         set/3,
         set/4,
         set_permanent/2,
         set_permanent/3,
         set_permanent/4,
         append/4,
         get_permanent_keys/1,
         get_permanent_fields/1,
         get/2,
         get/3,
         get/4,
         delete/3,
         delete/2,
         delete_many/3
        ]).
% Strip with or without stanza replacement
-export([strip/1, strip/2]).

-ignore_xref([delete/2, ref/1]).

%% Note about 'undefined' to_jid and from_jid: these are the special cases when JID may be
%% truly unknown: before a client is authorized.

-type line_number() :: non_neg_integer().
-type location() :: #{mfa  := mfa(),
                      line := line_number(),
                      file := string()}.

-type stanza_metadata() :: #{
        element := exml:element(),
        from_jid := jid:jid() | undefined,
        to_jid := jid:jid() | undefined,
        name := binary(),
        type := binary(),
        ref := reference()
       }.

%% If it is defined as -opaque then dialyzer fails
%% It's still valid in acc 2.0 and gain is probably not worth the effort
-type t() :: #{
        mongoose_acc := true,
        ref := reference(),
        timestamp := integer(), %microsecond
        origin_pid := pid(),
        origin_location := location(),
        stanza := stanza_metadata() | undefined,
        lserver := jid:lserver(),
        host_type := binary() | undefined,
        non_strippable := [ns_key()],
        statem_acc := mongoose_c2s_acc:t(),
        ns_key() => Value :: any()
       }.

-export_type([t/0,
              new_acc_params/0]).

-type new_acc_params() :: #{
        location := location(),
        lserver => jid:lserver(),
        element => exml:element() | undefined,
        host_type => binary() | undefined, % optional
        from_jid => jid:jid() | undefined, % optional
        to_jid => jid:jid() | undefined, % optional
        statem_acc => mongoose_c2s_acc:t() % optional
       }.

-type strip_params() :: #{
        lserver := jid:lserver(),
        element := exml:element(),
        host_type => binary() | undefined, % optional
        from_jid => jid:jid() | undefined, % optional
        to_jid => jid:jid() | undefined % optional
       }.

-type stanza_params() :: #{
        element := exml:element(),
        from_jid => jid:jid() | undefined, % optional
        to_jid => jid:jid() | undefined, % optional
        _ => _
       }.

-type ns_key() :: {NS :: any(), Key :: any()}.
-type ns_key_value() :: {Namespace :: any(), K :: any(), V :: any()}.

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------

-spec new(Params :: new_acc_params()) -> t().
new(#{ location := Location, lserver := LServer } = Params) ->
    Stanza = case maps:get(element, Params, undefined) of
                 undefined -> undefined;
                 _Element -> stanza_from_params(Params)
             end,
    HostType = get_host_type(Params),
    #{
      mongoose_acc => true,
      ref => make_ref(),
      timestamp => erlang:system_time(microsecond),
      origin_pid => self(),
      origin_location => Location,
      stanza => Stanza,
      lserver => LServer,
      host_type => HostType,
      statem_acc => get_mongoose_c2s_acc(Params),
      %% The non_strippable elements must be unique.
      %% This used to be represented with the sets module, but as the number of elements inserted
      %% was too small, sets were themselves an overhead, and also annoying when printing
      non_strippable => []
     }.

-spec ref(Acc :: t()) -> reference().
ref(#{ mongoose_acc := true, ref := Ref }) ->
    Ref.

-spec timestamp(Acc :: t()) -> integer().
timestamp(#{ mongoose_acc := true, timestamp := TS }) ->
    TS.

-spec lserver(Acc :: t()) -> jid:lserver().
lserver(#{ mongoose_acc := true, lserver := LServer }) ->
    LServer.

-spec host_type(Acc :: t()) -> binary() | undefined.
host_type(#{ mongoose_acc := true, host_type := HostType }) ->
    HostType.

-spec element(Acc :: t()) -> exml:element() | undefined.
element(#{ mongoose_acc := true, stanza := #{ element := El } }) ->
    El;
element(#{ mongoose_acc := true }) ->
    undefined.

-spec from_jid(Acc :: t()) -> jid:jid() | undefined.
from_jid(#{ mongoose_acc := true, stanza := #{ from_jid := FromJID } }) ->
    FromJID;
from_jid(#{ mongoose_acc := true }) ->
    undefined.

-spec to_jid(Acc :: t()) -> jid:jid() | undefined.
to_jid(#{ mongoose_acc := true, stanza := #{ to_jid := ToJID } }) ->
    ToJID;
to_jid(#{ mongoose_acc := true }) ->
    undefined.

-spec packet(Acc :: t()) -> mongoose_c2s:packet() | undefined.
packet(#{ mongoose_acc := true, stanza := #{ to_jid := ToJID,
                                             from_jid := FromJID,
                                             element := El } }) ->
    {FromJID, ToJID, El};
packet(#{ mongoose_acc := true }) ->
    undefined.

-spec stanza_name(Acc :: t()) -> binary() | undefined.
stanza_name(#{ mongoose_acc := true, stanza := #{ name := Name } }) ->
    Name;
stanza_name(#{ mongoose_acc := true }) ->
    undefined.

-spec stanza_type(Acc :: t()) -> binary() | undefined.
stanza_type(#{ mongoose_acc := true, stanza := #{ type := Type } }) ->
    Type;
stanza_type(#{ mongoose_acc := true }) ->
    undefined.

-spec stanza_ref(Acc :: t()) -> reference() | undefined.
stanza_ref(#{ mongoose_acc := true, stanza := #{ ref := StanzaRef } }) ->
    StanzaRef;
stanza_ref(#{ mongoose_acc := true }) ->
    undefined.

-spec update_stanza(NewStanzaParams :: stanza_params(), Acc :: t()) -> t().
update_stanza(NewStanzaParams, #{ mongoose_acc := true } = Acc) ->
    Acc#{ stanza := stanza_from_params(NewStanzaParams) }.

-spec get_statem_acc(Acc :: t()) -> mongoose_c2s_acc:t().
get_statem_acc(#{ mongoose_acc := true, statem_acc := StatemAcc }) ->
    StatemAcc.

-spec set_statem_acc(NewStatemAcc :: mongoose_c2s_acc:t(), Acc :: t()) -> t().
set_statem_acc(NewStatemAcc, Acc = #{ mongoose_acc := true }) ->
    Acc#{statem_acc := NewStatemAcc}.

%% Values set with this function are discarded during 'strip' operation...
-spec set(Namespace :: any(), K :: any(), V :: any(), Acc :: t()) -> t().
set(NS, K, V, #{ mongoose_acc := true } = Acc) ->
    Acc#{ {NS, K} => V }.

-spec set(Namespace :: any(), [{K :: any(), V :: any()}], Acc :: t()) -> t().
set(NS, KVs, #{ mongoose_acc := true } = Acc) ->
    NSKVs = [ {{NS, K}, V} || {K, V} <- KVs ],
    Input = maps:from_list(NSKVs),
    maps:merge(Acc, Input).

-spec set([ns_key_value()], Acc :: t()) -> t().
set(NSKVs, #{ mongoose_acc := true } = Acc) ->
    PropList = [ {{NS, K}, V} || {NS, K, V} <- NSKVs ],
    Input = maps:from_list(PropList),
    maps:merge(Acc, Input).

%% .. while these are not.
-spec set_permanent(Namespace :: any(), K :: any(), V :: any(), Acc :: t()) -> t().
set_permanent(NS, K, V, #{ mongoose_acc := true, non_strippable := NonStrippable } = Acc) ->
    Key = {NS, K},
    NewNonStrippable = [Key | lists:delete(Key, NonStrippable)],
    Acc#{ Key => V, non_strippable := NewNonStrippable }.

-spec set_permanent(Namespace :: any(), [{K :: any(), V :: any()}], Acc :: t()) -> t().
set_permanent(NS, KVs, #{mongoose_acc := true, non_strippable := NonStrippable} = Acc) ->
    NewKeys = [{NS, K} || {K, _V} <- KVs, not lists:member({NS, K}, NonStrippable)],
    set(NS, KVs, Acc#{non_strippable := NewKeys ++ NonStrippable }).

-spec set_permanent([ns_key_value()], Acc :: t()) -> t().
set_permanent(NSKVs, #{mongoose_acc := true, non_strippable := NonStrippable} = Acc) ->
    NewKeys = [{NS, K} || {NS, K, _V} <- NSKVs, not lists:member({NS, K}, NonStrippable)],
    set(NSKVs, Acc#{non_strippable := NewKeys ++ NonStrippable }).

-spec append(NS :: any(), Key :: any(), Val :: any() | [any()], Acc :: t()) -> t().
append(NS, Key, Val, Acc) ->
    OldVal = get(NS, Key, [], Acc),
    set(NS, Key, append(OldVal, Val), Acc).

-spec get_permanent_keys(Acc :: t()) -> [ns_key()].
get_permanent_keys(#{mongoose_acc := true, non_strippable := NonStrippable}) ->
    NonStrippable.

-spec get_permanent_fields(Acc :: t()) -> [ns_key_value()].
get_permanent_fields(Acc) ->
    [{NS, Key, mongoose_acc:get(NS, Key, Acc)} ||
        {NS, Key} <- mongoose_acc:get_permanent_keys(Acc)].

-spec get(Namespace :: any(), Acc :: t()) -> [{K :: any(), V :: any()}].
get(NS, #{mongoose_acc := true} = Acc) ->
    Fn = fun({Namespace, K}, V, List) when Namespace =:= NS ->
                [{K, V} | List];
            (_, _, List) ->
                List
         end,
    maps:fold(Fn, [], Acc).

-spec get(Namespace :: any(), K :: any(), Acc :: t()) -> V :: any().
get(NS, K, #{ mongoose_acc := true } = Acc) ->
    maps:get({NS, K}, Acc).

-spec get(Namespace :: any(), K :: any(), Default :: any(), Acc :: t()) -> V :: any().
get(NS, K, Default, #{ mongoose_acc := true } = Acc) ->
    maps:get({NS, K}, Acc, Default).

-spec delete(Namespace :: any(), K :: any(), Acc :: t()) -> t().
delete(NS, K, #{ mongoose_acc := true, non_strippable := NonStrippable } = Acc0) ->
    Key = {NS, K},
    Acc1 = maps:remove(Key, Acc0),
    Acc1#{ non_strippable := lists:delete(Key, NonStrippable) }.

-spec delete_many(Namespace :: any(), [K :: any()], Acc :: t()) -> t().
delete_many(NS, Keys, #{ mongoose_acc := true, non_strippable := NonStrippable } = Acc0) ->
    KVs = [{NS, K} || K <- Keys],
    Acc1 = maps:without(KVs, Acc0),
    Acc1#{ non_strippable := lists:subtract(NonStrippable, KVs) }.

-spec delete(Namespace :: any(), Acc :: t()) -> t().
delete(NS, Acc) ->
    KeyList = [K || {K, _} <- get(NS, Acc)],
    delete_many(NS, KeyList, Acc).

-spec strip(Acc :: t()) -> t().
strip(#{ mongoose_acc := true, non_strippable := NonStrippable } = Acc) ->
    Stripped = maps:with(NonStrippable ++ default_non_strippable(), Acc),
    Stripped#{statem_acc := mongoose_c2s_acc:new()}.

-spec strip(ParamsToOverwrite :: strip_params(), Acc :: t()) -> t().
strip(#{ lserver := NewLServer } = Params, Acc) ->
    StrippedAcc = strip(Acc),
    StrippedAcc#{ lserver := NewLServer, host_type := get_host_type(Params),
                  stanza := stanza_from_params(Params) }.

%% --------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------
-spec get_host_type(new_acc_params() | strip_params()) -> binary() | undefined.
get_host_type(#{host_type := HostType}) ->
    HostType;
get_host_type(_) ->
    undefined.

-spec get_mongoose_c2s_acc(new_acc_params() | strip_params()) -> mongoose_c2s_acc:t() | undefined.
get_mongoose_c2s_acc(#{statem_acc := C2SAcc}) ->
    C2SAcc;
get_mongoose_c2s_acc(_) ->
    mongoose_c2s_acc:new().

-spec stanza_from_params(Params :: stanza_params() | strip_params()) ->
    stanza_metadata().
stanza_from_params(#{ element := El } = Params) ->
    #{
      element => El,
      from_jid => jid_from_params(from_jid, <<"from">>, Params),
      to_jid => jid_from_params(to_jid, <<"to">>, Params),
      name => El#xmlel.name,
      type => exml_query:attr(El, <<"type">>),
      ref => make_ref()
     }.

-spec jid_from_params(MapKey :: to_jid | from_jid,
                      StanzaAttrName :: binary(),
                      Params :: stanza_params()) -> jid:jid().
jid_from_params(to_jid, _, #{to_jid := Jid}) ->
    Jid;
jid_from_params(from_jid, _, #{from_jid := Jid}) ->
    Jid;
jid_from_params(_, StanzaAttrName, #{element := El}) ->
    #jid{} = jid:from_binary(exml_query:attr(El, StanzaAttrName)).

-spec default_non_strippable() -> [atom()].
default_non_strippable() ->
    [
     mongoose_acc,
     ref,
     timestamp,
     origin_pid,
     origin_location,
     stanza,
     lserver,
     host_type,
     statem_acc,
     non_strippable
    ].

-spec append(OldVal :: list(), Val :: list() | any()) -> list().
append(OldVal, Val) when is_list(OldVal), is_list(Val) -> OldVal ++ Val;
append(OldVal, Val) when is_list(OldVal) -> [Val | OldVal].
