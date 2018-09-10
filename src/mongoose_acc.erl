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
-author("piotr.nosek@erlang-solutions.com").

-include("jlib.hrl").
-include("mongoose.hrl").

%% API
-export([new/1]).
-export([set/4, get/3, get/4]).
%% Debug API
-export([dump/1]).

%% if it is defined as -opaque then dialyzer fails
-type t() :: map().
-export_type([t/0]).

-type new_acc_props() :: #{
        location => {Module :: module(), Function :: atom(), Line :: pos_integer()},
        from_jid => jid:jid(),
        to_jid => jid:jid(),
        lserver => jid:lserver(),
        origin_stanza => exml:element() % optional
       }.

%% --------------------------------------------------------
%% API
%% --------------------------------------------------------

-spec new(Props :: new_acc_props()) -> t().
new(#{ location := Location,
       from_jid := FromJID,
       to_jid := ToJID,
       lserver := LServer } = Props) ->
    Acc0 = #{
      mongoose_acc => true,
      ref => make_ref(),
      timestamp => os:timestamp(),
      origin_pid => self(),
      origin_location => Location,
      from_jid => FromJID,
      to_jid => ToJID,
      lserver => LServer,
      send_result => []
     },
    case maps:get(origin_stanza, Props, undefined) of
        undefined ->
            Acc0;
        OriginStanza ->
            Acc0#{
              origin_stanza => OriginStanza,
              origin_stanza_attrs => OriginStanza#xmlel.attrs
             }
    end.

-spec set(Acc :: t(), Namespace :: any(), K :: any(), V :: any()) -> t().
set(#{ mongoose_acc := true } = Acc, NS, K, V) ->
    Acc#{ ext_key(NS, K) => V }.

get(#{ mongoose_acc := true } = Acc, NS, K) ->
    maps:get(ext_key(NS, K), Acc).

get(#{ mongoose_acc := true } = Acc, NS, K, Default) ->
    maps:get(ext_key(NS, K), Acc, Default).

%% --------------------------------------------------------
%% Debug API
%% --------------------------------------------------------

dump(Acc) ->
    lists:foreach(fun(K) ->
                          ?ERROR_MSG("~p = ~p", [K, maps:get(K, Acc)])
                  end, lists:sort(maps:keys(Acc))).

%% --------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------

-spec ext_key(NS :: any(), K :: any()) -> {external, NS :: any(), K :: any()}.
ext_key(NS, K) ->
    {external, NS, K}.

