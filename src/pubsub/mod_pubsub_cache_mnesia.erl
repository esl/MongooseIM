-module(mod_pubsub_cache_mnesia).

-include("pubsub.hrl").
-include("jlib.hrl").

-export([start/0, stop/0]).

-export([create_table/0, delete_table/0, insert_last_item/4, delete_last_item/4]).
%% ------------------------ Backend start/stop ------------------------

-spec start() -> ok.
start() ->
   ok.

-spec stop() -> ok.
stop() ->
    ok.

-spec create_table() -> ok | {error, Reason :: term()}.
create_table() ->
    ok.

-spec delete_table() -> ok | {error, Reason :: term()}.
delete_table() ->
    ok. 

-spec insert_last_item(Nidx :: mod_pubsub:nodeIdx(),
                 ItemID :: mod_pubsub:itemId(),
                 Publisher::{erlang:timestamp(), jid:ljid()},
                 Payload::mod_pubsub:payload()) -> ok | {error, Reason :: term()}.
insert_last_item(_, _, _, _) ->
    ok.
 
-spec delete_last_item(Nidx :: mod_pubsub:nodeIdx(),
                 ItemID :: mod_pubsub:itemId(),
                 Publisher::{erlang:timestamp(), jid:ljid()},
                 Payload::mod_pubsub:payload()) -> ok | {error, Reason :: term()}.
delete_last_item(_, _, _, _) ->
    ok.
