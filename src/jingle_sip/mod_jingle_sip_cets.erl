%% @doc Backend module for mod_jingle_sip for CETS backend
-module(mod_jingle_sip_cets).
-behaviour(mod_jingle_sip_backend).

-include("mongoose.hrl").

-export([init/2]).
-export([read_session/1]).
-export([write_new_session/2]).
-export([update_session/2]).
-export([remove_session/1]).

-type call_id() :: mod_jingle_sip_session:call_id().
-type session() :: mod_jingle_sip_session:session().
-type update_fun() :: mod_jingle_sip_session:update_fun().

-define(TABLE, cets_jingle_sip_session).

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(_Host, _Opts) ->
    %% We store Erlang records, so keypos is 2
    cets:start(?TABLE, #{keypos => 2}),
    cets_discovery:add_table(mongoose_cets_discovery, ?TABLE).

-spec read_session(call_id()) -> [session()].
read_session(CallID) ->
    ets:lookup(?TABLE, CallID).

-spec write_new_session(call_id(), session()) ->
    ok | {error, conflict}.
write_new_session(CallID, Session) ->
    case read_session(CallID) of
        [_] ->
            {error, conflict};
        _ ->
            case cets:insert_new(?TABLE, Session) of
                true ->
                    ok;
                false ->
                    {error, conflict}
            end
    end.

-spec update_session(call_id(), update_fun()) -> ok | {error, _}.
update_session(CallID, F) ->
    case read_session(CallID) of
        [Session] ->
            case F(Session) of
                {error, _} = Err -> Err;
                Session2 -> cets:insert(?TABLE, Session2)
            end;
       _ ->
            {error, not_found}
    end.

-spec remove_session(call_id()) -> ok.
remove_session(CallID) ->
    cets:delete(?TABLE, CallID).
