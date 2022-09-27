-module(mongoose_admin_api_inbox).

-behaviour(mongoose_admin_api).
-export([routes/1]).

-behaviour(cowboy_rest).
-export([init/2,
         is_authorized/2,
         allowed_methods/2,
         delete_resource/2]).

-import(mongoose_admin_api, [try_handle_request/3, throw_error/2, respond/3]).

-type req() :: cowboy_req:req().
-type state() :: mongoose_admin_api:state().

-spec routes(state()) -> mongoose_http_handler:routes().
routes(State) ->
    [{"/inbox/:host_type/:days/bin", ?MODULE, State},
     {"/inbox/:domain/:user/:days/bin", ?MODULE, State}].

-spec init(req(), state()) -> {cowboy_rest, req(), state()}.
init(Req, State) ->
    mongoose_admin_api:init(Req, State).

-spec is_authorized(req(), state()) -> {true | {false, iodata()}, req(), state()}.
is_authorized(Req, State) ->
    mongoose_admin_api:is_authorized(Req, State).

-spec allowed_methods(req(), state()) -> {[binary()], req(), state()}.
allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"DELETE">>], Req, State}.

%% @doc Called for a method of type "DELETE"
-spec delete_resource(req(), state()) -> {stop, req(), state()}.
delete_resource(Req, State) ->
    try_handle_request(Req, State, fun handle_delete/2).

%% Internal functions

handle_delete(Req, State) ->
    Bindings = cowboy_req:bindings(Req),
    case Bindings of
        #{host_type := _} ->
            flush_global_bin(Req, State, Bindings);
        _ ->
            flush_user_bin(Req, State, Bindings)
    end.

flush_global_bin(Req, State, #{host_type := HostType} = Bindings) ->
    Days = get_days(Bindings),
    case mod_inbox_api:flush_global_bin(HostType, Days) of
        {host_type_not_found, Msg} ->
            throw_error(not_found, Msg);
        {ok, Count} ->
            respond(Req, State, Count)
    end.

flush_user_bin(Req, State, Bindings) ->
    JID = get_jid(Bindings),
    Days = get_days(Bindings),
    case mod_inbox_api:flush_user_bin(JID, Days) of
        {user_does_not_exist, Msg} ->
            throw_error(not_found, Msg);
        {domain_not_found, Msg} ->
            throw_error(not_found, Msg);
        {ok, Count} ->
            respond(Req, State, Count)
    end.

get_days(#{days := DaysBin}) ->
    try binary_to_integer(DaysBin)
    catch _:_ -> throw_error(bad_request, <<"Invalid number of days">>)
    end.

get_jid(#{user := User, domain := Domain}) ->
    case jid:make_bare(User, Domain) of
        error -> throw_error(bad_request, <<"Invalid JID">>);
        JID -> JID
    end.
