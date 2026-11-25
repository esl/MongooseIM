%% @doc AWS web identity credentials for mod_event_pusher_sns.
%%
%% One process per host type exchanges the web identity token for a temporary STS session
%% (AssumeRoleWithWebIdentity) and refreshes it before it expires. The token file is read again
%% for every exchange, because the platform (e.g. Kubernetes) rotates it.
%%
%% The current session is kept in persistent_term: it is read for every published notification,
%% but written only once per session lifetime (typically one hour), so the pool workers never
%% wait for this process, even while it is calling STS.
-module(mod_event_pusher_sns_web_identity).

-behaviour(gen_server).

-include("mongoose_logger.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% API
-export([start/2, stop/1, aws_config/2]).

%% gen_server callbacks
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ignore_xref([start_link/2]).

%% Refresh the session this long before it expires.
-define(REFRESH_MARGIN_S, 300).
%% Retry interval after a failed exchange. The previous session, if any, is kept until then.
-define(RETRY_INTERVAL_MS, 10000).
-define(STS_TIMEOUT_MS, 5000).
-define(STS_API_VERSION, "2011-06-15").
-define(DEFAULT_ROLE_SESSION_NAME, "mongooseim").

-type session() :: #{access_key_id := string(),
                     secret_access_key := string(),
                     session_token := string(),
                     expires_at := integer()}. % system time in seconds

-type state() :: #{host_type := mongooseim:host_type(),
                   opts := gen_mod:module_opts()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    ChildSpec = #{id => Proc,
                  start => {?MODULE, start_link, [HostType, Opts]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(ChildSpec),
    ok.

%% @doc Safe to call when the process is not running (static credentials).
-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_sup:stop_child(gen_mod:get_module_proc(HostType, ?MODULE)).

%% @doc Returns the SNS configuration signed with the current session.
%% Raises an error if no session has been obtained yet. The caller retries the publish.
-spec aws_config(mongooseim:host_type(), string()) -> aws_config().
aws_config(HostType, SNSHost) ->
    case persistent_term:get(session_key(HostType), undefined) of
        #{access_key_id := KeyId, secret_access_key := SecretKey, session_token := Token} ->
            Config = erlcloud_sns:new(KeyId, SecretKey, SNSHost),
            Config#aws_config{security_token = Token};
        undefined ->
            error(#{what => sns_web_identity_session_unavailable, host_type => HostType})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec start_link(mongooseim:host_type(), gen_mod:module_opts()) -> gen_server:start_ret().
start_link(HostType, Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, {HostType, Opts}, []).

-spec init({mongooseim:host_type(), gen_mod:module_opts()}) -> {ok, state()}.
init({HostType, Opts}) ->
    process_flag(trap_exit, true),
    State = #{host_type => HostType, opts => Opts},
    %% The first exchange is synchronous, so that notifications can be published
    %% as soon as the module has started. A failure does not stop the module.
    refresh(State),
    {ok, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, {error, unknown_call}, state()}.
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(refresh, State) ->
    refresh(State),
    {noreply, State};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{host_type := HostType}) ->
    persistent_term:erase(session_key(HostType)),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec refresh(state()) -> ok.
refresh(#{host_type := HostType, opts := Opts}) ->
    case fetch_session(Opts) of
        {ok, #{expires_at := ExpiresAt} = Session} ->
            persistent_term:put(session_key(HostType), Session),
            RefreshIn = max(ExpiresAt - erlang:system_time(second) - ?REFRESH_MARGIN_S, 1),
            erlang:send_after(timer:seconds(RefreshIn), self(), refresh),
            ok;
        {error, Reason} ->
            %% The previous session, if any, stays in use until it expires.
            ?LOG_WARNING(#{what => sns_web_identity_refresh_failed, host_type => HostType,
                           reason => Reason, retry_in_ms => ?RETRY_INTERVAL_MS}),
            erlang:send_after(?RETRY_INTERVAL_MS, self(), refresh),
            ok
    end.

-spec fetch_session(gen_mod:module_opts()) -> {ok, session()} | {error, term()}.
fetch_session(#{region := Region} = Opts) ->
    maybe
        {ok, RoleArn} ?= setting(Opts, role_arn, "AWS_ROLE_ARN"),
        {ok, TokenFile} ?= setting(Opts, web_identity_token_file, "AWS_WEB_IDENTITY_TOKEN_FILE"),
        {ok, Token} ?= read_token(TokenFile),
        SessionName = maps:get(role_session_name, Opts, ?DEFAULT_ROLE_SESSION_NAME),
        StsHost = maps:get(sts_host, Opts, "sts." ++ Region ++ ".amazonaws.com"),
        assume_role_with_web_identity(StsHost, RoleArn, SessionName, Token)
    end.

%% @doc A value from the module options, or else the environment variable
%% that the AWS SDKs read and that EKS sets for a service account with an IAM role.
-spec setting(gen_mod:module_opts(), atom(), string()) -> {ok, string()} | {error, term()}.
setting(Opts, Key, EnvVar) ->
    case Opts of
        #{Key := Value} ->
            {ok, Value};
        #{} ->
            case os:getenv(EnvVar) of
                [_ | _] = Value -> {ok, Value};
                _ -> {error, {missing_setting, Key, EnvVar}}
            end
    end.

-spec read_token(string()) -> {ok, string()} | {error, term()}.
read_token(TokenFile) ->
    case file:read_file(TokenFile) of
        {ok, Token} -> {ok, unicode:characters_to_list(string:trim(Token))};
        {error, Reason} -> {error, {token_file, TokenFile, Reason}}
    end.

-spec assume_role_with_web_identity(string(), string(), string(), string()) ->
          {ok, session()} | {error, term()}.
assume_role_with_web_identity(StsHost, RoleArn, SessionName, Token) ->
    %% The token goes in a POST body, so that it does not appear in URLs or proxy logs.
    Body = uri_string:compose_query([{"Action", "AssumeRoleWithWebIdentity"},
                                     {"Version", ?STS_API_VERSION},
                                     {"RoleArn", RoleArn},
                                     {"RoleSessionName", SessionName},
                                     {"WebIdentityToken", Token}]),
    Request = {"https://" ++ StsHost ++ "/", [], "application/x-www-form-urlencoded", Body},
    HttpOpts = [{timeout, ?STS_TIMEOUT_MS},
                {ssl, httpc:ssl_verify_host_options(true)},
                {autoredirect, false}],
    case httpc:request(post, Request, HttpOpts, [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            parse_session(ResponseBody);
        {ok, {{_, Status, _}, _, ResponseBody}} ->
            {error, {sts_error, Status, error_code(ResponseBody)}};
        {error, Reason} ->
            {error, {sts_request_failed, Reason}}
    end.

%% @doc The response contains the secret key, so errors never include it.
-spec parse_session(binary()) -> {ok, session()} | {error, invalid_sts_response}.
parse_session(ResponseBody) ->
    try
        {ok, Root} = exml:parse(ResponseBody),
        Credentials = exml_query:path(Root, [{element, <<"AssumeRoleWithWebIdentityResult">>},
                                             {element, <<"Credentials">>}]),
        Get = fun(Name) ->
                      <<_, _/binary>> = Value = exml_query:path(Credentials,
                                                                [{element, Name}, cdata]),
                      binary_to_list(Value)
              end,
        {ok, #{access_key_id => Get(<<"AccessKeyId">>),
               secret_access_key => Get(<<"SecretAccessKey">>),
               session_token => Get(<<"SessionToken">>),
               expires_at => calendar:rfc3339_to_system_time(Get(<<"Expiration">>))}}
    catch
        _:_ -> {error, invalid_sts_response}
    end.

-spec error_code(binary()) -> binary() | undefined.
error_code(ResponseBody) ->
    case exml:parse(ResponseBody) of
        {ok, Root} -> exml_query:path(Root, [{element, <<"Error">>}, {element, <<"Code">>}, cdata]);
        {error, _} -> undefined
    end.

-spec session_key(mongooseim:host_type()) -> {module(), mongooseim:host_type()}.
session_key(HostType) ->
    {?MODULE, HostType}.
