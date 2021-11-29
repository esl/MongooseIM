-module(mongoose_graphql_cowboy_handler).

-behavior(cowboy_rest).

%% ejabberd_cowboy callbacks
-export([cowboy_router_paths/2]).

%% Cowboy Handler Interface
-export([init/2]).

%% REST callbacks
-export([
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    content_types_accepted/2,
    charsets_provided/2,
    is_authorized/2
]).

%% Data input/output callbacks
-export([
    from_json/2,
    to_json/2,
    to_html/2
]).

-ignore_xref([cowboy_router_paths/2, from_json/2, to_html/2, to_json/2]).

%% -- API ---------------------------------------------------

cowboy_router_paths(BasePath, Opts) ->
    [{[BasePath, "/"], ?MODULE, [{priv_file, mongooseim, "graphql/wsite/index.html"} | Opts]}].

init(Req, [{priv_file, _, _} = PrivFile | Opts]) ->
    OptsMap = maps:from_list(Opts),
    {cowboy_rest,
     Req,
     OptsMap#{index_location => PrivFile}
    }.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, from_json}
    ], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json},
        {{<<"text">>, <<"html">>, []}, to_html}
    ], Req, State}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

is_authorized(Req, State) ->
    Auth = cowboy_req:parse_header(<<"authorization">>, Req),
    State2 = check_auth(Auth, State),
    {true, Req, State2}.

resource_exists(#{ method := <<"GET">> } = Req, State) ->
    {true, Req, State};
resource_exists(#{ method := <<"POST">> } = Req, State) ->
    {false, Req, State}.

to_html(Req, #{ index_location :=
                    {priv_file, App, FileLocation}} = State) ->
    Filename = filename:join(code:priv_dir(App), FileLocation),
    {ok, Data} = file:read_file(Filename),
    {Data, Req, State}.

json_request(Req, State) ->
    case gather(Req) of
        {error, Reason} ->
            reply_error(400, Reason, Req, State);
        {ok, Req2, Decoded} ->
            run_request(Decoded, Req2, State)
    end.

from_json(Req, State) -> json_request(Req, State).
to_json(Req, State) -> json_request(Req, State).

%% -- INTERNAL FUNCTIONS ---------------------------------------

check_auth(Auth, #{schema_endpoint := <<"admin">>} = State) ->
    auth_admin(Auth, State);
check_auth(Auth, #{schema_endpoint := <<"user">>} = State) ->
    auth_user(Auth, State).

auth_user({basic, User, Password}, State) ->
    case mongoose_api_common:check_password(jid:from_binary(User), Password) of
        {true, _} -> State#{authorized => true, schema_ctx => #{username => User}};
        _ ->  State#{authorized => false}
    end;
auth_user(_, State) ->
    State#{authorized => false}.

auth_admin({basic, Username, Password}, #{username := Username, password := Password} = State) ->
    State#{authorized => true};
auth_admin(undefined, #{username := _, password := _} = State) ->
    State#{authorized => false};
auth_admin(_, State) ->
    % auth credentials not provided in config
    State#{authorized => true}.

run_request(#{ document := undefined }, Req, State) ->
    reply_error(400, no_query_supplied, Req, State);
run_request(#{} = ReqCtx, Req, #{schema_endpoint := EpName,
                                 authorized := AuthStatus} = State) ->
    {ok, Ep} = mongoose_graphql:get_endpoint(binary_to_existing_atom(EpName)),
    Ctx = maps:get(schema_ctx, State, #{}),
    ReqCtx2 = ReqCtx#{authorized => AuthStatus, ctx => Ctx},
    case mongoose_graphql:execute(Ep, ReqCtx2) of
        {ok, Response} ->
            ResponseBody = mongoose_graphql_cowboy_response:term_to_json(Response),
            Req2 = cowboy_req:set_resp_body(ResponseBody, Req),
            Reply = cowboy_req:reply(200, Req2),
            {stop, Reply, State};
        {error, Reason} ->
            reply_error(400, Reason, Req, State)
    end.

gather(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Bindings = cowboy_req:bindings(Req2),
    try jsx:decode(Body, [return_maps]) of
        JSON ->
            gather(Req2, JSON, Bindings)
    catch
        error:badarg ->
            {error, invalid_json_body}
    end.

gather(Req, Body, Params) ->
    QueryDocument = document([Params, Body]),
    case variables([Params, Body]) of
        {ok, Vars} ->
            Operation = operation_name([Params, Body]),
            {ok, Req, #{ document => QueryDocument,
                         vars => Vars,
                         operation_name => Operation}};
        {error, Reason} ->
            {error, Reason}
    end.

document([#{ <<"query">> := Q }|_]) -> Q;
document([_|Next]) -> document(Next);
document([]) -> undefined.

variables([#{ <<"variables">> := Vars} | _]) ->
  if
      is_binary(Vars) ->
          try jsx:decode(Vars, [return_maps]) of
              null -> {ok, #{}};
              JSON when is_map(JSON) -> {ok, JSON};
              _ -> {error, invalid_json}
          catch
              error:badarg ->
                  {error, invalid_json}
          end;
      is_map(Vars) ->
          {ok, Vars};
      Vars == null ->
          {ok, #{}}
  end;
variables([_ | Next]) ->
    variables(Next);
variables([]) ->
    {ok, #{}}.

operation_name([#{ <<"operationName">> := OpName } | _]) ->
    OpName;
operation_name([_ | Next]) ->
    operation_name(Next);
operation_name([]) ->
    undefined.

reply_error(Code, Msg, Req, State) ->
    Errors =
        case Msg of
            {error, E} ->
                graphql_err:format_errors(#{}, [E]);
            _ ->
                Formatted = iolist_to_binary(io_lib:format("~p", [Msg])),
                [#{type => error, message => Formatted}]
            end,

    Body = jsx:encode(#{ errors => Errors}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.
