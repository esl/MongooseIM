%%%----------------------------------------------------------------------
%%% File    : mod_invites_http.erl
%%% Author  : Stefan Strigler <stefan@strigler.de>
%%% Purpose : Landing Pages to mod_invites.
%%% Created : Su Sep 20 2026 by Stefan Strigler <stefan@strigler.de>
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------
-module(mod_invites_http).
-moduledoc "HTTP handler for mod_invites' landing page".
-author('stefan@strigler.de').

-behaviour(cowboy_handler).
-behaviour(mongoose_http_handler).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_invites.hrl").

-record(request,
        {method            :: method(),
         path = []         :: [binary()],
         raw_path          :: binary(),
         q = []            :: [{binary() | nokey, binary()}],
         us = {<<>>, <<>>} :: {binary(), binary()},
         auth              :: {binary(), binary()} | {oauth, binary(), []} | undefined | invalid,
         lang = <<"">>     :: binary(),
         data = <<"">>     :: binary(),
         ip                :: {inet:ip_address(), inet:port_number()},
         host = <<"">>     :: binary(),
         port = 5280       :: inet:port_number(),
         opts = []         :: list(),
         tp = http         :: protocol(),
         headers = []      :: [{atom() | binary(), binary()}],
         length = 0        :: non_neg_integer(),
         sockmod           :: gen_tcp | fast_tls,
         socket            :: inet:socket() | fast_tls:tls_socket()}).

-type method() :: 'GET' | 'HEAD' | 'DELETE' | 'OPTIONS' | 'PUT' | 'POST' | 'TRACE' | 'PATCH'.
-type protocol() :: http | https.
-type http_request() :: #request{}.

-export([init/2, routes/1]).

-define(HTTP(Code, Headers, CT, Text), {Code, [{<<"Content-Type">>, CT} | Headers], Text}).
-define(HTTP(Code, CT, Text), ?HTTP(Code, [], CT, Text)).
-define(HTTP(Code, Text), ?HTTP(Code, <<"text/plain">>, Text)).
-define(HTTP_OK(Text), ?HTTP_OK([], Text)).
-define(HTTP_OK(Headers, Text), ?HTTP(200, security_headers() ++ Headers, <<"text/html">>, Text)).
-define(NOT_FOUND, ?HTTP(404, ?BIN("NOT FOUND"))).
-define(NOT_FOUND(Text), ?HTTP(404, <<"text/html">>, Text)).
-define(BAD_REQUEST, ?HTTP(400, ?BIN("BAD REQUEST"))).
-define(BAD_REQUEST(Headers, Text), ?HTTP(400, security_headers() ++ Headers, <<"text/html">>, Text)).
-define(BAD_REQUEST(Text), ?HTTP(400, security_headers(), <<"text/html">>, Text)).

-define(DEFAULT_CONTENT_TYPE, <<"application/octet-stream">>).
-define(CONTENT_TYPES,
        [{<<".css">>, <<"text/css">>},
         {<<".js">>, <<"application/javascript">>},
         {<<".png">>, <<"image/png">>},
         {<<".svg">>, <<"image/svg+xml">>}]).
-define(STATIC, <<"static">>).
-define(REGISTRATION, <<"registration">>).
-define(STATIC_CTX, {static, <<"/", Base/binary, "/", ?STATIC/binary>>}).
-define(SITE_NAME_CTX(Name), {site_name, Name}).
-define(LANG(Lang), {lang, Lang}).

init(Req0, #{path := PathPrefix} = Opts) ->
    Path = make_local_path(PathPrefix, cowboy_req:path(Req0)),
    {Code, Headers, Body} = process(Path, make_request(Req0)),
    Req = cowboy_req:reply(
            Code,
            maps:from_list(Headers),
            cowboy_req:set_resp_body(Body, Req0)),
    {ok, Req, Opts}.

make_local_path(PathPrefixS, BinPath) ->
    PathPrefix = list_to_binary(PathPrefixS),
    <<PathPrefix:(size(PathPrefix))/binary, LocalPathBin/binary>> = BinPath,
    binary:split(LocalPathBin, <<"/">>, [global, trim_all]).

make_request(#{method := M, path := P, host := H} = Req) ->
    L = maps:get(language, Req, <<"">>),
    #request{host = H,
             method = M,
             path = binary:split(P, <<"/">>, [global, trim_all]),
             lang = L}.

routes(Opts = #{path := Path}) ->
    [{Path ++ "/static/[...]", cowboy_static,
      {priv_dir, mongooseim, "mod_invites/static/"}},
     {Path ++ "/[...]", ?MODULE, Opts}
    ].

%% ----

landing_page(Host, Invite) ->
    case landing_page_tmpl(Host) of
        <<>> ->
            <<>>;
        Tmpl ->
            render_landing_page_url(Tmpl, Host, Invite)
    end.

landing_page_tmpl(Host) ->
    case gen_mod:get_module_opt(Host, mod_invites, landing_page) of
        <<"none">> ->
            <<>>;
        <<"auto">> ->
            case ejabberd_http:get_auto_url(any, mod_invites) of
                undefined ->
                    ?WARNING_MSG(
                       "'auto' URL configured for mod_invites but no "
                       "request_handler found in your ~s listeners configuration.",
                                 [Host]),
                    <<>>;
                AutoURL ->
                    ExpandedAutoURL = misc:expand_keyword(<<"@HOST@">>, AutoURL, Host),
                    <<ExpandedAutoURL/binary, "{{ invite.token }}">>
            end;
        Tmpl ->
            Tmpl
    end.

render_landing_page_url(Tmpl, Host, Invite) ->
    Ctx = [{invite, invite_to_proplist(Invite)}, {host, Host}],
    render_url(Tmpl, Ctx).

process([Token | _] = LocalPath,
        #request{host = Host,
                 lang = Lang,
                 path = Path} =
            Request) ->
    try mod_invites:is_token_valid(Host, Token) of
        true ->
            case mod_invites:get_invite(Host, Token) of
                %% #invite_token{type = reset_token} = Invite ->
                %%     process_reset_token(Request, Invite, LocalPath);
                %% #invite_token{type = roster_only} = Invite ->
                %%     process_roster_token(LocalPath, Request, Invite);
                Invite ->
                    process_valid_token(LocalPath, Request, Invite)
            end;
        false ->
            ?NOT_FOUND(render(Host,
                              Lang,
                              <<"invite_invalid.html">>,
                              base_ctx(Host, Lang, Path, LocalPath)))
    catch
        _:not_found ->
            ?NOT_FOUND;
        _:{error, host_unknown} ->
            ?NOT_FOUND
    end.

%% process_valid_token([_Token, AppID, ?REGISTRATION] = LocalPath,
%%                     #request{method = 'POST'} = Request,
%%                     Invite) ->
%%     process_register_post(Invite, AppID, Request, LocalPath);
%% process_valid_token([_Token, AppID, ?REGISTRATION] = LocalPath, Request, Invite) ->
%%     process_register_form(Invite, AppID, Request, LocalPath);
%% process_valid_token([_Token, ?REGISTRATION] = LocalPath,
%%                     #request{method = 'POST'} = Request,
%%                     Invite) ->
%%     process_register_post(Invite, <<>>, Request, LocalPath);
%% process_valid_token([_Token, ?REGISTRATION] = LocalPath, Request, Invite) ->
%%     process_register_form(Invite, <<>>, Request, LocalPath);
process_valid_token([_Token, AppID] = LocalPath,
                    #request{host = Host, lang = Lang} = Request,
                    Invite) ->
    try app_ctx(Host, AppID, Lang, ctx(Invite, Request, LocalPath)) of
        AppCtx ->
            render_ok(Host, Invite, Lang, <<"client.html">>, AppCtx)
    catch
        _:not_found ->
            ?NOT_FOUND
    end;
process_valid_token([_Token] = LocalPath,
                    #request{host = Host, lang = Lang} = Request,
                    Invite) ->
    Ctx0 = ctx(Invite, Request, LocalPath),
    Apps = [render_app_urls(App, [{app, App} | Ctx0]) || App <- apps_json(Host, Lang, Ctx0)],
    Ctx = [{apps, Apps} | Ctx0],
    render_ok(Host, Invite, Lang, <<"invite.html">>, Ctx);
process_valid_token(_, _, _) ->
    ?NOT_FOUND.

%% process_reset_token(#request{method = 'POST'} = Request, Invite, LocalPath) ->
%%     process_post(reset_token, Invite, <<>>, Request, LocalPath);
%% process_reset_token(Request, Invite, LocalPath) ->
%%     process_form(reset_token, Invite, <<>>, Request, LocalPath).

%% process_register_form(Invite, AppID, Request, LocalPath) ->
%%     process_form(register, Invite, AppID, Request, LocalPath).

%% process_form(Form,
%%              Invite,
%%              AppID,
%%              #request{host = Host, lang = Lang} = Request,
%%              LocalPath) ->
%%     try app_ctx(Host, AppID, Lang, ctx(Invite, Request, LocalPath)) of
%%         AppCtx ->
%%             CSRFCookie = gen_rand_id(),
%%             Ctx = [{csrf_token, csrf_token(CSRFCookie)} | maybe_add_username(AppCtx, Invite)],
%%             Body = render_form(Form, Request, Ctx),
%%             Headers =
%%                 add_cookie_header(maybe_hsts_header(is_https_lp(Host, Invite)),
%%                                   csrf_cookie_string(form_id(Form), CSRFCookie)),
%%             ?HTTP_OK(Headers, Body)
%%     catch
%%         _:not_found ->
%%             ?NOT_FOUND
%%     end.

%% render_form(Form, #request{host = Host, lang = Lang}, Ctx) ->
%%     MinLength =
%%         case mod_register_opt:password_strength(Host) of
%%             0 ->
%%                 0;
%%             _ ->
%%                 6
%%         end,
%%     render(Host, Lang, form(Form), [{password_min_length, MinLength} | Ctx]).

%% form(register) ->
%%     <<"register.html">>;
%% form(reset_token) ->
%%     <<"reset_token.html">>.

%% form_success(register) ->
%%     <<"register_success.html">>;
%% form_success(reset_token) ->
%%     <<"reset_success.html">>.

%% form_error(register) ->
%%     <<"register_error.html">>;
%% form_error(reset_token) ->
%%     <<"reset_error.html">>.

%% form_id(register) ->
%%     <<"register-id">>;
%% form_id(reset_token) ->
%%     <<"reset-id">>.

%% process_register_post(Invite, AppID, Request, LocalPath) ->
%%     process_post(register, Invite, AppID, Request, LocalPath).

%% process_post(Form,
%%              Invite,
%%              AppID,
%%              #request{host = Host,
%%                       q = Q,
%%                       lang = Lang,
%%                       path = Path,
%%                       ip = {Source, _},
%%                       headers = Headers} =
%%                  Request,
%%              LocalPath) ->
%%     Username = proplists:get_value(<<"user">>, Q),
%%     Password = proplists:get_value(<<"password">>, Q),
%%     CSRFToken = proplists:get_value(<<"csrf_token">>, Q),
%%     Token = Invite#invite_token.token,
%%     CSRFCookie = get_csrf_cookie(form_id(Form), Headers),
%%     try {app_ctx(Host, AppID, Lang, ctx(Invite, Request, LocalPath)),
%%          ensure_same(Token, proplists:get_value(<<"token">>, Q)),
%%          check_csrf(CSRFCookie, CSRFToken)}
%%     of
%%         {AppCtx, ok, ok} ->
%%             case mod_invites_register:try_register(Invite, Username, Host, Password, Source, Lang)
%%             of
%%                 {ok, _UpdatedInvite} ->
%%                     Ctx = maybe_add_webchat_url(Form,
%%                                                 Host,
%%                                                 [{username, Username}, {password, Password}
%%                                                  | AppCtx]),
%%                     render_ok(Host, Invite, Lang, form_success(Form), Ctx);
%%                 {error,
%%                  #stanza_error{text = Text,
%%                                type = Type,
%%                                reason = Reason} =
%%                      Error} ->
%%                     ?DEBUG("registration failed with error: ~p", [Error]),
%%                     Msg = xmpp:get_text(Text, xmpp:prep_lang(Lang)),
%%                     case Type of
%%                         T when T == cancel; T == modify ->
%%                             Ctx = [{username, Username},
%%                                    {csrf_token, CSRFToken},
%%                                    {error, [{text, Msg}, {class, error_class(Reason)}]}]
%%                                   ++ AppCtx,
%%                             Body = render_form(Form, Request, Ctx),
%%                             ?BAD_REQUEST(Body);
%%                         _ ->
%%                             render_bad_request(Host,
%%                                                is_https_lp(Host, Invite),
%%                                                form_error(Form),
%%                                                [{message, Msg} | base_ctx(Host,
%%                                                                           Lang,
%%                                                                           Path,
%%                                                                           LocalPath,
%%                                                                           Token)])
%%                     end
%%             end
%%     catch
%%         _:not_found ->
%%             ?NOT_FOUND;
%%         _:no_match ->
%%             ?BAD_REQUEST
%%     end.

check_csrf(_Token, undefined) ->
    throw(no_match);
check_csrf(<<>>, _Could) ->
    throw(no_match);
check_csrf(Token, Could) ->
    Should = csrf_token(Token),
    try crypto:hash_equals(Should, Could) of
        true ->
            ok;
        _ ->
            throw(no_match)
    catch
        _:_ ->
            throw(no_match)
    end.

csrf_token(Msg) when Msg /= <<>> ->
    SecretKey = ejabberd_config:get_shared_key(),
    base64:encode(
        crypto:mac(hmac,
                   sha256,
                   str:to_hexlist(
                       crypto:hash(sha256, SecretKey)),
                   Msg)).

maybe_add_webchat_url(register, Host, Ctx) ->
    case mod_invites_opt:webchat_url(Host) of
        none ->
            Ctx;
        auto ->
            case ejabberd_http:get_auto_url(any, mod_conversejs) of
                undefined ->
                    ?INFO_MSG("'auto' URL configured for webchat_url but no request_handler for mod_conversejs found in your ~s listeners configuration.",
                              [Host]),
                    Ctx;
                WebchatUrlRaw ->
                    WebchatUrl = misc:expand_keyword(<<"@HOST@">>, WebchatUrlRaw, Host),
                    [{webchat_url, WebchatUrl} | Ctx]
            end;
        WebchatUrl ->
            [{webchat_url, WebchatUrl} | Ctx]
    end;
maybe_add_webchat_url(_, _, Ctx) ->
    Ctx.

process_roster_token([_Token] = LocalPath,
                     #request{host = Host, lang = Lang} = Request,
                     Invite) ->
    Ctx0 = ctx(Invite, Request, LocalPath),
    Apps =
        lists:map(fun(App = #{<<"download">> := #{<<"buttons">> := [Button | _]}}) ->
                     ProceedUrl =
                         case render_app_button_url(Button, Ctx0) of
                             #{magic_link := MagicLink} ->
                                 MagicLink;
                             #{<<"url">> := Url} ->
                                 Url
                         end,
                     App#{proceed_url => ProceedUrl, select_text => translate(Lang, ?BIN("Install"))}
                  end,
                  apps_json(Host, Lang, Ctx0)),
    Ctx = [{apps, Apps} | Ctx0],
    render_ok(Host, Invite, Lang, <<"roster.html">>, Ctx);
process_roster_token(_, _, _) ->
    ?NOT_FOUND.

ensure_same(V, V) ->
    ok;
ensure_same(_, _) ->
    throw(no_match).

app_ctx(_Host, <<>>, _Lang, Ctx) ->
    Ctx;
app_ctx(Host, AppID, Lang, Ctx) ->
    FilteredApps = [A || A <- apps_json(Host, Lang, Ctx), maps:get(<<"id">>, A) == AppID],
    case FilteredApps of
        [App] ->
            [{app, render_app_button_urls(App, Ctx)} | Ctx];
        [] ->
            throw(not_found)
    end.

base_ctx(Host, Lang, Path, LocalPath) ->
    base_ctx(Host, Lang, Path, LocalPath, <<>>).

base_ctx(Host, Lang, Path, LocalPath, Token) ->
    Base = configured_base_path(Host, Path, LocalPath, Token),
    SiteName = gen_mod:get_module_opt(Host, mod_invites, site_name, Host),
    [{base, Base}, {domain, Host}, ?STATIC_CTX, ?SITE_NAME_CTX(SiteName), ?LANG(Lang)].

configured_base_path(Host, Path, LocalPath, Token) ->
    BasePath =
        case landing_page_tmpl(Host) of
            <<>> ->
                Path -- LocalPath;
            Tmpl ->
                Url = render_url(Tmpl, [{invite, [{token, Token}]}, {host, Host}]),
                #{path := OPath0} = uri_string:parse(Url),
                {OPath, _Q} = ejabberd_http:url_decode_q_split_normalize(OPath0),
                OPath -- LocalPath
        end,
    iolist_to_binary(uri_string:normalize(
                         lists:join(<<"/">>, BasePath))).

ctx(Invite,
    #request{host = Host,
             lang = Lang,
             path = Path},
    LocalPath) ->
    [{invite, invite_to_proplist(Invite)},
     {uri, mod_invites:token_uri(Invite)},
     {token, Invite#invite_token.token},
     {registration_url, <<(Invite#invite_token.token)/binary, "/", ?REGISTRATION/binary>>}
     | base_ctx(Host, Lang, Path, LocalPath, Invite#invite_token.token)].

apps_json(Host, Lang, Ctx) ->
    AppsBins = render(Host, Lang, <<"apps.json">>, Ctx),
    AppsBin = binary_join(AppsBins, <<>>),
    AppsMap = jiffy:decode(AppsBin, [return_maps]),
    [app_id(App) || App <- AppsMap].

app_id(App = #{<<"id">> := _ID}) ->
    App;
app_id(App = #{<<"name">> := Name}) ->
    App#{<<"id">> => re:replace(Name, "[^a-zA-Z0-9]+", "-", [global, {return, binary}])}.

invite_to_proplist(I) ->
    [{uri, mod_invites:token_uri(I)} | lists:zip(record_info(fields, invite_token),
                                                 tl(tuple_to_list(I)))].

render_url(Tmpl, Vars) ->
    Renderer = tmpl_to_renderer(Tmpl),
    {ok, URL} = Renderer:render(Vars),
    binary_join(URL, <<>>).

render_app_urls(App = #{<<"supports_preauth_uri">> := true}, Vars) ->
    App#{proceed_url => render_url(<<"{{ invite.token }}/{{ app.id }}">>, Vars)};
render_app_urls(App, Vars) ->
    App#{proceed_url =>
             render_url(<<"{{ invite.token }}/{{ app.id }}/", ?REGISTRATION/binary>>, Vars)}.

render_app_button_urls(App = #{<<"download">> := #{<<"buttons">> := Buttons}}, Vars) ->
    App#{<<"download">> =>
             #{<<"buttons">> =>
                   lists:map(fun(Button) -> render_app_button_url(Button, [{button, Button} | Vars])
                             end,
                             Buttons)}};
render_app_button_urls(App, _Vars) ->
    App.

render_app_button_url(Button = #{<<"magic_link_format">> := MLF}, Vars) ->
    Button#{magic_link => render_url(MLF, Vars)};
render_app_button_url(Button, _Vars) ->
    Button.

file_to_renderer(Host, Filename) ->
    ModName =
        binary_to_atom(<<"mod_invites_template__", Host/binary, "__", Filename/binary>>),
    TemplatesDir = gen_mod:get_module_opt(Host, mod_invites, template_dir),
    TemplatePath = binary_to_list(filename:join([TemplatesDir, Filename])),
    {ok, _Mod, Warnings} =
        erlydtl:compile_file(TemplatePath,
                             ModName,
                             [{out_dir, false},
                              return,
                              {libraries, [{mod_invites_http_erlylib, mod_invites_http_erlylib}]},
                              {default_libraries, [mod_invites_http_erlylib]}]),
    ?DEBUG("got warnings: ~p", [Warnings]),
    ModName.

tmpl_to_renderer(Tmpl) ->
    ModName = binary_to_atom(<<"mod_invites_template__", Tmpl/binary>>),
    case erlang:function_exported(ModName, render, 1) of
        true ->
            ModName;
        false ->
            {ok, _Mod} =
                erlydtl:compile_template(Tmpl,
                                         ModName,
                                         [{out_dir, false},
                                          {libraries,
                                           [{mod_invites_http_erlylib, mod_invites_http_erlylib}]},
                                          {default_libraries, [mod_invites_http_erlylib]}]),
            ModName
    end.

render(Host, Lang, File, Ctx) ->
    Renderer = file_to_renderer(Host, File),
    {ok, Rendered} =
        Renderer:render(Ctx,
                        [{locale, Lang},
                         {translation_fun,
                          fun(Msg, TFLang) -> translate(lang(TFLang), list_to_binary(Msg)) end}]),
    Rendered.

lang(default) ->
    <<"en">>;
lang(Lang) ->
    Lang.

render_ok(Host, Invite, Lang, File, Ctx) ->
    URI = proplists:get_value(uri, Ctx),
    Headers =
        maybe_add_hsts_header([{<<"Link">>, <<"<", URI/binary, ">">>}],
                              is_https_lp(Host, Invite)),
    ?HTTP_OK(Headers, render(Host, Lang, File, Ctx)).

is_https_lp(Host, Invite) ->
    LP = landing_page(Host, Invite),
    re:run(LP, "^https://") =/= nomatch.

maybe_hsts_header(IsHttps) ->
    maybe_add_hsts_header([], IsHttps).

maybe_add_hsts_header(Headers, true) ->
    [{<<"Strict-Transport-Security">>, <<"max-age=31536000; includeSubDomains">>} | Headers];
maybe_add_hsts_header(Headers, false) ->
    Headers.

render_bad_request(Host, IsHttps, File, Ctx) ->
    Headers = maybe_hsts_header(IsHttps),
    Renderer = file_to_renderer(Host, File),
    {ok, Rendered} = Renderer:render(Ctx),
    ?BAD_REQUEST(Headers, Rendered).

-spec guess_content_type(binary()) -> binary().
guess_content_type(FileName) ->
    mod_http_fileserver:content_type(FileName, ?DEFAULT_CONTENT_TYPE, ?CONTENT_TYPES).

maybe_add_username(Ctx, #invite_token{account_name = <<>>}) ->
    Ctx;
maybe_add_username(Ctx, #invite_token{account_name = AccountName}) ->
    [{username, AccountName} | Ctx].

-spec binary_join(binary() | [binary()], binary()) -> binary().
binary_join(Bin, _Sep) when is_binary(Bin) ->
    Bin;
binary_join([], _Sep) ->
    <<>>;
binary_join([Part], _Sep) ->
    Part;
binary_join(List, Sep) ->
    lists:foldr(fun(A, B) ->
                   if bit_size(B) > 0 ->
                          <<A/binary, Sep/binary, B/binary>>;
                      true ->
                          A
                   end
                end,
                <<>>,
                List).

security_headers() ->
    [{<<"Content-Security-Policy">>,
      <<"default-src 'none'; script-src 'self'; style-src 'self'; img-src 'self' data:; frame-ancestors 'none'">>},
     {<<"X-Content-Type-Options">>, <<"nosniff">>},
     {<<"Referrer-Policy">>, <<"no-referrer">>}].

gen_rand_id() ->
    misc:strong_alphanum_token(32).

csrf_cookie_string(Key, CSRFCookie) ->
    <<Key/binary, "=", CSRFCookie/binary, "; HttpOnly; SameSite=strict; Max-Age=86400">>.

add_cookie_header(Headers, Cookie) ->
    [{<<"Set-Cookie">>, Cookie} | Headers].

get_csrf_cookie(Key, Headers) ->
    maps:get(Key, parse_cookie_header(Headers), <<>>).

parse_cookie_header(Headers) ->
    C = proplists:get_value('Cookie', Headers, <<>>),
    lists:foldl(fun ([K, V], M) ->
                        M#{K => V};
                    (_, M) ->
                        M
                end,
                #{},
                [binary:split(S, <<"=">>) || S <- binary:split(C, <<"; ">>, [global])]).

error_class('jid-malformed') ->
    username;
error_class('not-allowed') ->
    username;
error_class(conflict) ->
    username;
error_class(num_invites_exceeded) ->
    username;
error_class('not-acceptable') ->
    password;
error_class(reserved) ->
    account_name;
error_class(user_exists) ->
    account_name;
error_class(account_name_invalid) ->
    account_name;
error_class(_) ->
    undefined.

reason_to_hr(Lang, num_invites_exceeded) ->
    translate(Lang, ?BIN("Maximum number of invites reached"));
reason_to_hr(Lang, user_exists) ->
    translate(Lang, ?BIN("Username exists already"));
reason_to_hr(Lang, account_name_invalid) ->
    translate(Lang, ?BIN("Username contains invalid characters"));
reason_to_hr(Lang, reserved) ->
    translate(Lang, ?BIN("Username is reserved"));
reason_to_hr(_Lang, T) when is_atom(T) ->
    atom_to_binary(T);
reason_to_hr(_Lang, T) ->
    %% good luck!
    T.

translate(_L, T) ->
    T.
