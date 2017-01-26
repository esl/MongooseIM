%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_http_upload).
-author('konrad.zemek@gmail.com').
-behaviour(gen_mod).
-xep([{xep, 363}, {version, "0.2.4"}]).

-include("jlib.hrl").
-include("ejabberd.hrl").

-define(DEFAULT_TOKEN_BYTES, 32).
-define(DEFAULT_MAX_FILE_SIZE, 10 * 1024 * 1024). % 10 MB
-define(DEFAULT_SUBHOST, <<"upload.@HOST@">>).

-export([start/2, stop/1, iq_handler/3]).

%% Hook implementations
-export([get_disco_identity/5, get_disco_items/5, get_disco_features/5]).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------

-callback create_slot(UTCDateTime :: calendar:datetime(), UUID :: binary(),
                      Filename :: unicode:unicode_binary(), ContentType :: binary(),
                      Size :: pos_integer(), Opts :: proplists:proplist()) ->
    {PUTURL :: binary(), GETURL :: binary()}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:server(), Opts :: list()) -> any().
start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    SubHost = subhost(Host),
    mod_disco:register_subhost(Host, SubHost),
    mongoose_subhosts:register(Host, SubHost),
    ejabberd_hooks:add(disco_local_features, SubHost, ?MODULE, get_disco_features, 90),
    ejabberd_hooks:add(disco_local_identity, SubHost, ?MODULE, get_disco_identity, 90),
    ejabberd_hooks:add(disco_local_items, Host, ?MODULE, get_disco_items, 90),
    gen_iq_handler:add_iq_handler(ejabberd_local, SubHost, ?NS_HTTP_UPLOAD, ?MODULE,
                                  iq_handler, IQDisc),
    gen_mod:start_backend_module(?MODULE, with_default_backend(Opts), [create_slot]).


-spec stop(Host :: ejabberd:server()) -> any().
stop(Host) ->
    SubHost = subhost(Host),
    gen_iq_handler:remove_iq_handler(ejabberd_local, SubHost, ?NS_HTTP_UPLOAD),
    ejabberd_hooks:delete(disco_local_items, Host, ?MODULE, get_disco_items, 90),
    ejabberd_hooks:delete(disco_local_identity, SubHost, ?MODULE, get_disco_identity, 90),
    ejabberd_hooks:delete(disco_local_features, SubHost, ?MODULE, get_disco_features, 90),
    mongoose_subhosts:unregister(SubHost),
    mod_disco:unregister_subhost(Host, SubHost).


-spec iq_handler(From :: ejabberd:jid(), To :: ejabberd:jid(), IQ :: ejabberd:iq()) ->
                        ejabberd:iq() | ignore.
iq_handler(_From, _To, IQ = #iq{type = set, sub_el = SubEl}) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};
iq_handler(_From, _To, IQ = #iq{type = get, sub_el = Request}) ->
    case parse_request(Request) of
        {Filename, Size, ContentType} ->
            case max_file_size =:= undefined orelse Size =< max_file_size() of
                true ->
                    UTCDateTime = calendar:universal_time(),
                    Token = generate_token(),
                    Opts = module_opts(),

                    {PutUrl, GetUrl} = mod_http_upload_backend:create_slot(
                                         UTCDateTime, Token, Filename, ContentType, Size, Opts),

                    compose_iq_reply(IQ, PutUrl, GetUrl);

                false ->
                    MaxFileSizeBin = integer_to_binary(max_file_size()),
                    Error0 = ?ERR_NOT_ACCEPTABLE,
                    Error = Error0#xmlel{
                              children =
                                  [
                                   #xmlel{name = <<"file-too-large">>,
                                          attrs = [{<<"xmlns">>, ?NS_HTTP_UPLOAD}],
                                          children =
                                              [#xmlel{name = <<"max-file-size">>,
                                                      children = [exml:escape_cdata(MaxFileSizeBin)]}]}
                                   | Error0#xmlel.children
                                  ]},
                    IQ#iq{type = error, sub_el = [Error]}
            end;

        bad_request ->
            IQ#iq{type = error, sub_el = [Request, ?ERR_BAD_REQUEST]}
    end.


-spec get_disco_identity(Acc :: term(), From :: ejabberd:jid(), To :: ejabberd:jid(),
                         Node :: binary(), ejabberd:lang()) -> [jlib:xmlel()] | term().
get_disco_identity(Acc, _From, _To, _Node, Lang) ->
    [#xmlel{name = <<"identity">>,
            attrs = [{<<"category">>, <<"store">>},
                     {<<"type">>, <<"file">>},
                     {<<"name">>, my_disco_name(Lang)}]} | Acc];
get_disco_identity(Acc, _From, _To, _Node, _Lang) ->
    Acc.


-spec get_disco_items(Acc :: term(), From :: ejabberd:jid(), To :: ejabberd:jid(),
                      Node :: binary(), ejabberd:lang()) -> {result, [jlib:xmlel()]} | term().
get_disco_items({result, Nodes}, _From, #jid{lserver = LServer} = _To, <<"">>, Lang) ->
    Item = #xmlel{name = <<"item">>,
                  attrs = [{<<"jid">>, subhost(LServer)}, {<<"name">>, my_disco_name(Lang)}]},
    {result, [Item | Nodes]};
get_disco_items(empty, From, To, Node, Lang) ->
    get_disco_items({result, []}, From, To, Node, Lang);
get_disco_items(Acc, _From, _To, _Node, _Lang) ->
    Acc.


-spec get_disco_features(Acc :: term(), From :: ejabberd:jid(), To :: ejabberd:jid(),
                         Node :: binary(), ejabberd:lang()) -> {result, [jlib:xmlel()]} | term().
get_disco_features({result, Nodes}, _From, #jid{lserver = LServer} = _To, <<"">>, Lang) ->
    {result, [?NS_HTTP_UPLOAD | Nodes]};
get_disco_features(empty, From, To, Node, Lang) ->
    get_disco_features({result, []}, From, To, Node, Lang);
get_disco_features(Acc, _From, _To, _Node, _Lang) ->
    Acc.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec subhost(Host :: ejabberd:server()) -> binary().
subhost(Host) ->
    gen_mod:get_module_opt_subhost(Host, ?MODULE, ?DEFAULT_SUBHOST).


-spec my_disco_name(ejabberd:lang()) -> binary().
my_disco_name(Lang) ->
    translate:translate(Lang, <<"HTTP File Upload">>).


-spec compose_iq_reply(IQ :: ejabberd:iq(), PutUrl :: binary(), GetUrl :: binary()) ->
                              Reply :: ejabberd:iq().
compose_iq_reply(IQ, PutUrl, GetUrl) ->
    Slot = #xmlel{
              name = <<"slot">>,
              attrs=[{<<"xmlns">>, ?NS_HTTP_UPLOAD}],
              children =[#xmlel{name = <<"put">>, children = [exml:escape_cdata(PutUrl)]},
                         #xmlel{name = <<"get">>, children = [exml:escape_cdata(GetUrl)]}]},
    IQ#iq{type = result, sub_el =[Slot]}.


-spec token_bytes() -> pos_integer().
token_bytes() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, token_bytes, ?DEFAULT_TOKEN_BYTES).


-spec max_file_size() -> pos_integer() | undefined.
max_file_size() ->
    gen_mod:get_module_opt(?MYNAME, ?MODULE, max_file_size, ?DEFAULT_MAX_FILE_SIZE).


-spec with_default_backend(Opts :: proplists:proplist()) -> proplists:proplist().
with_default_backend(Opts) ->
    case lists:keyfind(backend, 1, Opts) of
        false -> [{backend, s3} | Opts];
        _ -> Opts
    end.


-spec module_opts() -> proplists:proplist().
module_opts() ->
    gen_mod:get_module_opts(?MYNAME, ?MODULE).


-spec generate_token() -> binary().
generate_token() ->
    base16:encode(crypto:strong_rand_bytes(token_bytes())).


-spec parse_request(Request :: exml:element()) ->
                           {Filename :: binary(), Size :: integer(),
                            ContentType :: binary() | undefined} |
                           bad_request.
parse_request(Request) ->
    Filename = exml_query:path(Request, [{element, <<"filename">>}, cdata]),
    SizeBin = exml_query:path(Request, [{element, <<"size">>}, cdata]),
    Size = (catch erlang:binary_to_integer(SizeBin)),
    ContentType = exml_query:path(Request, [{element, <<"content-type">>}, cdata]),

    case is_binary(Filename) andalso <<>> =/= Filename
        andalso is_integer(Size) andalso Size >= 0 of
        false -> bad_request;
        true -> {Filename, Size, ContentType}
    end.
