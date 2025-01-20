-module(mod_http_upload_SUITE).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-import(domain_helper, [host_type/0]).

-define(NS_XDATA, <<"jabber:x:data">>).
-define(NS_HTTP_UPLOAD_030, <<"urn:xmpp:http:upload:0">>).

-define(S3_HOSTNAME, <<"http://bucket.s3-eu-east-25.example.com">>).
-define(MINIO_HOSTNAME, <<"http://127.0.0.1:9000/mybucket/">>).

-define(MINIO_TEST_DATA, "qwerty").

-export([all/0, groups/0, suite/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).

-export([
	 test_minio_upload_without_content_type/1,
	 test_minio_upload_with_content_type/1,
	 http_upload_item_discovery/1,
	 http_upload_feature_discovery/1,
	 advertises_max_file_size/1,
	 request_slot/1,
	 rejects_set_iq/1,
	 rejects_disco_set_iq/1,
	 rejects_feature_discovery_with_node/1,
	 get_url_ends_with_filename/1,
	 get_url_ends_with_filename_with_unicode_characters/1,
	 urls_contain_s3_hostname/1,
	 rejects_empty_filename/1,
	 rejects_negative_filesize/1,
	 rejects_invalid_size_type/1,
	 denies_slots_over_max_file_size/1,
	 sends_different_put_and_get_urls/1,
	 escapes_urls_once/1
	]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_http_upload_s3}, {group, real_upload_with_acl},
     {group, real_upload_without_acl}].

groups() ->
    [{real_upload_with_acl, [], [test_minio_upload_without_content_type,
                                 test_minio_upload_with_content_type]},
     {real_upload_without_acl, [], [test_minio_upload_without_content_type,
                                    test_minio_upload_with_content_type]},
     {mod_http_upload_s3, [], [
                               http_upload_item_discovery,
                               http_upload_feature_discovery,
                               advertises_max_file_size,
                               request_slot,
                               rejects_set_iq,
                               rejects_disco_set_iq,
                               rejects_feature_discovery_with_node,
                               get_url_ends_with_filename,
                               get_url_ends_with_filename_with_unicode_characters,
                               urls_contain_s3_hostname,
                               rejects_empty_filename,
                               rejects_negative_filesize,
                               rejects_invalid_size_type,
                               denies_slots_over_max_file_size,
                               sends_different_put_and_get_urls,
                               escapes_urls_once
                              ]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).
end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(real_upload_without_acl, Config) ->
    case mongoose_helper:should_minio_be_running(Config) of
        true ->
            dynamic_modules:start(host_type(), mod_http_upload,
                create_opts(?MINIO_HOSTNAME, false)),
            escalus:create_users(Config, escalus:get_users([bob]));
        false -> {skip, "minio is not running"}
    end;
init_per_group(real_upload_with_acl, Config) ->
    case mongoose_helper:should_minio_be_running(Config) of
        true ->
            dynamic_modules:start(host_type(), mod_http_upload, create_opts(?MINIO_HOSTNAME, true)),
            [{with_acl, true} | escalus:create_users(Config, escalus:get_users([bob]))];
        false -> {skip, "minio is not running"}
    end;
init_per_group(_, Config) ->
    dynamic_modules:start(host_type(), mod_http_upload, create_opts(?S3_HOSTNAME, true)),
    escalus:create_users(Config, escalus:get_users([bob])).

end_per_group(_, Config) ->
    dynamic_modules:stop(host_type(), mod_http_upload),
    escalus:delete_users(Config, escalus:get_users([bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

create_opts(Host, AddAcl) ->
    config_parser_helper:mod_config(mod_http_upload,
    #{
        max_file_size => 1234,
        s3 => #{
            bucket_url => Host,
            add_acl => AddAcl,
            region => <<"eu-east-25">>,
            access_key_id => <<"AKIAIAOAONIULXQGMOUA">>,
            secret_access_key => <<"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">>
        }
    }).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

http_upload_item_discovery(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_items(ServJID)),
              escalus:assert(is_iq_result, Result),
              Query = exml_query:subelement(Result, <<"query">>),
              Item = exml_query:subelement_with_attr(Query, <<"jid">>, upload_service(Bob)),
              ?assertEqual(<<"HTTP File Upload">>, exml_query:attr(Item, <<"name">>))
      end).

http_upload_feature_discovery(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_info(ServJID)),
              escalus:assert(fun has_no_feature/2, [ns()], Result),
              SubServJID = upload_service(Bob),
              SubResult = escalus:send_and_wait(Bob, escalus_stanza:disco_info(SubServJID)),
              escalus:assert(has_feature, [ns()], SubResult)
      end).

advertises_max_file_size(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_info(ServJID)),
              Forms = exml_query:paths(Result, [{element, <<"query">>}, {element, <<"x">>}]),
              [Form] = lists:filter(
                         fun(F) -> has_field(<<"FORM_TYPE">>, <<"hidden">>, ns(), F) end,
                         Forms),

              escalus:assert(has_type, [<<"result">>], Form),
              escalus:assert(has_ns, [?NS_XDATA], Form),
              escalus:assert(fun has_field/4, [<<"max-file-size">>, undefined, <<"1234">>], Form),
              escalus:assert(has_identity, [<<"store">>, <<"file">>], Result)
      end).

rejects_set_iq(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              IQ = escalus_stanza:iq_set(ns(), []),
              Request = escalus_stanza:to(IQ, ServJID),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"cancel">>, <<"not-allowed">>)
      end).

rejects_disco_set_iq(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              IQ = escalus_stanza:iq_set(?NS_DISCO_INFO, []),
              Request = escalus_stanza:to(IQ, ServJID),
              Stanza = escalus:send_and_wait(Bob, Request),
              escalus:assert(is_iq_error, [Request], Stanza),
              escalus:assert(is_error, [<<"cancel">>, <<"not-allowed">>], Stanza),
              escalus:assert(is_stanza_from, [ServJID], Stanza)
      end).

rejects_feature_discovery_with_node(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = escalus_stanza:disco_info(ServJID, <<"bad-node">>),
              Stanza = escalus:send_and_wait(Bob, Request),
              escalus:assert(is_iq_error, [Request], Stanza),
              escalus:assert(is_error, [<<"cancel">>, <<"item-not-found">>], Stanza),
              escalus:assert(is_stanza_from, [ServJID], Stanza)
      end).

request_slot(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(is_iq_result, Result),
              escalus:assert(fun has_upload_namespace/1, Result),
              escalus:assert(fun has_put_and_get_fields/1, Result)
      end).

get_url_ends_with_filename(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Filename = <<"filename.jpg">>,
              Request = create_slot_request_stanza(ServJID, Filename, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun path_ends_with/3, [<<"get">>, Filename], Result)
      end).

get_url_ends_with_filename_with_unicode_characters(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Filename = unicode:characters_to_binary("très cool.jpg"),
              Request = create_slot_request_stanza(ServJID, Filename, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun path_ends_with/3, [<<"get">>, <<"tr%C3%A8s%20cool.jpg">>], Result)
      end).

urls_contain_s3_hostname(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun url_contains/3, [<<"get">>, ?S3_HOSTNAME], Result),
              escalus:assert(fun url_contains/3, [<<"put">>, ?S3_HOSTNAME], Result)
      end).

test_minio_upload_without_content_type(Config) ->
    test_minio_upload(Config, undefined).

test_minio_upload_with_content_type(Config) ->
    test_minio_upload(Config, <<"text/plain">>).

test_minio_upload(Config, ContentType) ->
    escalus:story(
        Config, [{bob, 1}],
        fun(Bob) ->
            ServJID = upload_service(Bob),
            FileSize = length(?MINIO_TEST_DATA),
            Request = create_slot_request_stanza(ServJID, <<"file.txt">>, FileSize, ContentType),
            Result = escalus:send_and_wait(Bob, Request),
            GetUrl = binary_to_list(extract_url(Result, <<"get">>)),
            PutUrl = binary_to_list(extract_url(Result, <<"put">>)),
            Header = generate_header(Config, ContentType),
            PutRetValue = request(put, PutUrl, Header, ?MINIO_TEST_DATA),
            ?assertMatch({200, _}, PutRetValue),
            GetRetValue = request(get, GetUrl, [], []),
            ?assertMatch({200, ?MINIO_TEST_DATA}, GetRetValue)
        end).

request(Method, PutUrl, Header, Data) ->
    PURL = #{host := Host, port := Port, path := Path} = uri_string:parse(PutUrl),
    {ok, ConnPid} = gun:open(Host, Port),
    {ok, _} = gun:await_up(ConnPid),
    FullPath =
        case PURL of
            #{query := Query} ->
                Path ++ "?" ++ Query;
            _ ->
                Path
        end,
    StreamRef =
        case Method of
            put ->
                gun:put(ConnPid, FullPath, Header, Data);
            get ->
                gun:get(ConnPid, FullPath)
        end,
    RespOpts = #{pid => ConnPid, stream_ref => StreamRef, acc => <<>>},
    #{status := Status, acc := Acc} = get_reponse(RespOpts),
    ok = gun:close(ConnPid),
    {Status, binary_to_list(Acc)}.

get_reponse(#{pid := Pid, stream_ref := StreamRef, acc := Acc} = Opts) ->
    case gun:await(Pid, StreamRef) of
        {response, fin, Status, _} ->
            Opts#{status => Status, acc => Acc};
        {response, nofin, Status, _} ->
            get_reponse(Opts#{status => Status});
        {data, nofin, Data} ->
            get_reponse(Opts#{acc => <<Acc/binary, Data/binary>>});
        {data, fin, Data} ->
            Opts#{acc => <<Acc/binary, Data/binary>>};
        Error ->
            Error
    end.

generate_header(Config, undefined) ->
    case proplists:get_value(with_acl, Config, false) of
        true ->
            [{<<"x-amz-acl">>, <<"public-read">>}];
        false ->
            []
    end;
generate_header(Config, ContentType) ->
    [{<<"content-type">>, ContentType} | generate_header(Config, undefined)].

rejects_empty_filename(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<>>, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

rejects_negative_filesize(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, -1, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

rejects_invalid_size_type(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"a.jpg">>, <<"filesize">>, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

denies_slots_over_max_file_size(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 54321, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Result),
              <<"1234">> = exml_query:path(Result, [{element, <<"error">>},
                                                    {element, <<"file-too-large">>},
                                                    {element, <<"max-file-size">>},
                                                    cdata])
      end).

sends_different_put_and_get_urls(Config) ->
    escalus:story(
     Config, [{bob, 1}],
     fun(Bob) ->
             ServJID = upload_service(Bob),
             Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123, undefined),
             Result = escalus:send_and_wait(Bob, Request),
             escalus:assert(fun urls_not_equal/1, Result)
     end).

escapes_urls_once(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun url_contains/3, [<<"put">>, <<"%3Bx-amz-acl">>], Result)
      end).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------
create_slot_request_stanza(Server, Filename, Size, ContentType) when is_integer(Size) ->
    create_slot_request_stanza(Server, Filename, integer_to_binary(Size), ContentType);
create_slot_request_stanza(Server, Filename, BinSize, ContentType) ->
    #xmlel{name     = <<"iq">>,
           attrs    = #{<<"type">> => <<"get">>, <<"to">> => Server},
           children = [create_request_element(Filename, BinSize, ContentType)]}.

create_request_element(Filename, BinSize, ContentType) ->
    ContentTypeAttr = case ContentType of
                        undefined -> #{};
                        _ -> #{<<"content-type">> => ContentType}
                    end,
    #xmlel{name  = <<"request">>,
           attrs = ContentTypeAttr#{<<"xmlns">> => ?NS_HTTP_UPLOAD_030,
                                    <<"filename">> => Filename,
                                    <<"size">> => BinSize}}.

has_upload_namespace(#xmlel{name = <<"iq">>, children = [#xmlel{ name = <<"slot">> } = Slot]}) ->
    ?NS_HTTP_UPLOAD_030 == exml_query:attr(Slot, <<"xmlns">>);
has_upload_namespace(_) ->
    false.

has_no_feature(Feature, Stanza) ->
    not escalus_pred:has_feature(Feature, Stanza).

has_put_and_get_fields(Elem = #xmlel{name = <<"iq">>}) ->
    PutUrl = extract_url(Elem, <<"put">>),
    GetUrl = extract_url(Elem, <<"get">>),
    is_binary(PutUrl) andalso is_binary(GetUrl)
        andalso byte_size(PutUrl) > 0 andalso byte_size(GetUrl) > 0;
has_put_and_get_fields(_Elem) ->
    false.

path_ends_with(UrlType, Filename, Result) ->
    Url = extract_url(Result, UrlType),
    #{ path := Path } = uri_string:parse(Url),
    FilenameSize = byte_size(Filename),
    ReverseFilename = reverse(Filename),
    case reverse(Path) of
        <<ReverseFilename:FilenameSize/binary, _/binary>> -> true;
        _ -> false
    end.

url_contains(UrlType, Filename, Result) ->
    Url = extract_url(Result, UrlType),
    binary:match(Url, Filename) =/= nomatch.

urls_not_equal(Result) ->
    Get = extract_url(Result, <<"get">>),
    Put = extract_url(Result, <<"put">>),
    Get =/= Put.

reverse(List) when is_list(List) ->
    list_to_binary(lists:reverse(List));
reverse(Binary) ->
    reverse(binary_to_list(Binary)).

upload_service(Client) ->
    <<"upload.", (escalus_client:server(Client))/binary>>.

has_field(Var, Type, Value, Form) ->
    Fields = Form#xmlel.children,
    VarFits = fun(I) -> Var =:= undefined orelse exml_query:attr(I, <<"var">>) =:= Var end,
    TypeFits = fun(I) -> Type =:= undefined orelse exml_query:attr(I, <<"type">>) =:= Type end,
    ValueFits =
        fun(I) ->
                Value =:= undefined orelse
                    Value =:= exml_query:path(I, [{element, <<"value">>}, cdata])
        end,
    lists:any(fun(Item) -> VarFits(Item) andalso TypeFits(Item) andalso ValueFits(Item) end,
              Fields).

extract_url(Result, UrlType) ->
    exml_query:path(Result, [{element, <<"slot">>}, {element, UrlType}, {attr, <<"url">>}]).

ns() -> ?NS_HTTP_UPLOAD_030.
