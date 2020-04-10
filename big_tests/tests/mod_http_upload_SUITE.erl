-module(mod_http_upload_SUITE).

-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_XDATA, <<"jabber:x:data">>).
-define(NS_HTTP_UPLOAD_025, <<"urn:xmpp:http:upload">>).
-define(NS_HTTP_UPLOAD_030, <<"urn:xmpp:http:upload:0">>).

-define(S3_HOSTNAME, "http://bucket.s3-eu-east-25.example.com").
-define(S3_OPTS, ?MOD_HTTP_UPLOAD_OPTS(?S3_HOSTNAME, true)).

-define(MINIO_HOSTNAME, "http://localhost:9000/mybucket/").
-define(MINIO_OPTS(AddAcl), ?MOD_HTTP_UPLOAD_OPTS(?MINIO_HOSTNAME, AddAcl)).

-define(MINIO_TEST_DATA, "qwerty").

-define(MOD_HTTP_UPLOAD_OPTS(Host, AddAcl),
    [
        {max_file_size, 1234},
        {s3, [
            {bucket_url, Host},
            {add_acl, AddAcl},
            {region, "eu-east-25"},
            {access_key_id, "AKIAIAOAONIULXQGMOUA"},
            {secret_access_key, "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"}
        ]}
    ]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_http_upload_s3}, {group, unset_size},
     {group, real_upload_with_acl}, {group, real_upload_without_acl}].

groups() ->
    G = [{unset_size, [], [does_not_advertise_max_size_if_unset]},
         {real_upload_with_acl, [], [test_minio_upload_without_content_type,
                                     test_minio_upload_with_content_type]},
         {real_upload_without_acl, [], [test_minio_upload_without_content_type,
                                        test_minio_upload_with_content_type]},
         {mod_http_upload_s3, [], [
                                   http_upload_item_discovery,
                                   http_upload_feature_discovery,
                                   advertises_max_file_size,
                                   request_slot,
                                   rejects_set_iq,
                                   get_url_ends_with_filename,
                                   urls_contain_s3_hostname,
                                   rejects_empty_filename,
                                   rejects_negative_filesize,
                                   rejects_invalid_size_type,
                                   denies_slots_over_max_file_size,
                                   sends_different_put_and_get_urls,
                                   escapes_urls_once
                                  ]}],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    ibrowse:start(),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    ibrowse:stop(),
    escalus:end_per_suite(Config).

init_per_group(unset_size, Config) ->
    dynamic_modules:start(host(), mod_http_upload, [{max_file_size, undefined} | ?S3_OPTS]),
    escalus:create_users(Config, escalus:get_users([bob]));
init_per_group(real_upload_without_acl, Config) ->
    case is_minio_running(Config) of
        true ->
            dynamic_modules:start(host(), mod_http_upload, ?MINIO_OPTS(false)),
            escalus:create_users(Config, escalus:get_users([bob]));
        false -> {skip, "minio is not running"}
    end;
init_per_group(real_upload_with_acl, Config) ->
    case is_minio_running(Config) of
        true ->
            dynamic_modules:start(host(), mod_http_upload, ?MINIO_OPTS(true)),
            [{with_acl, true} | escalus:create_users(Config, escalus:get_users([bob]))];
        false -> {skip, "minio is not running"}
    end;
init_per_group(_, Config) ->
    dynamic_modules:start(host(), mod_http_upload, ?S3_OPTS),
    escalus:create_users(Config, escalus:get_users([bob])).

end_per_group(_, Config) ->
    dynamic_modules:stop(host(), mod_http_upload),
    escalus:delete_users(Config, escalus:get_users([bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

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
              escalus:assert(has_item, [upload_service(Bob)], Result)
      end).

http_upload_feature_discovery(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = escalus_client:server(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_info(ServJID)),
              escalus:assert(fun has_no_feature/2, [Namespace], Result),
              SubServJID = upload_service(Bob),
              SubResult = escalus:send_and_wait(Bob, escalus_stanza:disco_info(SubServJID)),
              escalus:assert(has_feature, [Namespace], SubResult)
      end).

advertises_max_file_size(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_info(ServJID)),
              Forms = exml_query:paths(Result, [{element, <<"query">>}, {element, <<"x">>}]),
              [Form] = lists:filter(
                         fun(F) -> has_field(<<"FORM_TYPE">>, <<"hidden">>, Namespace, F) end,
                         Forms),

              escalus:assert(has_type, [<<"result">>], Form),
              escalus:assert(has_ns, [?NS_XDATA], Form),
              escalus:assert(fun has_field/4, [<<"max-file-size">>, undefined, <<"1234">>], Form)
      end).

does_not_advertise_max_size_if_unset(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = upload_service(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_info(ServJID)),
              undefined = exml_query:path(Result, {element, <<"x">>})
      end).

rejects_set_iq(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              IQ = escalus_stanza:iq_set(Namespace, []),
              Request = escalus_stanza:to(IQ, ServJID),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"cancel">>, <<"not-allowed">>)
      end).

request_slot(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123,
                                                   undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(is_iq_result, Result),
              escalus:assert(fun has_namespace/2, [Namespace], Result),
              escalus:assert(fun has_put_and_get_fields/2, [Namespace], Result)
      end).

get_url_ends_with_filename(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Filename = <<"filename.jpg">>,
              Request = create_slot_request_stanza(ServJID, Filename, 123, undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun path_ends_with/4, [<<"get">>, Filename, Namespace], Result)
      end).

urls_contain_s3_hostname(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123,
                                                   undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun url_contains/4, [<<"get">>, <<?S3_HOSTNAME>>, Namespace], Result),
              escalus:assert(fun url_contains/4, [<<"put">>, <<?S3_HOSTNAME>>, Namespace], Result)
      end).

test_minio_upload_without_content_type(Config) ->
    test_minio_upload(Config, undefined).

test_minio_upload_with_content_type(Config) ->
    test_minio_upload(Config, <<"text/plain">>).

test_minio_upload(Config, ContentType) ->
    namespaced_story(
        Config, [{bob, 1}],
        fun(Namespace, Bob) ->
            ServJID = upload_service(Bob),
            FileSize = length(?MINIO_TEST_DATA),
            Request = create_slot_request_stanza(ServJID, <<"somefile.txt">>, FileSize,
                                                 ContentType, Namespace),
            Result = escalus:send_and_wait(Bob, Request),
            GetUrl = binary_to_list(extract_url(Result, <<"get">>, Namespace)),
            PutUrl = binary_to_list(extract_url(Result, <<"put">>, Namespace)),
            Header = generate_header(Config, ContentType),
            PutRetValue = ibrowse:send_req(PutUrl, Header, put, ?MINIO_TEST_DATA),
            ?assertMatch({ok, "200", _, []}, PutRetValue),
            GetRetValue = ibrowse:send_req(GetUrl, [], get),
            ?assertMatch({ok, "200", _, ?MINIO_TEST_DATA}, GetRetValue)
        end).

generate_header(Config, undefined) ->
    case proplists:get_value(with_acl, Config, false) of
        true ->
            [{<<"x-amz-acl">>, <<"public-read">>}];
        false ->
            []
    end;
generate_header(Config, ContentType) ->
    [{<<"Content-Type">>, ContentType} | generate_header(Config, undefined)].

rejects_empty_filename(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<>>, 123, undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

rejects_negative_filesize(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, -1,
                                                   undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

rejects_invalid_size_type(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>,
                                                   <<"filesize">>, undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

denies_slots_over_max_file_size(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 54321,
                                                   undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(is_error, [<<"modify">>, <<"not-acceptable">>], Result),
              <<"1234">> = exml_query:path(Result, [{element, <<"error">>},
                                                    {element, <<"file-too-large">>},
                                                    {element, <<"max-file-size">>},
                                                    cdata])
      end).

sends_different_put_and_get_urls(Config) ->
    namespaced_story(
     Config, [{bob, 1}],
     fun(Namespace, Bob) ->
             ServJID = upload_service(Bob),
             Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123,
                                                  undefined, Namespace),
             Result = escalus:send_and_wait(Bob, Request),
             escalus:assert(fun urls_not_equal/2, [Namespace], Result)
     end).

escapes_urls_once(Config) ->
    namespaced_story(
      Config, [{bob, 1}],
      fun(Namespace, Bob) ->
              ServJID = upload_service(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123,
                                                   undefined, Namespace),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun url_contains/4, [<<"put">>, <<"%3Bx-amz-acl">>, Namespace], Result)
      end).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------
is_minio_running(Config) ->
    case proplists:get_value(preset, Config, undefined) of
        undefined -> false;
        Preset ->
            PresetAtom = list_to_existing_atom(Preset),
            DBs = ct:get_config({ejabberd_presets, PresetAtom, dbs}, []),
            lists:member(minio, DBs)
    end.

create_slot_request_stanza(Server, Filename, Size, ContentType, Namespace) when is_integer(Size) ->
    create_slot_request_stanza(Server, Filename, integer_to_binary(Size), ContentType, Namespace);
create_slot_request_stanza(Server, Filename, BinSize, ContentType, Namespace) ->
    #xmlel{name     = <<"iq">>,
           attrs    = [{<<"type">>, <<"get">>}, {<<"to">>, Server}],
           children = [create_request_for_namespace(Filename, BinSize, ContentType, Namespace)]}.

create_request_for_namespace(Filename, BinSize, ContentType, Namespace = ?NS_HTTP_UPLOAD_025) ->
    ContentTypeEl =
        case ContentType of
            undefined -> [];
            _ -> [#xmlel{name = <<"content-type">>, children = [#xmlcdata{content = ContentType}]}]
        end,
    #xmlel{name     = <<"request">>,
           attrs    = [{<<"xmlns">>, Namespace}],
           children =
               [#xmlel{name = <<"filename">>, children = [#xmlcdata{content = Filename}]},
                #xmlel{name = <<"size">>, children = [#xmlcdata{content = BinSize}]}
                | ContentTypeEl]};
create_request_for_namespace(Filename, BinSize, ContentType, Namespace = ?NS_HTTP_UPLOAD_030) ->
    ContentTypeEl = case ContentType of
                        undefined -> [];
                        _ -> [{<<"content-type">>, ContentType}]
                    end,
    #xmlel{name  = <<"request">>,
           attrs = [{<<"xmlns">>, Namespace},
                    {<<"filename">>, Filename},
                    {<<"size">>, BinSize}
                    | ContentTypeEl]}.

has_namespace(Namespace, #xmlel{name = <<"iq">>, children = [Slot]}) ->
    case Slot of
        #xmlel{name = <<"slot">>, attrs = [{<<"xmlns">>, Namespace}]} -> true;
        _ -> false
    end;
has_namespace(_Namespace, _) ->
    false.

has_no_feature(Feature, Stanza) ->
    not escalus_pred:has_feature(Feature, Stanza).

has_put_and_get_fields(Namespace, Elem = #xmlel{name = <<"iq">>}) ->
    PutUrl = extract_url(Elem, <<"put">>, Namespace),
    GetUrl = extract_url(Elem, <<"get">>, Namespace),
    is_binary(PutUrl) andalso is_binary(GetUrl)
        andalso byte_size(PutUrl) > 0 andalso byte_size(GetUrl) > 0;
has_put_and_get_fields(_Namespace, _Elem) ->
    false.

path_ends_with(UrlType, Filename, Namespace, Result) ->
    Url = extract_url(Result, UrlType, Namespace),
    {ok, {_, _, _, _, PathList, _}} = http_uri:parse(binary_to_list(Url)),
    FilenameSize = byte_size(Filename),
    ReverseFilename = reverse(Filename),
    case reverse(PathList) of
        <<ReverseFilename:FilenameSize/binary, _/binary>> -> true;
        _ -> false
    end.

url_contains(UrlType, Filename, Namespace, Result) ->
    Url = extract_url(Result, UrlType, Namespace),
    binary:match(Url, Filename) =/= nomatch.

urls_not_equal(Namespace, Result) ->
    Get = extract_url(Result, <<"get">>, Namespace),
    Put = extract_url(Result, <<"put">>, Namespace),
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

host() ->
    ct:get_config({hosts, mim, domain}).

extract_url(Result, UrlType, ?NS_HTTP_UPLOAD_025) ->
    exml_query:path(Result, [{element, <<"slot">>}, {element, UrlType}, cdata]);
extract_url(Result, UrlType, ?NS_HTTP_UPLOAD_030) ->
    exml_query:path(Result, [{element, <<"slot">>}, {element, UrlType}, {attr, <<"url">>}]).

namespaced_story(Config, Users, Story) ->
    lists:foreach(
      fun(Namespace) ->
              escalus:story(Config, Users, fun(User) -> Story(Namespace, User) end)
      end,
      [?NS_HTTP_UPLOAD_025, ?NS_HTTP_UPLOAD_030]).
