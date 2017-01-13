-module(mod_http_upload_SUITE).
-compile(export_all).
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-define(NS_HTTP_UPLOAD, <<"urn:xmpp:http:upload">>).
-define(S3_HOSTNAME, "http://bucket.s3-eu-east-25.example.com").
-define(S3_OPTS,
        [
         {s3, [
               {bucket_url, ?S3_HOSTNAME},
               {region, "eu-east-25"},
               {access_key_id, "AKIAIAOAONIULXQGMOUA"},
               {secret_access_key, "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"}
              ]}
        ]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, mod_http_upload_s3}].

groups() ->
    [{mod_http_upload_s3, [], [
                               http_upload_service_discovery,
                               request_slot,
                               get_url_ends_with_filename,
                               urls_contain_s3_hostname,
                               rejects_empty_filename,
                               rejects_negative_filesize,
                               rejects_invalid_size_type
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

init_per_group(mod_http_upload_s3, Config) ->
    dynamic_modules:start(<<"localhost">>, mod_http_upload, ?S3_OPTS),
    escalus:create_users(Config, escalus:get_users([bob])).

end_per_group(_, Config) ->
    dynamic_modules:stop(<<"localhost">>, mod_http_upload),
    escalus:delete_users(Config, escalus:get_users([bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Service discovery test
%%--------------------------------------------------------------------

http_upload_service_discovery(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Result = escalus:send_and_wait(Bob, escalus_stanza:disco_info(ServJID)),
              escalus:assert(is_iq_result, Result),
              escalus:assert(has_feature, [?NS_HTTP_UPLOAD], Result)
      end).

request_slot(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(is_iq_result, Result),
              escalus:assert(fun check_namespace/1, Result),
              escalus:assert(fun check_put_and_get_presence/1, Result)
      end).

get_url_ends_with_filename(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Filename = <<"filename.jpg">>,
              Request = create_slot_request_stanza(ServJID, Filename, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun check_path_ends_with/3, [<<"get">>, Filename], Result)
      end).

urls_contain_s3_hostname(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus:assert(fun check_url_contains/3, [<<"get">>, <<?S3_HOSTNAME>>], Result),
              escalus:assert(fun check_url_contains/3, [<<"put">>, <<?S3_HOSTNAME>>], Result)
      end).

rejects_empty_filename(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Request = create_slot_request_stanza(ServJID, <<>>, 123, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

rejects_negative_filesize(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>, -1, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

rejects_invalid_size_type(Config) ->
    escalus:story(
      Config, [{bob, 1}],
      fun(Bob) ->
              ServJID = escalus_client:server(Bob),
              Request = create_slot_request_stanza(ServJID, <<"filename.jpg">>,
                                                   <<"filesize">>, undefined),
              Result = escalus:send_and_wait(Bob, Request),
              escalus_assert:is_error(Result, <<"modify">>, <<"bad-request">>)
      end).

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

create_slot_request_stanza(Server, Filename, Size, ContentType) when is_integer(Size) ->
    create_slot_request_stanza(Server, Filename, integer_to_binary(Size), ContentType);
create_slot_request_stanza(Server, Filename, BinSize, ContentType) ->
    ContentTypeEl =
        case ContentType of
            undefined -> [];
            _ -> [#xmlel{name = <<"content-type">>, children = [exml:escape_cdata(ContentType)]}]
        end,

    Request =
        #xmlel{
           name = <<"request">>,
           attrs = [{<<"xmlns">>, ?NS_HTTP_UPLOAD}],
           children =
               [
                #xmlel{name = <<"filename">>, children = [exml:escape_cdata(Filename)]},
                #xmlel{name = <<"size">>, children = [exml:escape_cdata(BinSize)]}
                | ContentTypeEl
               ]},

    #xmlel{
       name = <<"iq">>,
       attrs = [{<<"type">>, <<"get">>}, {<<"to">>, Server}],
       children = [Request]}.

check_namespace(#xmlel{name = <<"iq">>, children = [Slot]}) ->
    case Slot of
        #xmlel{name = <<"slot">>, attrs = [{<<"xmlns">>, ?NS_HTTP_UPLOAD}]} -> true;
        _ -> false
    end;
check_namespace(_) ->
    false.

check_put_and_get_presence(#xmlel{name = <<"iq">>, children = [Slot]}) ->
    check_put_and_get_presence(Slot);
check_put_and_get_presence(#xmlel{name = <<"slot">>, children = PutGet}) ->
    Put = lists:keyfind(<<"put">>, 2, PutGet),
    Get = lists:keyfind(<<"get">>, 2, PutGet),
    check_put_and_get_presence(Put) andalso check_put_and_get_presence(Get);
check_put_and_get_presence(#xmlel{name = Name, children = [#xmlcdata{content = Content}]})
  when Name =:= <<"put">>; Name =:= <<"get">> ->
    is_binary(Content) andalso Content =/= <<>>;
check_put_and_get_presence(_) ->
    false.

check_path_ends_with(UrlType, Filename, Result) ->
    Url = exml_query:path(Result, [{element, <<"slot">>}, {element, UrlType}, cdata]),
    {ok, {_, _, _, _, PathList, _}} = http_uri:parse(binary_to_list(Url)),
    FilenameSize = byte_size(Filename),
    ReverseFilename = reverse(Filename),
    case reverse(PathList) of
        <<ReverseFilename:FilenameSize/binary, _/binary>> -> true;
        _ -> false
    end.

check_url_contains(UrlType, Filename, Result) ->
    Url = exml_query:path(Result, [{element, <<"slot">>}, {element, UrlType}, cdata]),
    binary:match(Url, Filename) =/= nomatch.

reverse(List) when is_list(List) ->
    list_to_binary(lists:reverse(List));
reverse(Binary) ->
    reverse(binary_to_list(Binary)).
