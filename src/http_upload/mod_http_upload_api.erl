-module(mod_http_upload_api).

-export([get_urls/5, get_urls_mongooseimctl/5]).

-ignore_xref([get_urls_mongooseimctl/5]).

-spec get_urls_mongooseimctl(Domain :: jid:lserver(), Filename :: binary(), Size :: pos_integer(),
               ContentType :: binary() | undefined, Timeout :: pos_integer()) ->
                {ok | error, string()}.
get_urls_mongooseimctl(_Domain, _Filename, Size, _ContentType, _Timeout) when Size =< 0 ->
    {error, "size must be positive integer"};
get_urls_mongooseimctl(_Domain, _Filename, _Size, _ContentType, Timeout) when Timeout =< 0 ->
    {error, "timeout must be positive integer"};
get_urls_mongooseimctl(Domain, Filename, Size, ContentType, Timeout) ->
    case get_urls(Domain, Filename, Size, ContentType, Timeout) of
        {ok, #{<<"putUrl">> := PutURL, <<"getUrl">> := GetURL, <<"headers">> := Headers}} ->
            {ok, generate_output_message(PutURL, GetURL, Headers)};
        {_, Message} ->
            {error, Message}
    end.

-spec get_urls(Domain :: jid:lserver(), Filename :: nonempty_binary(), Size :: pos_integer(),
               ContentType :: binary() | null | undefined, Timeout :: pos_integer()) ->
        {ok, #{binary() => term()}}
        | {size_error | timeout_error | module_not_loaded_error | domain_not_found |
           file_too_large_error, string()}.
get_urls(Domain, Filename, Size, ContentType, Timeout) ->
    ContentType1 = content_type(ContentType),
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            check_module_and_get_urls(HostType, Filename, Size, ContentType1, Timeout);
        _ ->
            {domain_not_found, "domain does not exist"}
    end.

content_type(null) -> undefined;
content_type(<<>>) -> undefined;
content_type(Binary) -> Binary.

check_module_and_get_urls(HostType, Filename, Size, ContentType, Timeout) ->
    case gen_mod:is_loaded(HostType, mod_http_upload) of
        true ->
            case mod_http_upload:get_urls(HostType, Filename, Size, ContentType, Timeout) of
                {PutURL, GetURL, Headers} ->
                    Headers1 = lists:map(fun({Name, Value}) -> {ok, #{<<"name">> => Name, <<"value">> => Value}} end,
                                         maps:to_list(Headers)),
                    {ok, #{<<"putUrl">> => PutURL, <<"getUrl">> => GetURL,
                           <<"headers">> => Headers1}};
                file_too_large_error ->
                    {file_too_large_error,
                     "Declared file size exceeds the host's maximum file size."}
            end;
        false ->
            {module_not_loaded_error, "mod_http_upload is not loaded for this host"}
    end.

-spec generate_output_message(PutURL :: binary(),
                              GetURL :: binary(),
                              Headers :: [{ok, map()}]) -> string().
generate_output_message(PutURL, GetURL, Headers) ->
    PutURLOutput = url_output("PutURL:", PutURL),
    GetURLOutput = url_output("GetURL:", GetURL),
    HeadersOutput = headers_output(Headers),
    lists:flatten([PutURLOutput, GetURLOutput, HeadersOutput]).

url_output(Name, Url) ->
    io_lib:format("~s ~s~n", [Name, Url]).

headers_output(Headers) ->
    List = [{Name, Value} || {ok, #{<<"name">> := Name, <<"value">> := Value}} <- Headers],
    io_lib:format("Header: ~p~n", [List]).
