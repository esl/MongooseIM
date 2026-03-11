-module(mod_http_upload_api).

-export([get_urls/5]).

-spec get_urls(Domain :: jid:lserver(), Filename :: nonempty_binary(), Size :: pos_integer(),
               ContentType :: binary() | null | undefined, Timeout :: pos_integer()) ->
        {ok, #{binary() => term()}}
        | {size_error | timeout_error | domain_not_found | file_too_large_error, string()}.
get_urls(Domain, Filename, Size, ContentType, Timeout) ->
    ContentType1 = content_type(ContentType),
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            do_get_urls(HostType, Filename, Size, ContentType1, Timeout);
        _ ->
            {domain_not_found, "domain does not exist"}
    end.

content_type(null) -> undefined;
content_type(<<>>) -> undefined;
content_type(Binary) -> Binary.

do_get_urls(HostType, Filename, Size, ContentType, Timeout) ->
    case mod_http_upload:get_urls(HostType, Filename, Size, ContentType, Timeout) of
        {PutURL, GetURL, Headers} ->
            Headers1 =
                [{ok, #{~"name" => Name, ~"value" => Value}} || Name := Value <- Headers],
            {ok, #{~"putUrl" => PutURL, ~"getUrl" => GetURL, ~"headers" => Headers1}};
        file_too_large_error ->
            {file_too_large_error, "Declared file size exceeds the host's maximum file size."}
    end.
