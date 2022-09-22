-module(mod_http_upload_api).

-export([get_urls/5, get_urls_mongooseimctl/5]).

-ignore_xref([get_urls_mongooseimctl/5]).

-spec get_urls_mongooseimctl(Domain :: jid:lserver(), Filename :: binary(), Size :: pos_integer(),
               ContentType :: binary() | undefined, Timeout :: pos_integer()) ->
                {ok | error, string()}.
get_urls_mongooseimctl(Domain, Filename, Size, ContentType, Timeout) ->
    case get_urls(Domain, Filename, Size, ContentType, Timeout) of
        {ok, #{<<"PutUrl">> := PutURL, <<"GetUrl">> := GetURL, <<"Header">> := Header}} ->
            {ok, generate_output_message(PutURL, GetURL, Header)};
        {_, Message} ->
            {error, Message}
    end.

-spec get_urls(Domain :: jid:lserver(), Filename :: binary(), Size :: pos_integer(),
               ContentType :: binary() | undefined, Timeout :: pos_integer()) ->
        {ok | size_error | timeout_error | module_not_loaded_error | domain_not_found |
         file_too_large_error, string()} | {ok, #{binary() => binary() | string()}}.
get_urls(_Domain, _Filename, Size, _ContentType, _Timeout) when Size =< 0 ->
    {size_error, "size must be positive integer"};
get_urls(_Domain, _Filename, _Size, _ContentType, Timeout) when Timeout =< 0 ->
    {timeout_error, "timeout must be positive integer"};
get_urls(Domain, Filename, Size, <<>>, Timeout) ->
    get_urls(Domain, Filename, Size, undefined, Timeout);
get_urls(Domain, Filename, Size, ContentType, Timeout) ->
    case mongoose_domain_api:get_domain_host_type(Domain) of
        {ok, HostType} ->
            check_module_and_get_urls(HostType, Filename, Size, ContentType, Timeout);
        _ ->
            {domain_not_found, "domain does not exist"}
    end.

check_module_and_get_urls(HostType, Filename, Size, ContentType, Timeout) ->
    %The check if the module is loaded is needed by one test in mongooseimctl_SUITE
    case gen_mod:is_loaded(HostType, mod_http_upload) of
        true ->
            case mod_http_upload:get_urls(HostType, Filename, Size, ContentType, Timeout) of
                {PutURL, GetURL, Header} ->
                    {ok, #{<<"PutUrl">> => PutURL, <<"GetUrl">> => GetURL,
                           <<"Header">> => header_output(Header)}};
                file_too_large_error ->
                    {file_too_large_error,
                     "Declared file size exceeds the host's maximum file size."}
            end;
        false ->
            {module_not_loaded_error, "mod_http_upload is not loaded for this host"}
    end.

-spec generate_output_message(PutURL :: binary(), GetURL :: binary(),
                              Header :: string()) -> string().
generate_output_message(PutURL, GetURL, Header) ->
    PutURLOutput = url_output("PutURL:", PutURL),
    GetURLOutput = url_output("GetURL:", GetURL),
    lists:flatten([PutURLOutput, GetURLOutput, Header]).

url_output(Name, Url) ->
    io_lib:format("~s ~s~n", [Name, Url]).

header_output(Header) when Header =:= #{} -> [];
header_output(Header) ->
    io_lib:format("Header: ~p~n", [maps:to_list(Header)]).
