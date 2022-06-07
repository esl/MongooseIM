-module(mod_http_upload_api).

-export([get_urls/5]).

-spec get_urls(Domain :: jid:lserver(), Filename :: binary(), Size :: pos_integer(),
               ContentType :: binary() | undefined, Timeout :: pos_integer()) ->
        {ok | size_error | timeout_error | module_not_loaded, string()}.
get_urls(_Domain, _Filename, Size, _ContentType, _Timeout) when Size =< 0->
    {size_error, "size must be positive integer"};
get_urls(_Domain, _Filename, _Size, _ContentType, Timeout) when Timeout =< 0->
    {timeout_error, "timeout must be positive integer"};
get_urls(Domain, Filename, Size, <<"">>, Timeout) ->
    get_urls(Domain, Filename, Size, undefined, Timeout);
get_urls(Domain, Filename, Size, ContentType, Timeout) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(Domain),
    case gen_mod:is_loaded(HostType, mod_http_upload) of
        true ->
            {PutURL, GetURL, Header} =
                mod_http_upload:get_urls(HostType, Filename, Size, ContentType, Timeout),
            {ok, generate_output_message(PutURL, GetURL, Header)};
        false ->
            {module_not_loaded_error, "mod_http_upload is not loaded for this host"}
    end.

-spec generate_output_message(PutURL :: binary(), GetURL :: binary(),
                              Headers :: #{binary() => binary()}) -> string().
generate_output_message(PutURL, GetURL, Header) ->
    PutURLOutput = url_output("PutURL:", PutURL),
    GetURLOutput = url_output("GetURL:", GetURL),
    HeaderOutput = header_output(Header),
    lists:flatten([PutURLOutput, GetURLOutput, HeaderOutput]).

url_output(Name, Url) ->
    io_lib:format("~s ~s~n", [Name, Url]).

header_output(Header) when Header =:= #{} -> [];
header_output(Header) ->
    io_lib:format("Header: ~p~n", [maps:to_list(Header)]).