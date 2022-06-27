%% @doc Provide an interface for frontends (like graphql or ctl) to manage private.
-module(mod_private_api).

-include("mongoose.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-export([private_get/3, private_set/2]).

-spec private_get(jid:jid(), binary(), binary()) -> {Res, iodata()} when
    Res :: ok | not_found.
private_get(JID, Element, Ns) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            {ok, do_private_get(JID, Element, Ns)};
        false ->
            {not_found, io_lib:format("User ~s does not exist", [jid:to_binary(JID)])}
    end.

-spec private_set(jid:jid(), ElementString :: binary()) -> {Res, iolist()} when
    Res :: ok | not_found | not_loaded | parse_error.
private_set(JID, ElementString) ->
    case exml:parse(ElementString) of
        {error, Error} ->
            String = io_lib:format("Error found parsing the element:~n  ~p~nError: ~p~n",
                      [ElementString, Error]),
            {parse_error, String};
        {ok, Xml} ->
            do_private_set(JID, Xml)
    end.

do_private_get(JID, Element, Ns) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(JID#jid.lserver),
    Xml = #xmlel{ name = Element, attrs = [{<<"xmlns">>, Ns}]},
    {_, ResIq} = send_iq(get, Xml, JID, HostType),
    [#xmlel{ name = <<"query">>,
             attrs = [{<<"xmlns">>, ?NS_PRIVATE}],
             children = [SubEl] }] = ResIq#iq.sub_el,
    exml:to_binary(SubEl).

do_private_set(JID, Xml) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            do_private_set2(JID, Xml);
        false ->
            {not_found, io_lib:format("User ~s does not exist", [jid:to_binary(JID)])}
    end.

do_private_set2(#jid{lserver = Domain} = JID, Xml) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(Domain),
    case is_private_module_loaded(HostType) of
        true ->
            send_iq(set, Xml, JID, HostType),
            {ok, ""};
        false ->
            {not_loaded, io_lib:format("Module mod_private is not loaded on domain ~s", [Domain])}
    end.

-spec is_private_module_loaded(jid:server()) -> true | false.
is_private_module_loaded(Server) ->
    lists:member(mod_private, gen_mod:loaded_modules(Server)).

send_iq(Method, Xml, From = To = _JID, HostType) ->
    IQ = {iq, <<"">>, Method, ?NS_PRIVATE, <<"">>,
          #xmlel{ name = <<"query">>,
                  attrs = [{<<"xmlns">>, ?NS_PRIVATE}],
                  children = [Xml] } },
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              from_jid => From,
                              to_jid => To,
                              lserver => From#jid.lserver,
                              host_type => HostType,
                              element => jlib:iq_to_xml(IQ) }),
    mod_private:process_iq(Acc, From, To, IQ, #{}).
