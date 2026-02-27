%% @doc Provide an interface for frontends (like graphql or ctl) to manage private.
-module(mod_private_api).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml.hrl").

-export([private_get/4, private_set/3]).

-spec private_get(jid:jid(), binary(), binary(), user_api | admin_api) ->
    {ok, exml:element()} | {Error, string()} when
    Error :: not_found.
private_get(JID, Element, Ns, Origin) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            do_private_get(JID, Element, Ns, Origin);
        false ->
            {not_found, io_lib:format("User ~ts does not exist", [jid:to_binary(JID)])}
    end.

-spec private_set(jid:jid(), exml:element(), user_api | admin_api) ->
    {ok, exml:element()} | {not_found, iolist()}.
private_set(#jid{lserver = Domain} = JID, Xml, Origin) ->
    case ejabberd_auth:does_user_exist(JID) of
        true ->
            {ok, HostType} = mongoose_domain_api:get_domain_host_type(Domain),
            send_iq(set, Xml, JID, HostType, Origin),
            {ok, Xml};
        false ->
            {not_found, io_lib:format("User ~ts does not exist", [jid:to_binary(JID)])}
    end.

do_private_get(JID, Element, Ns, Origin) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(JID#jid.lserver),
    Xml = #xmlel{ name = Element, attrs = #{<<"xmlns">> => Ns}},
    {_, ResIq} = send_iq(get, Xml, JID, HostType, Origin),
    [#xmlel{ name = <<"query">>,
             attrs = #{<<"xmlns">> := ?NS_PRIVATE},
             children = [SubEl] }] = ResIq#iq.sub_el,
    {ok, SubEl}.

send_iq(Method, Xml, JID, HostType, Origin) ->
    From = To = JID,
    IQ = {iq, <<>>, Method, ?NS_PRIVATE, <<>>,
          #xmlel{ name = <<"query">>,
                  attrs = #{<<"xmlns">> => ?NS_PRIVATE},
                  children = [Xml] } },
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              from_jid => From,
                              to_jid => To,
                              lserver => From#jid.lserver,
                              host_type => HostType,
                              element => jlib:iq_to_xml(IQ),
                              origin => Origin }),
    mod_private:process_iq(Acc, From, To, IQ, #{}).
