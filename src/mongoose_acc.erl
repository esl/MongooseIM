%%%-------------------------------------------------------------------
%%% @doc
%%% TODO: docs
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_acc).

-include("jlib.hrl").
-include("mongoose.hrl").

%%%-------------------------------------------------------------------
%%% Exported types
%%%-------------------------------------------------------------------

-export_type([t/0]).
-export_type([element/0, element_name/0, element_type/0, element_attrs/0]).

-export_type([getter_result/1]).

                 % Key name starting with 'a' will make it appear early when the accumulator is
                 % printed and the map has only a few entries. Possibly could help with debugging.
-opaque t() :: #{accumulator   := true,
                 init_location := init_location(),
                 element       => element_props(),
                 from          => jid_props(),
                 to            => jid_props()}.

-type element()       :: exml:element() | jlib:iq().
-type element_name()  :: binary().
-type element_type()  :: binary() | undefined.
-type element_attrs() :: [exml:attr()].

-type getter_result(Prop) :: Prop | no_return().

%%%-------------------------------------------------------------------
%%% Internal types
%%%-------------------------------------------------------------------

-type init_location() :: {Module :: module(), Function :: atom(), Line :: pos_integer()}.
-type prop() :: {element, element_props()}
              | {from, jid_props()}
              | {to, jid_props()}.
-type element_props() :: #{record := element(),
                           name   := element_name(),
                           type   := element_type(),
                           attrs  := element_attrs()}.
-type jid_props() :: #{jid     := jid:jid(),
                       bin_jid := binary()}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-export([new/3, new/4, new/5, new/6]).
-export([set_element/2]).
-export([get_element/1, get_element_name/1, get_element_type/1, get_element_attrs/1]).
-export([get_from_jid/1, get_from_bin/1]).
-export([get_to_jid/1, get_to_bin/1]).

%%%-------------------------------------------------------------------
% @doc Creates a new accumulator instance given location in the source code.
%
% The location arguments are required because they make it easier to debug where the accumulator
% is coming from, whether it was initialized in the XMPP, HTTP or CLI context.
% @end
%-------------------------------------------------------------------
-spec new(module(), atom(), pos_integer()) -> t().
new(Module, Function, Line) when is_atom(Module),
                                 is_atom(Function),
                                 is_integer(Line),
                                 Line > 0 ->
    #{accumulator => true, init_location => {Module, Function, Line}}.

%-------------------------------------------------------------------
% @doc Creates a new accumulator instance given XML stanza which initiated the processing and
% location in the source code.
%
% See `new/3' for more information.
% @end
%-------------------------------------------------------------------
-spec new(element(), module(), atom(), pos_integer()) -> t().
new(Element, Module, Function, Line) ->
    set_element(new(Module, Function, Line), Element).

%-------------------------------------------------------------------
% @doc Creates a new accumulator instance given XML stanza which initiated the processing,
% JID of the sender of that stanza and location in the source code.
%
% See `new/3' for more information.
% @end
%-------------------------------------------------------------------
-spec new(element(), jid:jid(), module(), atom(), pos_integer()) -> t().
new(Element, From, Module, Function, Line) ->
    set_from(new(Element, Module, Function, Line), From).

%-------------------------------------------------------------------
% @doc Creates a new accumulator instance given XML stanza which initiated the processing,
% JIDs of the sender and the recipient of that stanza, and location in the source code.
%
% See `new/3' for more information.
% @end
%-------------------------------------------------------------------
-spec new(element(), jid:jid(), jid:jid(), module(), atom(), pos_integer()) -> t().
new(Element, From, To, Module, Function, Line) ->
    set_to(new(Element, From, Module, Function, Line), To).

%%% Setters

%-------------------------------------------------------------------
% @doc Sets XML stanza in the given accumulator.
%
% This function should be used very rarely, only when it is absolutely necessary to modify the
% stanza for further processing. Examples include MAM or AMP which inject additional elements into
% the original stanza.
%
% Ideally stanza should be set using `new/4,5,6'.
% @end
%-------------------------------------------------------------------
-spec set_element(t(), element()) -> t().
set_element(Acc, #xmlel{name = ElName, attrs = ElAttrs} = El) ->
    ElType = exml_query:attr(El, <<"type">>, undefined),
    ElProps = #{record => El,
                name   => ElName,
                type   => ElType,
                attrs  => ElAttrs},
    put_prop(Acc, {element, ElProps});
set_element(Acc, #iq{type = IqType} = El) ->
    ElType = atom_to_binary(IqType, latin1),
    ElAttrs = [{<<"type">>, ElType}],
    ElProps = #{record => El,
                name   => <<"iq">>,
                type   => ElType,
                attrs  => ElAttrs},
    put_prop(Acc, {element, ElProps}).

%%% Getters

%-------------------------------------------------------------------
% @doc Retrieves the whole XML element record set using `new/4,5,6'.
%
% Returns `error' atom if element isn't set.
% @end
%-------------------------------------------------------------------
-spec get_element(t()) -> getter_result(element()).
get_element(Acc) ->
    maps:get(record, get_prop(Acc, element, new)).

%-------------------------------------------------------------------
% @doc Retrieves name of the XML element record set using `new/4,5,6'.
%
% Returns `error' atom if element isn't set.
% @end
%-------------------------------------------------------------------
-spec get_element_name(t()) -> getter_result(element_name()).
get_element_name(Acc) ->
    maps:get(name, get_prop(Acc, element, new)).

%-------------------------------------------------------------------
% @doc Retrieves type of the XML element set using `new/4,5,6'.
%
% Returns `error' atom if element isn't set.
% @end
%-------------------------------------------------------------------
-spec get_element_type(t()) -> getter_result(element_type()).
get_element_type(Acc) ->
    maps:get(type, get_prop(Acc, element, new)).

%-------------------------------------------------------------------
% @doc Retrieves list of attributes of the XML element set using `new/4,5,6'.
%
% Returns `error' atom if element isn't set.
% @end
%-------------------------------------------------------------------
-spec get_element_attrs(t()) -> getter_result(element_attrs()).
get_element_attrs(Acc) ->
    maps:get(attrs, get_prop(Acc, element, new)).

%-------------------------------------------------------------------
% @doc Retrieves `jid:jid()' representation of the sender of the stanza set using `new/5,6'.
%
% Returns `error' atom if sender isn't set.
% @end
%-------------------------------------------------------------------
-spec get_from_jid(t()) -> getter_result(jid:jid()).
get_from_jid(Acc) ->
    maps:get(jid, get_prop(Acc, from, new)).

%-------------------------------------------------------------------
% @doc Retrieves binary representation of the sender of the stanza set using `new/5,6'.
%
% Returns `error' atom if sender isn't set.
% @end
%-------------------------------------------------------------------
-spec get_from_bin(t()) -> getter_result(binary()).
get_from_bin(Acc) ->
    maps:get(bin_jid, get_prop(Acc, from, new)).

%-------------------------------------------------------------------
% @doc Retrieves `jid:jid()' representation of the recipient of the stanza set using `new/6'.
%
% Returns `error' atom if recipient isn't set.
% @end
%-------------------------------------------------------------------
-spec get_to_jid(t()) -> getter_result(jid:jid()).
get_to_jid(Acc) ->
    maps:get(jid, get_prop(Acc, to, new)).

%-------------------------------------------------------------------
% @doc Retrieves binary representation of the recipient of the stanza set using `new/5,6'.
%
% Returns `error' atom if recipient isn't set.
% @end
%-------------------------------------------------------------------
-spec get_to_bin(t()) -> getter_result(binary()).
get_to_bin(Acc) ->
    case maps:find(to, Acc) of
        {ok, ElProps} ->
            {ok, maps:get(bin_jid, ElProps)};
        error ->
            error
    end.


%%%-------------------------------------------------------------------
%%% Internal "API"
%%%-------------------------------------------------------------------

-spec put_prop(t(), prop()) -> t().
put_prop(Acc, {PropKey, PropVal}) ->
    maps:put(PropKey, PropVal, Acc).

-spec get_prop(t(), atom(), atom()) -> term() | no_return().
get_prop(Acc, PropKey, Setter) ->
    case maps:find(PropKey, Acc) of
        {ok, PropVal} ->
            PropVal;
        error ->
            {Module, Function, Line} = maps:get(init_location, Acc),
            ?ERROR_MSG("Accumulator created in ~p:~p, line ~p, is missing property `~p`. "
                       "It needs to be set first using ~p:~p.",
                       [Module, Function, Line, PropKey, ?MODULE, Setter]),
            Reason = {acc_property_missing, [{acc, Acc},
                                             {property, PropKey},
                                             {setter, Setter}]},
            error(Reason)
    end.

-spec set_from(t(), jid:jid()) -> t().
set_from(Acc, #jid{} = Jid) ->
    JidProps = #{jid     => Jid,
                 bin_jid => jid:to_binary(Jid)},
    put_prop(Acc, {from, JidProps}).

-spec set_to(t(), jid:jid()) -> t().
set_to(Acc, #jid{} = Jid) ->
    JidProps = #{jid     => Jid,
                 bin_jid => jid:to_binary(Jid)},
    put_prop(Acc, {to, JidProps}).

