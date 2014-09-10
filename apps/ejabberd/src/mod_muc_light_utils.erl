%%%----------------------------------------------------------------------
%%% File    : mod_muc_light_utils.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Utilities for mod_muc_light
%%% Created : 8 Sep 2014 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_light_utils).
-author('piotr.nosek@erlang-solutions.com').

%% API
-export([iq_to_config/2, sanitize_config/1, sanitize_config/2]).

-include("jlib.hrl").

%%====================================================================
%% API
%%====================================================================

iq_to_config(Packet, Defaults) ->
    Xs = exml_query:paths(Packet, [{element, <<"query">>}, {element, <<"x">>}]),
    process_form(find_form(Xs), Defaults).

sanitize_config(Config) ->
    sanitize_config(Config, false).

sanitize_config(Config, false) ->
    {SaneConfig, _} = sanitize_config(Config, [], [], config_schema()),
    SaneConfig;
sanitize_config(Config, true) ->
    sanitize_config(Config, [], [], config_schema()).

%%====================================================================
%% Internal functions
%%====================================================================

find_form([]) ->
    [];
find_form([XElem | XRest]) ->
    case exml_query:attr(XElem, <<"xmlns">>) of
        ?NS_XDATA -> exml_query:paths(XElem, [{element, <<"field">>}]);
        _ -> find_form(XRest)
    end.

process_form([], Config) ->
    sanitize_config(Config);
process_form([Field | Fields], Config) ->
    case exml_query:attr(Field, <<"var">>) of
        <<"FORM_TYPE">> ->
            process_form(Fields, Config);
        undefined ->
            process_form(Fields, Config);
        Var ->
            Key = list_to_existing_atom(binary_to_list(Var)),
            Value = exml_query:path(Field, [{element, <<"value">>}, cdata]),
            process_form(Fields, lists:keystore(Key, 1, Config, {Key, Value}))
    end.

config_schema() ->
    [
     {roomname, binary}
    ].

sanitize_config([], SaneConfig, Errors, _ConfigSchema) ->
    {SaneConfig, Errors};
sanitize_config([{ Key, Val } = Opt | RConfig], SaneConfig, Errors, ConfigSchema) ->
    case lists:keyfind(Key, 1, ConfigSchema) of
        false ->
            sanitize_config(RConfig, SaneConfig, [{Key, unknown} | Errors], ConfigSchema);
        {_, Type} ->
            case check_type(Val, Type) of
                true ->
                    sanitize_config(RConfig, [Opt | SaneConfig], Errors, ConfigSchema);
                false ->
                    sanitize_config(RConfig, SaneConfig, [{Key, type} | Errors], ConfigSchema)
            end
    end.

check_type(Val, binary) when is_binary(Val) -> true;
check_type(_Val, _Type) -> false.
