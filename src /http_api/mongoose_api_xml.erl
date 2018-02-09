%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(mongoose_api_xml).

-behaviour(mongoose_api_format).

-export([serialize/1, deserialize/1]).

-include("mongoose.hrl").
-include_lib("exml/include/exml.hrl").

%%--------------------------------------------------------------------
%% mongoose_api_format callbacks
%%--------------------------------------------------------------------
serialize(Data) ->
    do_serialize(Data).

deserialize(IOList) ->
    {ok, ParsedXML} = exml:parse(iolist_to_binary([IOList])),
    ParsedXML.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
do_serialize(Data) ->
    exml:to_iolist(prepare_xmlel(Data)).

prepare_xmlel(List) when is_list(List) ->
    prepare_xmlel({<<"list">>, List});
prepare_xmlel({ElementName, List}) when is_list(List) ->
    {Attrs, Children} = lists:partition(fun is_attribute/1, List),
    #xmlel{name = to_iolist_compliant(ElementName),
           attrs = [prepare_xmlel(Attr) || Attr <- Attrs],
           children = [prepare_xmlel(Child) || Child <- Children]};
prepare_xmlel({Key, Value}) ->
    {to_iolist_compliant(Key), to_iolist_compliant(Value)};
prepare_xmlel(Other) ->
    #xmlel{name = to_iolist_compliant(Other)}.

is_attribute({_, List}) when is_list(List) ->
    false;
is_attribute({_, _}) ->
    true;
is_attribute(_) ->
    false.

to_iolist_compliant(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
to_iolist_compliant(Int) when is_integer(Int) ->
    integer_to_binary(Int);
to_iolist_compliant(Other) ->
    Other.
