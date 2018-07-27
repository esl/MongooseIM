%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
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

-module(mongoose_rdbms_pgsql_codec_boolean).
-author('konrad.zemek@erlang-solutions.com').
-behaviour(epgsql_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-export_type([data/0]).

-type data() :: boolean() | 0 | 1.

init(_, _) -> [].

names() ->
    [bool].

encode(true, bool, State) -> encode(1, bool, State);
encode(false, bool, State) -> encode(0, bool, State);
encode(1, bool, _) -> <<1:1/big-signed-unit:8>>;
encode(0, bool, _) -> <<0:1/big-signed-unit:8>>.

decode(<<1:1/big-signed-unit:8>>, bool, _) -> true;
decode(<<0:1/big-signed-unit:8>>, bool, _) -> false.

decode_text(V, _, _) -> V.
