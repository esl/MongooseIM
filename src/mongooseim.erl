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
-module(mongooseim).

-type host_type() :: binary().
-type host_type_or_global() :: host_type() | global.
-type domain_name() :: jid:lserver().
-export_type([host_type/0, host_type_or_global/0, domain_name/0]).

%% API
-export([start/0, stop/0]).

-ignore_xref([start/0, stop/0]).

start() ->
    application:ensure_all_started(mongooseim).

stop() ->
    application:stop(mongooseim).
