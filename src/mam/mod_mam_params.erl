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

-module(mod_mam_params).

-type mam_module() :: mod_mam_pm | mod_mam_muc.

-export([extra_params_module/2, max_result_limit/2, default_result_limit/2,
         has_full_text_search/2, is_archivable_message_module/2, send_message_mod/2,
         archive_chat_markers/2, add_stanzaid_element/2, extra_fin_element_module/2,
         enforce_simple_queries/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec extra_params_module(mam_module(), mongooseim:host_type()) -> module() | undefined.
extra_params_module(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, extra_lookup_params, undefined).

-spec max_result_limit(mam_module(), mongooseim:host_type()) -> pos_integer().
max_result_limit(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, max_result_limit).

-spec default_result_limit(mam_module(), mongooseim:host_type()) -> pos_integer().
default_result_limit(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, default_result_limit).

-spec enforce_simple_queries(mam_module(), mongooseim:host_type()) -> module() | undefined.
enforce_simple_queries(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, enforce_simple_queries, false).

-spec has_full_text_search(Module :: mod_mam_pm | mod_mam_muc, mongooseim:host_type()) -> boolean().
has_full_text_search(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, full_text_search).

-spec is_archivable_message_module(mam_module(), mongooseim:host_type()) -> module().
is_archivable_message_module(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, is_archivable_message).

-spec send_message_mod(mam_module(), mongooseim:host_type()) -> module().
send_message_mod(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, send_message).

-spec archive_chat_markers(mam_module(), mongooseim:host_type()) -> boolean().
archive_chat_markers(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, archive_chat_markers).

-spec add_stanzaid_element(mam_module(), mongooseim:host_type()) -> boolean().
add_stanzaid_element(Module, HostType) ->
    not gen_mod:get_module_opt(HostType, Module, no_stanzaid_element).

-spec extra_fin_element_module(mam_module(), mongooseim:host_type()) -> module() | undefined.
extra_fin_element_module(Module, HostType) ->
    gen_mod:get_module_opt(HostType, Module, extra_fin_element, undefined).
