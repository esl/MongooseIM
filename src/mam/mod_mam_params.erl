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

-type mam_module() :: mod_mam | mod_mam_muc.

-export([extra_params_module/2, max_result_limit/2, default_result_limit/2,
         has_full_text_search/2, is_archivable_message_fun/2, send_message_mod/2,
         archive_chat_markers/2, add_stanzaid_element/2, extra_fin_element_module/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec extra_params_module(mam_module(), mongooseim:host_type()) -> module() | undefined.
extra_params_module(Module, HostType) ->
    param(Module, HostType, extra_lookup_params, undefined).

-spec max_result_limit(mam_module(), mongooseim:host_type()) -> pos_integer().
max_result_limit(Module, HostType) ->
    param(Module, HostType, max_result_limit, 50).

-spec default_result_limit(mam_module(), mongooseim:host_type()) -> pos_integer().
default_result_limit(Module, HostType) ->
    param(Module, HostType, default_result_limit, 50).


-spec has_full_text_search(Module :: mod_mam | mod_mam_muc, mongooseim:host_type()) -> boolean().
has_full_text_search(Module, HostType) ->
    param(Module, HostType, full_text_search, true).

-spec is_archivable_message_fun(mam_module(), mongooseim:host_type()) ->
                                       MF :: {module(), atom()}.
is_archivable_message_fun(Module, HostType) ->
    {IsArchivableModule, IsArchivableFunction} =
        case param(Module, HostType, is_archivable_message, undefined) of
            undefined ->
                case param(Module, HostType, is_complete_message, undefined) of
                    undefined -> {mod_mam_utils, is_archivable_message};
                    OldStyleMod -> {OldStyleMod, is_complete_message}
                end;

            Mod -> {Mod, is_archivable_message}
        end,
    {IsArchivableModule, IsArchivableFunction}.

-spec send_message_mod(mam_module(), mongooseim:host_type()) -> module().
send_message_mod(Module, HostType) ->
    param(Module, HostType, send_message, mod_mam_utils).

-spec archive_chat_markers(mam_module(), mongooseim:host_type()) -> boolean().
archive_chat_markers(Module, HostType) ->
    param(Module, HostType, archive_chat_markers, false).

-spec add_stanzaid_element(mam_module(), mongooseim:host_type()) -> boolean().
add_stanzaid_element(Module, HostType) ->
    not param(Module, HostType, no_stanzaid_element, false).

-spec extra_fin_element_module(mam_module(), mongooseim:host_type()) -> module() | undefined.
extra_fin_element_module(Module, HostType) ->
    param(Module, HostType, extra_fin_element, undefined).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec param(mam_module(), mongooseim:host_type(), Opt :: term(), Default :: term()) -> term().
param(Module, HostType, Opt, Default) ->
    gen_mod:get_module_opt(HostType, Module, Opt, Default).
