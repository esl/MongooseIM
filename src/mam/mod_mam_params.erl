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
         has_full_text_search/2, is_archivable_message_fun/2, archive_chat_markers/2,
         add_stanzaid_element/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec extra_params_module(mam_module(), Host :: ejabberd:lserver()) -> module() | undefined.
extra_params_module(Module, Host) ->
    param(Module, Host, extra_lookup_params, undefined).

-spec max_result_limit(mam_module(), Host :: ejabberd:lserver()) -> pos_integer().
max_result_limit(Module, Host) ->
    param(Module, Host, max_result_limit, 50).

-spec default_result_limit(mam_module(), Host :: ejabberd:lserver()) -> pos_integer().
default_result_limit(Module, Host) ->
    param(Module, Host, default_result_limit, 50).


-spec has_full_text_search(Module :: mod_mam | mod_mam_muc, Host :: ejabberd:server()) -> boolean().
has_full_text_search(Module, Host) ->
    param(Module, Host, full_text_search, true).

-spec is_archivable_message_fun(mam_module(), Host :: ejabberd:lserver()) ->
                                       MF :: {module(), atom()}.
is_archivable_message_fun(Module, Host) ->
    {IsArchivableModule, IsArchivableFunction} =
        case param(Module, Host, is_archivable_message, undefined) of
            undefined ->
                case param(Module, Host, is_complete_message, undefined) of
                    undefined -> {mod_mam_utils, is_archivable_message};
                    OldStyleMod -> {OldStyleMod, is_complete_message}
                end;

            Mod -> {Mod, is_archivable_message}
        end,
    {IsArchivableModule, IsArchivableFunction}.

-spec archive_chat_markers(mam_module(), Host :: ejabberd:lserver()) -> boolean().
archive_chat_markers(Module, Host) ->
    param(Module, Host, archive_chat_markers, false).

-spec add_stanzaid_element(mam_module(), Host :: ejabberd:lserver()) -> boolean().
add_stanzaid_element(Module, Host) ->
    not param(Module, Host, no_stanzaid_element, false).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec param(mam_module(), Host :: ejabberd:lserver(), Opt :: term(), Default :: term()) -> term().
param(Module, Host, Opt, Default) ->
    gen_mod:get_module_opt(Host, Module, Opt, Default).
