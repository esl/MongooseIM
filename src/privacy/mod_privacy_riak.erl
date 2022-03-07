%%==============================================================================
%% Copyright 2015 Guillaume Bour.
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

%
% NOTES:
%   user default privacy is stored in
%       bucket {<<"privacy_defaults">>, LServer}, key LUser@LServer
%   user privacy lists names are stored in
%       set {<<"privacy_lists_names">>, <<"LServer">>}, key LUser@LServer
%   user privacy lists content are stored in
%       bucket {<<"privacy_lists">>, LServer}, key LUser@LServer/ListName

-module(mod_privacy_riak).
-author('guillaume@bour.cc').
-behaviour(mod_privacy_backend).

-export([init/2,
         get_default_list/3,
         get_list_names/3,
         get_privacy_list/4,
         set_default_list/4,
         forget_default_list/3,
         remove_privacy_list/4,
         replace_privacy_list/5,
         remove_user/3,
         remove_domain/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-include_lib("riakc/include/riakc.hrl").

get_bucket_name(HostType, Opt) ->
    {gen_mod:get_module_opt(HostType, mod_privacy, [riak, Opt]), HostType}.

-define(BKT_DEFAULT_LIST(HostType),
        get_bucket_name(HostType, defaults_bucket_type)).
-define(BKT_LISTS_NAMES(HostType),
        get_bucket_name(HostType, names_bucket_type)).
-define(BKT_LISTS(HostType),
        get_bucket_name(HostType, bucket_type)).

init(_HostType, _Opts) ->
    ok.

get_default_list(HostType, LUser, LServer) ->
    case get_default_list_name(HostType, LUser, LServer) of
        none ->
            {error, not_found};
        Default ->
            case get_privacy_list(HostType, LUser, LServer, Default) of
                {ok, List} ->
                    {ok, {Default, List}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_list_names(HostType, LUser, LServer) ->
    Default = get_default_list_name(HostType, LUser, LServer),
    Names = get_list_names_only(HostType, LUser, LServer),
    {ok, {Default, Names}}.

-spec get_default_list_name(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary() | none.
get_default_list_name(HostType, LUser, LServer) ->
    case mongoose_riak:get(?BKT_DEFAULT_LIST(HostType), key(LUser, LServer)) of
        {ok, Obj} ->
            riakc_obj:get_value(Obj);
        _ -> none
    end.

-spec get_list_names_only(mongooseim:host_type(), jid:luser(), jid:lserver()) -> list(binary()).
get_list_names_only(HostType, LUser, LServer) ->
    case mongoose_riak:fetch_type(?BKT_LISTS_NAMES(HostType), key(LUser, LServer)) of
        {ok, Set} ->
            riakc_set:value(Set);
        {error, {notfound, set}} ->
            [];
        Err ->
            ?LOG_ERROR(#{what => privacy_get_list_names_only_failed,
                         user => LUser, server => LServer, reason => Err}),
            []
    end.

get_privacy_list(HostType, LUser, LServer, Name) ->
    case mongoose_riak:get(?BKT_LISTS(HostType), key(LUser, LServer, Name)) of
        {ok, Obj} ->
            Val = binary_to_term(riakc_obj:get_value(Obj)),
            {ok, Val};
        {error, notfound} ->
            {error, not_found};
        Err ->
            ?LOG_ERROR(#{what => privacy_get_list_names_only_failed,
                         user => LUser, server => LServer, list_name => Name,
                         reason => Err}),
            Err
    end.

forget_default_list(HostType, LUser, LServer) ->
    mongoose_riak:delete(?BKT_DEFAULT_LIST(HostType), key(LUser, LServer)).

set_default_list(HostType, LUser, LServer, Name) ->
    case mongoose_riak:get(?BKT_LISTS(HostType), key(LUser, LServer, Name)) of
        {ok, _} ->
            % create or update
            Obj = riakc_obj:new(?BKT_DEFAULT_LIST(HostType), key(LUser, LServer), Name),
            mongoose_riak:put(Obj);
        % in case list name is not found
        {error, notfound} ->
            {error, not_found};
        Err -> Err
    end.

remove_privacy_list(HostType, LUser, LServer, Name) ->
    mongoose_riak:delete(?BKT_LISTS(HostType), key(LUser, LServer, Name)),

    case mongoose_riak:fetch_type(?BKT_LISTS_NAMES(HostType), key(LUser, LServer)) of
        {ok, S1} ->
            S2 = riakc_set:del_element(Name, S1),
            mongoose_riak:update_type(?BKT_LISTS_NAMES(HostType), key(LUser, LServer), riakc_set:to_op(S2));
        Err ->
            ?LOG_ERROR(#{what => privacy_remove_privacy_list_failed,
                         user => LUser, server => LServer, list_name => Name,
                         reason => Err}),
            Err
    end.

replace_privacy_list(HostType, LUser, LServer, Name, List) ->
    % store privacy-list content
    BinaryList = term_to_binary(List),
    Obj = riakc_obj:new(?BKT_LISTS(HostType), key(LUser, LServer, Name), BinaryList),
    mongoose_riak:put(Obj),

    % add new list name to user privacy-lists set
    S = riakc_set:add_element(Name, riakc_set:new()),
    mongoose_riak:update_type(?BKT_LISTS_NAMES(HostType), key(LUser, LServer), riakc_set:to_op(S)).

remove_user(HostType, LUser, LServer) ->
    forget_default_list(HostType, LUser, LServer),
    lists:foreach(
        fun(Name) -> remove_privacy_list(HostType, LUser, LServer, Name) end,
        get_list_names_only(HostType, LUser, LServer)
    ),
    mongoose_riak:delete(?BKT_LISTS_NAMES(HostType), key(LUser, LServer)).

%% TODO
remove_domain(_HostType, _LServer) ->
    ok.
    % forget_default_list(HostType, <<>>, LServer),
    % lists:foreach(
    %     fun(Name) -> remove_privacy_list(HostType, <<>>, LServer, Name) end,
    %     get_list_names_only(HostType, <<>>, LServer)
    % ),
    % mongoose_riak:delete(?BKT_LISTS_NAMES(HostType), key(<<>>, LServer)).

key(LUser, LServer) ->
    jid:to_binary({LUser, LServer}).
key(LUser, LServer, Name) ->
    jid:to_binary({LUser, LServer, Name}).
