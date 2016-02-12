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
%   user default privacy is stored in bucket {<<"privacy_defaults">>, LServer}, key LUser
%   user privacy lists names are stored in set {<<"privacy_lists_names">>, <<"LServer">>}, key LUser
%   user privace lists content are stored in bucket {<<"privacy_lists">>, LServer}, key <<LUser,$/,ListName>>

-module(mod_privacy_riak).
-author('guillaume@bour.cc').
-behaviour(mod_privacy).

-export([init/2,
         get_default_list/2,
         get_list_names/2,
         get_privacy_list/3,
         forget_default_list/2,
         set_default_list/3,
         remove_privacy_list/3,
         replace_privacy_list/4,
         remove_user/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_privacy.hrl").

-include_lib("riakc/include/riakc.hrl").

-define(BKT_DEFAULT_LIST(Server), {<<"privacy_defaults">>, Server}).
-define(BKT_LISTS_NAMES(Server), {<<"privacy_lists_names">>, Server}).
-define(BKT_LISTS(Server), {<<"privacy_lists">>, Server}).

init(_Host, _Opts) ->
    ok.

get_default_list(LUser, LServer) ->
    case get_default_list_name(LUser, LServer) of
        none ->
            {error, not_found};
        Default ->
            case get_privacy_list(LUser, LServer, Default) of
                {ok, List} ->
                    {ok, {Default, List}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

get_list_names(LUser, LServer) ->
    Default = get_default_list_name(LUser, LServer),
    Names = get_list_names_only(LUser, LServer),
    {ok, {Default, Names}}.

-spec get_default_list_name(ejabberd:luser(), ejabberd:lserver()) -> binary() | none.
get_default_list_name(LUser, LServer) ->
    case mongoose_riak:get(?BKT_DEFAULT_LIST(LServer), LUser) of
        {ok, Obj} ->
            riakc_obj:get_value(Obj);
        _ -> none
    end.

-spec get_list_names_only(ejabberd:luser(), ejabberd:lserver()) -> list(binary()).
get_list_names_only(LUser, LServer) ->
    case mongoose_riak:fetch_type(?BKT_LISTS_NAMES(LServer), LUser) of
        {ok, Set} ->
            riakc_set:value(Set);
        {error, {notfound, set}} ->
            [];
        Err ->
            ?ERROR_MSG("~p", [Err]),
            []
    end.

get_privacy_list(LUser, LServer, Name) ->
    case mongoose_riak:get(?BKT_LISTS(LServer), <<LUser/binary, $/, Name/binary>>) of
        {ok, Obj} ->
            Val = binary_to_term(riakc_obj:get_value(Obj)),
            {ok, Val};
        {error, notfound} ->
            {error, not_found};
        Err->
            ?ERROR_MSG("~p", [Err]),
            Err
    end.

forget_default_list(LUser, LServer) ->
    mongoose_riak:delete(?BKT_DEFAULT_LIST(LServer), LUser).

set_default_list(LUser, LServer, Name) ->
    case mongoose_riak:get(?BKT_LISTS(LServer), <<LUser/binary, $/, Name/binary>>) of
        {ok, _} ->
            % create or update
            Obj = riakc_obj:new(?BKT_DEFAULT_LIST(LServer), LUser, Name),
            mongoose_riak:put(Obj);
        % in case list name is not found
        {error, notfound} ->
            {error, not_found};
        Err -> Err
    end.

remove_privacy_list(LUser, LServer, Name) ->
    mongoose_riak:delete(?BKT_LISTS(LServer), <<LUser/binary, $/, Name/binary>>),

    case mongoose_riak:fetch_type(?BKT_LISTS_NAMES(LServer), LUser) of
        {ok, S1} ->
            S2 = riakc_set:del_element(Name, S1),
            mongoose_riak:update_type(?BKT_LISTS_NAMES(LServer), LUser, riakc_set:to_op(S2));
        Err ->
            ?ERROR_MSG("~p", [Err]),
            Err
    end.

replace_privacy_list(LUser, LServer, Name, List) ->
    % store privacy-list content
    BinaryList = term_to_binary(List),
    Obj = riakc_obj:new(?BKT_LISTS(LServer), <<LUser/binary, $/, Name/binary>>, BinaryList),
    mongoose_riak:put(Obj),

    % add new list name to user privacy-lists set
    S = riakc_set:add_element(Name, riakc_set:new()),
    mongoose_riak:update_type(?BKT_LISTS_NAMES(LServer), LUser, riakc_set:to_op(S)).

remove_user(LUser, LServer) ->
    forget_default_list(LUser, LServer),

    lists:foreach(
        fun(Name) -> remove_privacy_list(LUser, LServer, Name) end,
        get_list_names_only(LUser, LServer)
    ),

    % delete user privacy_lists set
    mongoose_riak:delete(?BKT_LISTS_NAMES(LServer), LUser).


