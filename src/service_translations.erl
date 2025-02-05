%%%----------------------------------------------------------------------
%%% File    : translate.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Localization helper
%%% Created :  6 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(service_translations).
-author('alexey@process-one.net').

-behaviour(mongoose_service).
-behaviour(mongoose_module_metrics).

-export([start/1, stop/0, config_spec/0]).
-export([do/2]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-define(TABLE, translations).

-spec start(mongoose_service:options()) -> ok.
start(_) ->
    ets:new(?TABLE, [named_table, public, set, {read_concurrency, true}]),
    ok = load_translations_from_dir(lang_files_directory()).

-spec stop() -> any().
stop() ->
    ets:delete(?TABLE).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{}.

-spec do(ejabberd:lang(), binary()) -> binary().
do(<<"en">>, Msg) ->
    Msg; %% Exit early, in most cases no translation is required
do(Lang, Msg) ->
    case mongoose_service:is_loaded(?MODULE) of
        false ->
            Msg;
        true ->
            do_(Lang, Msg)
    end.

%% Private

-spec do_(ejabberd:lang(), binary()) -> binary().
do_(Lang, Msg) ->
    LLang = to_lower(Lang),
    case get_translation(LLang, Msg) of
        {ok, Trans} -> Trans;
        {error, not_found} -> get_default_server_lang_translation(Msg)
    end.

-spec lang_files_directory() -> file:filename().
lang_files_directory() ->
    case os:getenv("EJABBERD_MSGS_PATH") of
        false ->
            case code:priv_dir(mongooseim) of
                {error, _} ->
                    exit(<<"No directory with message translations available">>);
                Path ->
                    Path ++ "/translations"
            end;
        Path -> Path
    end.

-spec load_translations_from_dir(file:filename()) -> ok.
load_translations_from_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            MsgFiles = lists:filter(fun has_msg_extension/1, Files),
            load_translation_files(Dir, MsgFiles);
        {error, Reason} ->
            ?LOG_ERROR(#{what => load_translations_from_dir_failed,
                         directory => Dir, reason => Reason}),
            ok
    end.

-spec load_translation_files(file:filename(), [file:filename()]) -> ok.
load_translation_files(Dir, MsgFiles) ->
    lists:foreach(fun(Filename) ->
                          Lang = lang_from_file_name(Filename),
                          load_file(Lang, Dir ++ "/" ++ Filename)
                  end, MsgFiles).

-spec lang_from_file_name(file:filename()) -> string().
lang_from_file_name(Filename) ->
    string:lowercase(filename:rootname(Filename)).

-spec has_msg_extension(file:filename()) -> boolean().
has_msg_extension(FileName) ->
    filename:extension(FileName) == ".msg".

-spec load_file(string(), file:name()) -> ok.
load_file(Lang, File) ->
    BLang = list_to_binary(Lang),
    case file:consult(File) of
        {ok, Terms} ->
            lists:foreach(fun({Orig, Trans}) ->
                                  insert_translation(BLang,
                                                     unicode:characters_to_binary(Orig),
                                                     unicode:characters_to_binary(Trans))
                          end, Terms);
        {error, Reason} ->
            ExitText = iolist_to_binary(File ++ ": " ++ file:format_error(Reason)),
            ?LOG_ERROR(#{what => load_translation_file_failed,
                         text => <<"Problem loading translation file">>,
                         filename => File, reason => ExitText}),
            exit(ExitText)
    end.

-spec insert_translation(ejabberd:lang(), binary(), binary()) -> true.
insert_translation(Lang, Msg, <<>>) ->
    insert_translation(Lang, Msg, Msg); %% use key if it is not defined
insert_translation(Lang, Msg, Trans) ->
    ets:insert(?TABLE, {{Lang, Msg}, Trans}).

-spec get_default_server_lang_translation(binary()) ->  binary().
get_default_server_lang_translation(Msg) ->
    case get_translation(?MYLANG, Msg) of
        {ok, DefaultTrans} -> DefaultTrans;
        {error, not_found} -> Msg
    end.

-spec get_translation(ejabberd:lang(), binary()) -> {ok, binary()} | {error, not_found}.
get_translation(LLang, Msg) ->
    case read_trans(LLang, Msg) of
        {error, not_found} ->
            read_trans(short_lang(LLang), Msg);
        {ok, Trans} ->
            {ok, Trans}
    end.

-spec read_trans(ejabberd:lang(), binary()) -> {ok, binary()} | {error, not_found}.
read_trans(<<"en">>, Msg) ->
    {ok, Msg};
read_trans(LLang, Msg) ->
    case ets:lookup_element(?TABLE, {LLang, Msg}, 2, {error, not_found}) of
        {error, not_found} -> {error, not_found};
        Trans when is_binary(Trans) -> {ok, Trans}
    end.

-spec short_lang(ejabberd:lang()) -> ejabberd:lang().
short_lang(LLang) ->
    case string:tokens(binary_to_list(LLang), "-") of
        [] -> LLang;
        [ShortLang | _] -> list_to_binary(ShortLang)
    end.

-spec to_lower(binary()) -> binary().
to_lower(Bin) when is_binary(Bin) ->
    string:lowercase(Bin).
