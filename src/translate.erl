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

-module(translate).
-author('alexey@process-one.net').

-export([start/0,
         translate/2]).

-include("mongoose.hrl").

-define(MSGS_DIR,    "msgs").

%%
%% Public
%%

-spec start() -> 'ok'.
start() ->
    ets:new(translations, [named_table, public]),
    ok = load_translations_from_dir(lang_files_directory()),
    ok.

-spec translate(ejabberd:lang(), binary()) -> binary().
translate(Lang, Msg) ->
    LLang = to_lower(Lang),
    case get_translation(LLang, Msg) of
        {ok, Trans} -> Trans;
        {error, not_found} -> get_default_server_lang_translation(Msg)
    end.

%%
%% Private
%%

-spec lang_files_directory() ->  file:filename().
lang_files_directory() ->
    case os:getenv("EJABBERD_MSGS_PATH") of
        false ->
            case code:priv_dir(mongooseim) of
                {error, _} -> ?MSGS_DIR;
                Path -> Path
            end;
        Path -> Path
    end.

-spec load_translations_from_dir(file:filename()) -> ok | {error, lager_not_running}.
load_translations_from_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            MsgFiles = lists:filter(fun has_msg_extension/1, Files),
            load_translation_files(Dir, MsgFiles);
        {error, Reason} ->
            ?ERROR_MSG("~p", [Reason])
    end.

-spec load_translation_files(file:filename(), [file:filename()]) -> ok.
load_translation_files(Dir, MsgFiles) ->
    lists:foreach(fun(Filename) ->
                          Lang = lang_from_file_name(Filename),
                          load_file(Lang, Dir ++ "/" ++ Filename)
                  end, MsgFiles).

-spec lang_from_file_name(file:filename()) -> string().
lang_from_file_name(Filename) ->
    string:to_lower(filename:rootname(Filename)).

-spec has_msg_extension(file:filename()) -> boolean().
has_msg_extension(FileName) ->
    filename:extension(FileName) == ".msg".

-spec load_file(string(), file:name()) -> 'ok'.
load_file(Lang, File) ->
    BLang = list_to_binary(Lang),
    case file:consult(File) of
        {ok, Terms} ->
            lists:foreach(fun({Orig, Trans}) ->
                                  insert_translation(BLang,
                                                     unicode:characters_to_binary(Orig),
                                                     unicode:characters_to_binary(Trans))
                          end, Terms);
        %% Code copied from ejabberd_config.erl
        {error, {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
            ExitText = lists:flatten(File ++ " approximately in the line "
                                     ++ file:format_error(Reason)),
            ?ERROR_MSG("Problem loading translation file ~n~s", [ExitText]),
            exit(ExitText);
        {error, Reason} ->
            ExitText = lists:flatten(File ++ ": " ++ file:format_error(Reason)),
            ?ERROR_MSG("Problem loading translation file ~n~s", [ExitText]),
            exit(ExitText)
    end.

-spec insert_translation(ejabberd:lang(), binary(), binary()) -> true.
insert_translation(Lang, Msg, <<"">>) ->
    insert_translation(Lang, Msg, Msg); %% use key if it is not defined
insert_translation(Lang, Msg, Trans) ->
    ets:insert(translations, {{Lang, Msg}, Trans}).

-spec get_default_server_lang_translation(binary()) ->  binary().
get_default_server_lang_translation(Msg) ->
    case get_translation(default_server_lang(), Msg) of
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
    case ets:lookup(translations, {LLang, Msg}) of
        [{_, Trans}] -> {ok, Trans};
        _ -> {error, not_found}
    end.

-spec short_lang(ejabberd:lang()) -> ejabberd:lang().
short_lang(LLang) ->
    case string:tokens(binary_to_list(LLang), "-") of
        [] -> LLang;
        [ShortLang | _] -> list_to_binary(ShortLang)
    end.

-spec default_server_lang() -> ejabberd:lang().
default_server_lang() ->
    case ?MYLANG of
        undefined -> <<"en">>;
        <<"en">> ->  <<"en">>;
        Lang -> Lang
    end.

-spec to_lower(binary()) -> binary().
to_lower(Bin) ->
    list_to_binary(string:to_lower(binary_to_list(Bin))).
