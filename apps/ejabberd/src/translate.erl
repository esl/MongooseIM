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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(translate).
-author('alexey@process-one.net').

-export([start/0,
         load_dir/1,
         load_file/2,
         translate/2]).

-include("ejabberd.hrl").

-spec start() -> 'ok'.
start() ->
    ets:new(translations, [named_table, public]),
    Dir =
        case os:getenv("EJABBERD_MSGS_PATH") of
            false ->
                case code:priv_dir(ejabberd) of
                    {error, _} ->
                        ?MSGS_DIR;
                    Path ->
                        Path
                end;
            Path ->
                Path
        end,
    load_dir(Dir),
    ok.


-spec load_dir(file:name()) -> 'ok' | {'error','lager_not_running'}.
load_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            MsgFiles = lists:filter(
                         fun(FN) ->
                                 case string:len(FN) > 4 of
                                     true ->
                                         string:substr(
                                           FN,
                                           string:len(FN) - 3) == ".msg";
                                     _ ->
                                         false
                                 end
                         end, Files),
            lists:foreach(
              fun(FN) ->
                      LP = ascii_tolower(
                             string:substr(FN, 1, string:len(FN) - 4)),
                      L = case string:tokens(LP, ".") of
                              [Language] -> Language;
                              [Language, _Project] -> Language
                          end,
                      load_file(L, Dir ++ "/" ++ FN)
              end, MsgFiles),
            ok;
        {error, Reason} ->
            ?ERROR_MSG("~p", [Reason])
    end.


-spec load_file(ejabberd:lang(), file:name()) -> 'ok'.
load_file(Lang, File) ->
    case file:consult(File) of
        {ok, Terms} ->
            lists:foreach(fun({Orig, Trans}) ->
                                  Trans1 = case Trans of
                                               "" ->
                                                   Orig;
                                               _ ->
                                                   Trans
                                           end,
                                  ets:insert(translations,
                                             {{Lang, Orig}, Trans1})
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


-spec translate(ejabberd:lang(), string()) -> string().
translate(Lang, Msg) when is_binary(Lang) ->
    translate(binary:bin_to_list(Lang), Msg);
translate(Lang, Msg) ->
    LLang = ascii_tolower(Lang),
    case ets:lookup(translations, {LLang, Msg}) of
        [{_, Trans}] ->
            Trans;
        _ ->
            ShortLang = case string:tokens(LLang, "-") of
                            [] ->
                                LLang;
                            [SL | _] ->
                                SL
                        end,
            case ShortLang of
                "en" ->
                    Msg;
                LLang ->
                    translate(Msg);
                _ ->
                    case ets:lookup(translations, {ShortLang, Msg}) of
                        [{_, Trans}] ->
                            Trans;
                        _ ->
                            translate(Msg)
                    end
            end
    end.


-spec translate(string()) -> string().
translate(Msg) ->
    case ?MYLANG of
        undefined ->
            Msg;
        "en" ->
            Msg;
        Lang ->
            LLang = ascii_tolower(Lang),
            case ets:lookup(translations, {LLang, Msg}) of
                [{_, Trans}] ->
                    Trans;
                _ ->
                    ShortLang = case string:tokens(LLang, "-") of
                                    [] ->
                                        LLang;
                                    [SL | _] ->
                                        SL
                                end,
                    case ShortLang of
                        "en" ->
                            Msg;
                        Lang ->
                            Msg;
                        _ ->
                            case ets:lookup(translations, {ShortLang, Msg}) of
                                [{_, Trans}] ->
                                    Trans;
                                _ ->
                                    Msg
                            end
                    end
            end
    end.


-spec ascii_tolower(string()) -> [string()].
ascii_tolower([C | Cs]) when C >= $A, C =< $Z ->
    [C + ($a - $A) | ascii_tolower(Cs)];
ascii_tolower([C | Cs]) ->
    [C | ascii_tolower(Cs)];
ascii_tolower([]) ->
    [].

