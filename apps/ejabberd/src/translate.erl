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
    Dir = case code:priv_dir(ejabberd) of
              {error, _} ->
                  ?MSGS_DIR;
              Path ->
                  Path
          end,
    load_dir(Dir).

-spec load_dir(file:name()) -> any().
load_dir(Dir) ->
    MsgFiles = filelib:wildcard("*.msg", Dir),
            lists:foreach(
              fun(FN) ->
                      [Lang, _] = string:tokens(FN, "."),
                      LLang = string:to_lower(Lang),
                      load_file(list_to_binary(LLang), filename:join(Dir, FN))
              end, MsgFiles).

-spec load_file(ejabberd:lang(), file:name()) -> 'ok'.
load_file(Lang, File) ->
    case file:consult(File) of
        {ok, Terms} ->
            lists:foreach(
              fun({OrigStr, TransStr0}) ->
                      TransStr = case TransStr0 of
                                     "" -> OrigStr;
                                     _ -> TransStr0
                                 end,
                      Trans = unicode:characters_to_binary(TransStr),
                      ets:insert(translations, {{Lang, list_to_binary(OrigStr)}, Trans})
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


-spec translate(ejabberd:lang(), binary()) -> binary().
translate(<<"en">>, Msg) ->
    Msg;
translate(Lang, Msg) ->
    case normalise_and_split(Lang) of
        {LLang, <<>>} -> % LLang is actually a base language
            attempt_translation([LLang, mylang], Msg);
        {LLang, LBaseLang} ->
            attempt_translation([LLang, LBaseLang, mylang], Msg)
    end.

-spec attempt_translation(Langs :: [binary()], Msg :: binary()) -> binary().
attempt_translation([], Msg) ->
    Msg;
attempt_translation([mylang | Langs], Msg) ->
    case ?MYLANG of
        undefined ->
            attempt_translation(Langs, Msg);
        MyLang ->
            case string:tokens(MyLang, "-") of
                [_] ->
                    attempt_translation([list_to_binary(MyLang) | Langs], Msg);
                [MyLangBase | _] ->
                    attempt_translation(
                      [list_to_binary(MyLang), list_to_binary(MyLangBase) | Langs], Msg)
            end
    end;
attempt_translation([<<"en">> | _], Msg) ->
    Msg;
attempt_translation([Lang | Langs], Msg) ->
    case ets:lookup(translations, {Lang, Msg}) of
        [{_, Trans}] -> Trans;
        _ -> attempt_translation(Langs, Msg)
    end.

-spec normalise_and_split(binary()) -> {Lower :: binary(), LowerBase :: binary()}.
normalise_and_split(Bin) ->
    normalise_and_split(Bin, <<>>, <<>>).

-spec normalise_and_split(Bin :: binary(), LowerAcc :: binary(), LowerBaseAcc :: binary()) ->
    {Lower :: binary(), LowerBase :: binary()}.
normalise_and_split(<<$-, _/binary>> = Cs, LowerAcc, <<>>) ->
    normalise_and_split(Cs, LowerAcc, LowerAcc);
normalise_and_split(<<C, Cs/binary>>, LowerAcc, LowerBaseAcc) when C >= $A, C =< $Z ->
    normalise_and_split(Cs, <<LowerAcc/binary, (C + ($a - $A))>>, LowerBaseAcc);
normalise_and_split(<<C, Cs/binary>>, LowerAcc, LowerBaseAcc) ->
    normalise_and_split(Cs, <<LowerAcc/binary, C>>, LowerBaseAcc);
normalise_and_split(<<>>, LowerAcc, LowerBaseAcc) ->
    {LowerAcc, LowerBaseAcc}.

