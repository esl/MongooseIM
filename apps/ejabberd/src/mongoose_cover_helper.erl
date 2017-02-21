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
-module(mongoose_cover_helper).


%% API
-export([start/1, analyze/0]).

-spec start([atom()]) -> list().
start(Apps) ->
    cover:start(),
    lists:flatmap(fun cover_compile_app/1, Apps).

analyze() ->
    file:make_dir("priv/cover"),
    cover:export("priv/cover/" ++ atom_to_list(node()) ++ ".coverdata"),
    cover:stop().

cover_compile_app(App) ->
    do_cover_compile_app(App, code:lib_dir(App)).

do_cover_compile_app(App, {error, Error}) ->
    [{error, App, Error}];
do_cover_compile_app(App, AppPath) ->
    EbinDir = filename:join(AppPath, "ebin"),
    BeamFilter = fun
        %% modules not compatible with cover
        (File) when File =:= "bin_to_hex.beam"; File =:= "cover.beam" ->
            false;
        (File) ->
            case filename:extension(File) of
                ".beam" -> true;
                _ -> false
            end
    end,
    case file:list_dir(EbinDir) of
        {ok, Files} ->
            BeamFileNames = lists:filter(BeamFilter, Files),
            BeamFiles = [filename:join(EbinDir, File) || File <- BeamFileNames],
            [{App, compile_beams(BeamFiles, [])}];
        Error ->
            [{error, EbinDir, Error}]
    end.

compile_beams([], Result) ->
    Result;
compile_beams([File | Files], Result) ->
    case catch cover:compile_beam(File) of
        {ok, Module} ->
            compile_beams(Files, [Module | Result]);
        _E ->
            compile_beams(Files, Result)
    end.

