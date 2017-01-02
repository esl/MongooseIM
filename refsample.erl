%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Dec 2016 12:51
%%%-------------------------------------------------------------------
-module(refsample).
-author("bartek").

%% API
-export([]).

%%%% OLD IMPLEMENTATION %%%%

processing(El) ->
    processing_stage_one(El, 1, 2),
    processing_stage_two(El, 3),
    ok.

processing_stage_one(El, _Arg1, _Arg2) ->
    Res = ejabberd_hooks:run_fold(a_hook, [El]),
    do_something_with_result(Res),
    ok.

processing_stage_two(El, _Arg3) ->
    ejabberd_hooks:run(another_hook, [El]),
    ok.

something_with_result(Res) ->
    ok.

%%%% STAGE ONE - HOOKS REFACTORING %%%%

processing(El) ->
    processing_stage_one(El, 1, 2),
    processing_stage_two(El, 3),
    ok.

processing_stage_one(El, _Arg1, _Arg2) ->
    Acc = mongoose_stanza:new(#{element => El}),
    Acc2 = ejabberd_hooks:run_fold(a_hook, [Acc]),
    something_with_result(Acc2),
    ok.

processing_stage_two(El, _Arg3) ->
    Acc = mongoose_stanza:new(#{element => El}),
    ejabberd_hooks:run(another_hook, [Acc]),
    ok.

something_with_result(Acc) ->
    R = mongoose_stanza:get(sthg, Acc),
    Nr = do_something_with_result(R),
    mongoose_stanza:put(sthg, Nr, Acc).

do_something_with_result(Res) ->
    ok.

%%%% STAGE TWO - FOLLOW %%%%

processing(El) ->
    Acc = mongoose_stanza:new(#{element => El}),
    Acc2 = processing_stage_one(Acc, 1, 2),
    Acc3 = processing_stage_two(Acc2, 3),
    Acc3.

processing_stage_one(Acc, _Arg1, _Arg2) ->
    Acc1 = ejabberd_hooks:run_fold(a_hook, [Acc]),
    Acc2 = something_with_result(Acc1),
    Acc2.

processing_stage_two(Acc, _Arg3) ->
    Acc1 = ejabberd_hooks:run(another_hook, [Acc]),
    Acc1.

something_with_result(Acc) ->
    R = mongoose_stanza:get(sthg, Acc),
    Nr = do_something_with_result(R),
    mongoose_stanza:put(sthg, Nr, Acc).

do_something_with_result(Res) ->
    ok.

