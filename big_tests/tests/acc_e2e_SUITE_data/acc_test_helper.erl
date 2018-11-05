-module(acc_test_helper).
-author("bartek").

-compile(export_all).

test_save_acc(#{ stanza := #{ type := <<"chat">>} } = Acc, _State) ->
    Rand = rand:uniform(),
    Acc1 = mongoose_acc:set_permanent(test, random_prop, Rand, Acc),
    Acc2 = mongoose_acc:set(test, should_be_stripped, 123, Acc1),
    Data = {mongoose_acc:ref(Acc2), mongoose_acc:timestamp(Acc2), Rand},
    ets:insert(test_message_index, Data),
    Acc2;
test_save_acc(Acc, _State) -> Acc.

test_check_acc({F, T, #{ stanza := #{ type := <<"chat">> } } = Acc, P}) ->
    try
        check_acc(Acc),
        {F, T, Acc, P}
    catch error:{badmatch, _} ->
        drop
    end;
test_check_acc(Arg) ->
    Arg.

test_check_final_acc(#{ stanza := #{ type := <<"chat">> } } = Acc, _Jid, _From, _To, _El) ->
    try
        check_acc(Acc, stripped),
        Acc
    catch error:{badmatch, _} ->
        drop
    end;
test_check_final_acc(Acc, _Jid, _From, _To, _El) ->
    Acc.

recreate_table() ->
    try ets:delete(test_message_index) catch _:_ -> ok end,
    ets:new(test_message_index, [named_table, public, {heir, whereis(ejabberd_c2s_sup), none}]).

check_acc(#{ stanza := #{ type := <<"chat">> } } = Acc) ->
    Ref = mongoose_acc:ref(Acc),
    [Data] = ets:lookup(test_message_index, Ref),
    Data = {Ref, mongoose_acc:timestamp(Acc), mongoose_acc:get(test, random_prop, Acc)};
check_acc(_Acc) -> ok.

check_acc(Acc, stripped) ->
    undefined = mongoose_acc:get(test, should_be_stripped, undefined, Acc),
    check_acc(Acc).

alter_message({From, To, Acc, Packet}) ->
    % Not using #xmlel as it causes some strange error in dynamic compilation
    {xmlel, PName, PAttrs, PCh} = Packet,
    NewBody = {xmlel, <<"body">>, [], [{xmlcdata, <<"bye">> }]},
    PCh2 = lists:keyreplace(<<"body">>, 2, PCh, NewBody),
    {From, To, Acc, {xmlel, PName, PAttrs, PCh2}}.

