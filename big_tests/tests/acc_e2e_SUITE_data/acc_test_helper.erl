-module(acc_test_helper).
-author("bartek").

-compile([export_all, nowarn_export_all]).

-spec test_save_acc(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: map().
test_save_acc(#{ stanza := #{ type := <<"chat">>} } = Acc, _, _) ->
    Rand = rand:uniform(),
    Acc1 = mongoose_acc:set_permanent(test, random_prop, Rand, Acc),
    Acc2 = mongoose_acc:set(test, should_be_stripped, 123, Acc1),
    Data = {mongoose_acc:ref(Acc2), mongoose_acc:timestamp(Acc2), Rand},
    ets:insert(test_message_index, Data),
    {ok, Acc2};
test_save_acc(Acc, _, _) -> {ok, Acc}.

-spec test_check_acc(Acc, Params, Extra) -> {ok, Acc} | {stop, drop} when
    Acc :: mongoose_hooks:filter_packet_acc(),
    Params :: map(),
    Extra :: map().
test_check_acc({F, T, #{ stanza := #{ type := <<"chat">> } } = Acc, P}, _, _) ->
    try
        check_acc(Acc),
        {ok, {F, T, Acc, P}}
    catch error:{badmatch, _} ->
        {stop, drop}
    end;
test_check_acc(Acc, _, _) ->
    {ok, Acc}.

-spec test_check_final_acc(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_hooks:filter_packet_acc() | drop,
    Params :: map(),
    Extra :: map().
test_check_final_acc(#{ stanza := #{ type := <<"chat">> } } = Acc, _, _) ->
    NewAcc = try
        check_acc(Acc, stripped),
        Acc
    catch error:{badmatch, _} ->
        drop
    end,
    {ok, NewAcc};
test_check_final_acc(Acc, _, _) ->
    {ok, Acc}.

-spec save_my_jid(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: map(),
    Extra :: map().
save_my_jid(#{ stanza := #{ type := <<"chat">>} } = Acc, _, _) ->
    Me = mongoose_acc:get(c2s, origin_jid, Acc),
    {_, Acc2} = cached_my_jid(Me, Acc),
    {ok, Acc2};
save_my_jid(Acc, _, _) -> {ok, Acc}.

-spec drop_if_jid_not_mine(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_hooks:filter_packet_acc() | drop,
    Params :: map(),
    Extra :: map().
drop_if_jid_not_mine({F, T, #{ stanza := #{ type := <<"chat">> } } = Acc, P}, _, _) ->
    %% since we are in filter_local_packet, means we are just about to deliver the message
    %% sender-side processing is already completed and now we want the other guy values
    NewAcc = case cached_my_jid(T, Acc) of
        {T, Acc2} ->
            {F, T, Acc2, P};
        _ ->
            drop
    end,
    {ok, NewAcc};
drop_if_jid_not_mine(Acc, _, _) ->
    {ok, Acc}.

recreate_table() ->
    try ets:delete(test_message_index) catch _:_ -> ok end,
    ets:new(test_message_index, [named_table, public, {heir, whereis(mongoose_c2s_sup), none}]).

check_acc(#{ stanza := #{ type := <<"chat">> } } = Acc) ->
    Ref = mongoose_acc:ref(Acc),
    [Data] = ets:lookup(test_message_index, Ref),
    Data = {Ref, mongoose_acc:timestamp(Acc), mongoose_acc:get(test, random_prop, Acc)};
check_acc(_Acc) -> ok.

check_acc(Acc, stripped) ->
    undefined = mongoose_acc:get(test, should_be_stripped, undefined, Acc),
    check_acc(Acc).

-spec alter_message(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_hooks:filter_packet_acc(),
    Params :: map(),
    Extra :: map().
alter_message({From, To, Acc, Packet}, _, _) ->
    % Not using #xmlel as it causes some strange error in dynamic compilation
    {xmlel, PName, PAttrs, PCh} = Packet,
    NewBody = {xmlel, <<"body">>, [], [{xmlcdata, <<"bye">> }]},
    PCh2 = lists:keyreplace(<<"body">>, 2, PCh, NewBody),
    NewAcc = {From, To, Acc, {xmlel, PName, PAttrs, PCh2}},
    {ok, NewAcc}.

cached_my_jid(User, Acc) ->
    case mongoose_acc:get(test, my_jid, undefined, Acc) of
        undefined ->
            {User, mongoose_acc:set(test, my_jid, User, Acc)};
        Jid ->
            {Jid, Acc}
    end.
