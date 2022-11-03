-module(ejabberd_gen_mam_prefs).

-callback get_behaviour(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mod_mam:archive_behaviour(),
    Params :: map(),
    Extra :: map().

-callback set_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: term(),
    Params :: map(),
    Extra :: map().

-callback get_prefs(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mod_mam:preference() | {error, Reason :: term()},
    Params :: map(),
    Extra :: map().
