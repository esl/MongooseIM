-module(ejabberd_gen_mam_prefs).

-type set_prefs_params() :: #{archive_id := undefined | mod_mam:archive_id(),
                              owner := jid:jid(),
                              default_mode := mod_mam:archive_behaviour(),
                              always_jids := [jid:literal_jid()],
                              never_jids := [jid:literal_jid()]}.

-type get_prefs_params() :: #{archive_id := undefined | mod_mam:archive_id(),
                              owner := jid:jid()}.

-type get_behaviour_params() :: #{archive_id := undefined | mod_mam:archive_id(),
                                  owner => jid:jid(),
                                  room => jid:jid(),
                                  remote := jid:jid()}.

-export_type([set_prefs_params/0, get_prefs_params/0, get_behaviour_params/0]).

-callback get_behaviour(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: mod_mam:archive_behaviour(),
    Params :: get_behaviour_params(),
    Extra :: map().

-callback set_prefs(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: term(),
    Params :: set_prefs_params(),
    Extra :: map().

-callback get_prefs(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: mod_mam:preference() | {error, Reason :: term()},
    Params :: get_prefs_params(),
    Extra :: map().
