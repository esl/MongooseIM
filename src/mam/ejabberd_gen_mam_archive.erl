-module(ejabberd_gen_mam_archive).

-callback archive_size(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: integer(),
    Params :: #{archive_id := mod_mam:archive_id() | undefined, owner => jid:jid(), room => jid:jid()},
    Extra :: gen_hook:extra().

-callback archive_message(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: ok | {error, term()},
    Params :: mod_mam:archive_message_params(),
    Extra :: gen_hook:extra().

-callback lookup_messages(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: {ok, mod_mam:lookup_result()} | {error, term()},
    Params :: mam_iq:lookup_params(),
    Extra :: gen_hook:extra().

-callback get_mam_pm_gdpr_data(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: mam_pm_gdpr_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().

-callback get_mam_muc_gdpr_data(Acc, Params, Extra) -> gen_hook:hook_fn_ret(Acc) when
    Acc :: mam_muc_gdpr_data(),
    Params :: #{jid := jid:jid()},
    Extra :: gen_hook:extra().

-optional_callbacks([get_mam_pm_gdpr_data/3,
                     get_mam_muc_gdpr_data/3,
                     archive_size/3,
                     lookup_messages/3]).

-type mam_pm_gdpr_data() :: [{MessageID :: bitstring(),
                              FromJID :: bitstring(),
                              Message :: bitstring()}].

-type mam_muc_gdpr_data() :: [{MessageID :: bitstring(),
                               Message :: bitstring()}].

-export_type([mam_pm_gdpr_data/0, mam_muc_gdpr_data/0]).
