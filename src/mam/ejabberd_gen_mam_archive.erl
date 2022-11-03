-module(ejabberd_gen_mam_archive).

-callback archive_size(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: integer(),
    Params :: map(),
    Extra :: map().

-callback archive_message(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: ok | {error, term()},
    Params :: map(),
    Extra :: map().

-callback lookup_messages(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: {ok, mod_mam:lookup_result()} | {error, term()},
    Params :: map(),
    Extra :: map().

-callback get_mam_pm_gdpr_data(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mam_pm_gdpr_data(),
    Params :: map(),
    Extra :: map().

-callback get_mam_muc_gdpr_data(Acc, Params, Extra) -> {ok | stop, Acc} when
    Acc :: mam_muc_gdpr_data(),
    Params :: map(),
    Extra :: map().

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
