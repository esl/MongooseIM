-module(ejabberd_gen_mam_archive).

-ignore_xref([behaviour_info/1]).

-callback archive_size(Size :: integer(), Host :: jid:server(),
                       ArchiveID :: mod_mam_pm:archive_id(), ArchiveJID :: jid:jid())
                      -> integer().

-callback archive_message(Result :: any(), Host :: jid:server(),
                          Params :: mod_mam_pm:archive_message_params()) -> ok | {error, timeout}.

-callback lookup_messages(Result :: any(), Host :: jid:server(),
                          Params :: map()) -> Result when
      Result :: {ok, mod_mam_pm:lookup_result()} | {error, 'policy-violation'}.

-callback get_mam_pm_gdpr_data(mam_pm_gdpr_data(), mongooseim:host_type(), jid:jid()) -> mam_pm_gdpr_data().

-callback get_mam_muc_gdpr_data(mam_muc_gdpr_data(), mongooseim:host_type(), jid:jid()) -> mam_muc_gdpr_data().

-optional_callbacks([get_mam_pm_gdpr_data/3,
                     get_mam_muc_gdpr_data/3,
                     archive_size/4,
                     lookup_messages/3]).

-type mam_pm_gdpr_data() :: [{MessageID :: bitstring(), FromJID :: bitstring(), Message :: bitstring()}].

-type mam_muc_gdpr_data() :: [{MessageID :: bitstring(), Message :: bitstring()}].

-export_type([mam_pm_gdpr_data/0, mam_muc_gdpr_data/0]).

