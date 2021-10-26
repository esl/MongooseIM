-module(vcard_helper).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [rpc/4, mim/0]).
-import(domain_helper, [host_type/0]).

is_vcard_ldap() ->
    ldap == rpc(mim(), gen_mod, get_module_opt, [host_type(), mod_vcard, backend, mnesia]).
