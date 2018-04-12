# `mod_mam` Custom Backend

You can implement a custom backend for mod_mam in order to support
 additional databases or your own proprietary service.

To configure MongooseIM to use your custom module make it have its own
 toplevel place within the modules section of your configuration file,
 eg:

``` erlang
{modules, [
  ...
  {mod_mam_custom_arch, []},
  {mod_mam, [...]},
  ...
]}
```

This module needs to implement the `gen_mod` behavior and typically
needs to add handlers for a (subset of) hooks triggered by mod_mam:

 * mam_archive_message
 * mam_archive_size
 * mam_lookup_messages
 * mam_remove_archive
 * mam_purge_single_message
 * mam_purge_multiple_messages

See official backend implementations (like `mod_mam_odbc_arch.erl`) to get an idea what their
signature looks like and what they are meant to do.

A minimalistic example that only implements message lookup (without
the actual data access):

``` erlang
start(Host, _Opts) ->
  ...
  ejabberd_hooks:add(mam_lookup_messages, Host, ?MODULE, lookup_messages, 50),
  ...
  ok.

...

-spec lookup_messages(any(), ejabberd:server(), map()) -> {ok, mod_mam:lookup_result()} |
                                                          {error, 'policy-violation'} |
                                                          {error, any()}.
lookup_messages({error, _Reason} = Result, _Host, _Params) -> Result;
lookup_messages(_Result, Host, #{owner_jid        := OwnerJid,
                                 rsm              := RSM,
                                 start_ts         := undefined,
                                 end_ts           := undefined,
                                 with_jid         := undefined,
                                 search_text      := undefined,
                                 page_size        := PageSize,
                                 limit_passed     := _LimitPassed,
                                 max_result_limit := _MaxResultLimit,
                                 is_simple        := _IsSimple      }) ->
    %% Your implementation goes here
    ...
    ;
lookup_messages(_Result, _Host, Params) ->
    lager:info("unsupported params: ~p", [Params]),
    {error, 'not-supported'}.

...

```

If this module uses a different format for its archive-ids other than
what's in `mam_uid_nodetime` you can configure a custom `uid_module`:

``` erlang
{modules, [
  ...
  {mod_mam, [{uid_module, mam_uid_custom}]},
  ...
]}
```

Your custom UID module needs to implement the `mam_uid` behaviour.

There's `mam_uid_uuid` provided for convenience, it implements time base UUIDs.
See https://www.famkruithof.net/guid-uuid-timebased.html for details.
