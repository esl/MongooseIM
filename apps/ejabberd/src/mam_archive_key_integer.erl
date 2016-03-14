%%% @doc Use ArchiveID provided by mod_mam_something_user
-module(mam_archive_key_integer).
-export([archive_key/2]).

-behaviour(mam_archive_key).
archive_key(ArchiveId, _ArchiveJID) ->
    ArchiveId.
