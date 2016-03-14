%%% @doc Create tuple using information from bare jid
-module(mam_archive_key_server_user).
-export([archive_key/2]).

-behaviour(mam_archive_key).
-include_lib("ejabberd/include/jlib.hrl").

archive_key(_ArchiveId, #jid{lserver=Server, luser=User}) ->
    {Server, User}.
