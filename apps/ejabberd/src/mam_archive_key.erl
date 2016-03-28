%%% @doc Key behaviour
%%%
%%% Converts JID and archive id from mod_mam_*_user module into term,
%%% that represents archive key. Usually used with mnesia.
-module(mam_archive_key).

-callback archive_key(mod_mam:archive_id(), ejabberd:jid()) -> term().
