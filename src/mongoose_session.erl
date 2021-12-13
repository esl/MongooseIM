-module(mongoose_session).

-export([merge_info/2]).
-export([get_info/1]).
-export([get_info/3]).
-export([set_info/3]).
-export([get_resource/1]).

-ignore_xref([behaviour_info/1, check_deps/1, ensure_loaded/1, get_service_opts/1,
              purge_service/1, start_service/2, stop/0, get_info/1, set_info/3]).

-include("session.hrl").

-spec merge_info(ejabberd_sm:session(), ejabberd_sm:session()) -> ejabberd_sm:session().
merge_info(New = #session{info = NewInfo}, #session{info = OldInfo}) ->
    New#session{info = maps:merge(OldInfo, NewInfo)}.

-spec get_info(ejabberd_sm:session()) -> ejabberd_sm:info().
get_info(#session{info = Info}) ->
    Info.

-spec get_info(ejabberd_sm:session(), ejabberd_sm:info_key(), any()) -> any().
get_info(#session{info = Info}, Key, Default) ->
    case maps:is_key(Key, Info) of
        true -> {Key, maps:get(Key, Info)};
        false -> Default
    end.

-spec set_info(ejabberd_sm:session(), ejabberd_sm:info_key(), any()) -> ejabberd_sm:session().
set_info(#session{info = Info} = Session, Key, Value) ->
    Session#session{info = maps:put(Key, Value, Info)}.

-spec get_resource(ejabberd_sm:session()) -> jid:lresource().
get_resource(#session{usr = {_U, _S, R}}) ->
    R.
