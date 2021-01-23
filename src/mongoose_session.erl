-module(mongoose_session).

-export([merge_info/2]).
-export([get_info/1]).
-export([get_resource/1]).

-include("mongoose.hrl").
-include("session.hrl").

-spec merge_info(ejabberd_sm:session(), ejabberd_sm:session()) -> ejabberd_sm:session().
merge_info(New = #session{info = NewInfo}, #session{info = OldInfo}) ->
    New#session{info = maps:merge(OldInfo, NewInfo)}.

-spec get_info(ejabberd_sm:session()) -> ejabberd_sm:info().
get_info(#session{info = Info}) ->
    Info.

-spec get_resource(ejabberd_sm:session()) -> jid:lresource().
get_resource(#session{usr = {_U, _S, R}}) ->
    R.
