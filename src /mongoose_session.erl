-module(mongoose_session).
-export([merge_info/2]).

-include("mongoose.hrl").
-include("session.hrl").

-spec merge_info(#session{}, #session{}) -> #session{}.
merge_info(New, Old) ->
    NewInfo = orddict:from_list(New#session.info),
    OldInfo = orddict:from_list(Old#session.info),
    New#session{info =
                    orddict:to_list(orddict:merge(fun merger/3,
                                                  NewInfo, OldInfo))}.

merger(_Key, NewVal, _OldVal) ->
    NewVal.
