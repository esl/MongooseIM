-module(mod_event_pusher_http_custom).
-export([should_make_req/6, prepare_body/7, prepare_headers/7]).

should_make_req(Acc, _, _, _, _, _) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> -> true;
        _ -> false
    end.

prepare_headers(_, _, _, _, _, _, _) ->
    mod_event_pusher_http_defaults:prepare_headers(x, x, x, x, x, x, x).

prepare_body(_Acc, Dir, _Host, Message, _Sender, _Receiver, _Opts) ->
    <<(atom_to_binary(Dir, utf8))/binary, $-, Message/binary>>.
