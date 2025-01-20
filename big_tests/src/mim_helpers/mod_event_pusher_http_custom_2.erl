-module(mod_event_pusher_http_custom_2).
-export([should_make_req/6, prepare_body/7, prepare_headers/7]).

should_make_req(Acc, out, _, _, _, _) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> -> true;
        _ -> false
    end;

should_make_req(_, in, _, _, _, _) -> false.

prepare_headers(_, _, _, _, _, _, _) ->
    mod_event_pusher_http_defaults:prepare_headers(x, x, x, x, x, x, x).

prepare_body(_Acc, Dir, _Host, Message, _Sender, _Receiver, _Opts) ->
    <<$2, $-, (atom_to_binary(Dir, utf8))/binary, $-, Message/binary>>.
