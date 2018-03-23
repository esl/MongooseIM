%%%-------------------------------------------------------------------
%%% @author ludwikbukowski
%%% @copyright (C) 2018, Erlang-Solutions
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 13:22
%%%-------------------------------------------------------------------
-module(mod_inbox_utils).
-include("mongoose_ns.hrl").
-include("jlib.hrl").
-author("ludwikbukowski").
-compile(export_all).

get_reset_markers(Host) ->
  Markers = gen_mod:get_module_opt(Host, mod_inbox, markers, [displayed]),
  MarkersBin = [atom_to_binary(M, unicode) || M <- Markers],
  Possible = [<<"acknowledged">>, <<"displayed">>, <<"received">>],
  Vals = lists:all(fun(Marker) -> lists:member(Marker, Possible) end, MarkersBin),
  if
    Vals -> MarkersBin;
    true -> erlang:throw(unknown_markers, MarkersBin)
  end.


has_chat_marker(_Packet, []) -> false;
has_chat_marker(Packet, [Marker | R]) ->
  case exml_query:subelement_with_ns(Packet, ?NS_CHAT_MARKERS) of
    #xmlel{name = Marker}    -> true;
    _                        -> has_chat_marker(Packet, R)
  end.

get_markered_msg_id(#xmlel{name = <<"message">>} = Msg) ->
  %% check if "received" chat marker and get the marked message id
  case exml_query:paths(Msg, [{element, <<"displayed">>}, {attr, <<"id">>}]) of
    [Id] ->
      Id;
    _ ->
      <<"noid">>
  end.

get_msg_id(#xmlel{name = <<"message">>} = Msg) ->
  exml_query:attr(Msg, <<"id">>, <<"noid">>).


add_from(Msg = #xmlel{attrs = Attrs}, FromBin) ->
  case exml_query:attr(Msg, <<"from">>, undefined) of
    undefined ->
      Msg#xmlel{attrs = Attrs ++ [{<<"from">>, FromBin}]};
    _ ->
      Msg
  end.

wrapper_id() ->
  uuid:uuid_to_string(uuid:get_v4(), binary_standard).
