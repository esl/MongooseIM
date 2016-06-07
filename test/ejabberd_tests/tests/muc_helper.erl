-module(muc_helper).

-include_lib("exml/include/exml.hrl").

-export([foreach_occupant/3,
        foreach_recipient/2,
        load_muc/1,
        unload_muc/0]).

-type verify_fun() :: fun((Incoming :: #xmlel{}) -> any()).

-export_type([verify_fun/0]).

-spec foreach_occupant(
        Users :: [escalus:client()], Stanza :: #xmlel{}, VerifyFun :: verify_fun()) -> ok.
foreach_occupant(Users, Stanza, VerifyFun) ->
    lists:foreach(
      fun(Sender) ->
              escalus:send(Sender, Stanza),
              case exml_query:path(Stanza, [{attr, <<"type">>}]) of
                  <<"get">> ->
                      Incoming = escalus:wait_for_stanza(Sender),
                      escalus:assert(is_iq_result, Incoming),
                      VerifyFun(Incoming);
                  _ ->
                      foreach_recipient(Users, VerifyFun),
                      case Stanza of
                          #xmlel{ name = <<"iq">> } ->
                              escalus:assert(is_iq_result, escalus:wait_for_stanza(Sender));
                          _ ->
                              ok
                      end
              end
      end, Users).

-spec foreach_recipient(Users :: [escalus:client()], VerifyFun :: verify_fun()) -> ok.
foreach_recipient(Users, VerifyFun) ->
    lists:foreach(
      fun(Recipient) ->
              VerifyFun(escalus:wait_for_stanza(Recipient))
      end, Users).

load_muc(Host) ->
    dynamic_modules:start(<<"localhost">>, mod_muc,
        [{host, binary_to_list(Host)},
            {access, muc},
            {access_create, muc_create}]),
    dynamic_modules:start(<<"localhost">>, mod_muc_log,
        [{outdir, "/tmp/muclogs"},
            {access_log, muc}]).

unload_muc() ->
    dynamic_modules:stop(<<"localhost">>, mod_muc),
    dynamic_modules:stop(<<"localhost">>, mod_muc_log).
