%%%-------------------------------------------------------------------
%%% @author Uvarov Michael <arcusfelis@gmail.com>
%%% @copyright (C) 2013, Uvarov Michael
%%% @doc XEP-0313: Message Archive Management
%%%-------------------------------------------------------------------
-module(mod_mam_dump).

%% Utils
-export([create_dump_file/2,
         restore_dump_file/3]).

%% ----------------------------------------------------------------------
%% Imports

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("exml/include/exml.hrl").

%% ----------------------------------------------------------------------
%% Utils API

create_dump_file(Iter, OutFileName) ->
    {ok, FD} = file:open(OutFileName, [write]),
    F = fun(Data, _) -> file:write(FD, Data) end,
    F(<<"<stream>">>, undefined),
    create_dump_cycle(F, Iter, OutFileName),
    F(<<"</stream>">>, undefined),
    file:close(FD).

create_dump_cycle(F, Iter, Acc) ->
    case Iter() of
        {ok, {Data, ContIter}} ->
            Acc1 = F(Data, Acc),
            create_dump_cycle(F, ContIter, Acc1);
        {error, eof} ->
            Acc
    end.


-spec restore_dump_file(WriterF, InFileName, Opts) -> ok when
    WriterF :: fun(),
    InFileName :: file:filename(),
    Opts :: [Opt],
    Opt :: {rewrite_jids, RewriterF | Substitutions},
    RewriterF :: fun((BinJID) -> BinJID),
    Substitutions :: [{BinJID, BinJID}],
    BinJID :: binary().
restore_dump_file(WriterF, InFileName, Opts)
    when is_function(WriterF, 4) ->
    {ok, StreamAcc} = file:open(InFileName, [read, binary]),
    StreamF = fun read_data/1,
    InsF = fun insert_message_iter/2,
    InsAcc = {WriterF},
    {CallF1, CallAcc1} = apply_rewrite_jids(InsF, InsAcc, Opts),
    prepare_res(parse_xml_stream(StreamF, StreamAcc, CallF1, CallAcc1)).

prepare_res({ok, _, _}) -> ok;
prepare_res({stop, _, _, _}) -> ok;
prepare_res({error, Reason, _, _}) -> {error, Reason}.

%% @doc Wrap `CallF'. New function will rewrite jids of the argument.
apply_rewrite_jids(CallF, InitCallAcc, Opts) ->
    case proplists:get_value(rewrite_jids, Opts) of
        undefined ->
            {CallF, InitCallAcc};
        RewriterOpts ->
            RewriterF = rewrite_opts_to_fun(RewriterOpts),
            {fun(ResElem=#xmlel{}, CallAcc) ->
                    CallF(rewrite_jids(ResElem, RewriterF), CallAcc);
                (Event, CallAcc) ->
                    ?DEBUG("Skipped ~p.", [Event]),
                    CallF(Event, CallAcc)
             end, InitCallAcc}
    end.

rewrite_opts_to_fun(RewriterF) when is_function(RewriterF) ->
    RewriterF;
rewrite_opts_to_fun(Substitutions) when is_list(Substitutions) ->
    substitute_fun(Substitutions).

substitute_fun(Substitutions) ->
    Dict = dict:from_list(Substitutions),
    fun(Orig) -> dict:fetch(Orig, Dict) end.

insert_message_iter(ResElem=#xmlel{}, {WriterF}=Acc) ->
    case insert_xml_message(WriterF, ResElem) of
        {error, Reason} ->
            {error, Reason, Acc};
        ok ->
            {ok, Acc}
    end;
insert_message_iter(_, Acc) ->
    %% Skip `#xmlstreamelement{}' and `#xmlstreamend{}'
    {ok, Acc}.

read_data(FD) ->
    case file:read(FD, 1024) of
    {ok, Data} ->
        {ok, Data, FD};
    eof ->
        file:close(FD),
        {stop, eof, FD}
    end.

-spec parse_xml_stream(StreamF, StreamAcc, CallF, CallAcc) -> Res when
    StreamF   :: fun((StreamAcc) -> StreamRes),
    CallF     :: fun((Elem, CallAcc) -> CallRes),
    CallRes   :: {ok, CallAcc} | {stop, Reason, CallAcc},
    StreamRes :: {ok, Data, StreamAcc} | {stop, Reason, StreamAcc},
    Res       :: {ok, StreamAcc, CallAcc}
               | {stop,  Reason, StreamAcc, CallAcc}
               | {error, Reason, StreamAcc, CallAcc},
    Data      :: binary(),
    Elem      :: term(),
    StreamAcc :: term(),
    CallAcc   :: term().

parse_xml_stream(StreamF, StreamAcc, CallF, CallAcc) ->
    {ok, Parser} = exml_stream:new_parser(),
    parse_xml_stream_cycle(StreamF, StreamAcc, CallF, CallAcc, Parser).

parse_xml_stream_cycle(StreamF, StreamAcc, CallF, CallAcc, Parser) ->
    case StreamF(StreamAcc) of
    {ok, Data, StreamAcc1} ->
        case exml_stream:parse(Parser, Data) of
        {ok, Parser1, Elems} ->
            ?DEBUG("Parsed ~p elements.", [length(Elems)]),
            case stopable_foldl(CallF, CallAcc, Elems) of
            {ok, CallAcc1} ->
            parse_xml_stream_cycle(
                StreamF, StreamAcc1, CallF, CallAcc1, Parser1);
            {stop, Reason, CallAcc1} ->
                {stop, Reason, StreamAcc1, CallAcc1};
            {error, Reason, CallAcc1} ->
                {error, Reason, StreamAcc1, CallAcc1}
            end;
        {error, Error} ->
            {error, Error, StreamAcc1, CallAcc}
        end;
    {stop, Reason, StreamAcc1} ->
        exml_stream:free_parser(Parser),
        {stop, Reason, StreamAcc1, CallAcc}
    end.

stopable_foldl(F, Acc, [H|T]) ->
    case F(H, Acc) of
        {ok, Acc1} -> stopable_foldl(F, Acc1, T);
        {stop, Reason, Acc1} -> {stop, Reason, Acc1};
        {error, Reason, Acc1} -> {error, Reason, Acc1}
    end;
stopable_foldl(_F, Acc, []) ->
    {ok, Acc}.

debug_rewriting(F) ->
    fun(From) ->
        To = F(From),
        ?DEBUG("Rewrote from ~p to ~p.", [From, To]),
        To
    end.

-spec rewrite_jids(ResElem, RewriterF) -> ResElem when
    ResElem :: #xmlel{},
    RewriterF :: fun((BinJID) -> BinJID).
rewrite_jids(ResElem, F) when is_function(F) ->
    F1 = debug_rewriting(F),
    WithMsgF = fun(MsgElem) ->
        update_tag_attr(F1, <<"to">>,
        update_tag_attr(F1, <<"from">>, MsgElem))
        end,
    WithDelayF = fun(DelayElem) ->
        update_tag_attr(F1, <<"from">>, DelayElem)
        end,
    WithFwdF = fun(FwdElem) ->
        update_sub_tag(WithDelayF, <<"delay">>,
        update_sub_tag(WithMsgF, <<"message">>, FwdElem))
        end,
    update_sub_tag(WithFwdF, <<"forwarded">>, ResElem).


-spec update_tag_attr(F, Name, Elem) -> Elem when
    F :: fun((Value) -> Value),
    Name :: binary(),
    Value :: binary(),
    Elem :: #xmlel{}.
update_tag_attr(F, Name, Elem)
    when is_function(F, 1), is_binary(Name) ->
    Value = xml:get_tag_attr_s(Name, Elem),
    xml:replace_tag_attr(Name, F(Value), Elem).

-spec update_sub_tag(F, Name, Elem) -> Elem when
    F :: fun((Elem) -> Elem),
    Name :: binary(),
    Elem :: #xmlel{}.
update_sub_tag(F, Name, Elem=#xmlel{children=Children})
    when is_function(F, 1), is_binary(Name) ->
    Elem#xmlel{children=update_tags(F, Name, Children)};
update_sub_tag(_, _, Elem) ->
    Elem.

update_tags(F, Name, [H=#xmlel{name=Name}|T]) ->
    [F(H)|update_tags(F, Name, T)];
update_tags(F, Name, [H|T]) ->
    [H|update_tags(F, Name, T)];
update_tags(_, _, []) ->
    [].

%% @doc Insert a message into archive.
%% `ResElem' is `<result><forwarded>...</forwarded></result>'.
%% This format is used inside dump files.
-spec insert_xml_message(WriterF, ResElem) ->
        ok | {error, Reason} when
    WriterF :: fun(),
    ResElem :: #xmlel{},
    Reason :: term().
insert_xml_message(WriterF, ResElem) ->
    FwdElem = xml:get_subtag(ResElem, <<"forwarded">>),
    MessElem = xml:get_subtag(FwdElem, <<"message">>),
    BExtMessID = xml:get_tag_attr_s(<<"id">>, ResElem),
    MessID = mod_mam_utils:external_binary_to_mess_id(BExtMessID),
    FromJID = jlib:binary_to_jid(xml:get_tag_attr_s(<<"from">>, MessElem)),
    ToJID   = jlib:binary_to_jid(xml:get_tag_attr_s(<<"to">>, MessElem)),
    ?DEBUG("Restore message with id ~p.", [MessID]),
    WriterF(MessID, FromJID, ToJID, MessElem).


