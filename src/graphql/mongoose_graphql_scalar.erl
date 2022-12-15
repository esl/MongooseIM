-module(mongoose_graphql_scalar).
-export([input/2, output/2]).
-ignore_xref([input/2, output/2]).

-include_lib("jid/include/jid.hrl").

-spec input(Type, Value) -> {ok, Coerced} | {error, Reason}
  when
    Type :: binary(),
    Value :: binary() | pos_integer(),
    Coerced :: any(),
    Reason :: term().
input(<<"DateTime">>, DT) -> binary_to_microseconds(DT);
input(<<"XmlElement">>, Value) -> exml:parse(Value);
input(<<"JID">>, Jid) -> jid_from_binary(Jid);
input(<<"BareJID">>, Jid) -> bare_jid_from_binary(Jid);
input(<<"FullJID">>, Jid) -> full_jid_from_binary(Jid);
input(<<"UserName">>, User) -> user_from_binary(User);
input(<<"RoomName">>, Room) -> room_from_binary(Room);
input(<<"DomainName">>, Domain) -> domain_from_binary(Domain);
input(<<"ResourceName">>, Res) -> resource_from_binary(Res);
input(<<"NodeName">>, Node) -> node_from_binary(Node);
input(<<"NonEmptyString">>, Value) -> non_empty_string_to_binary(Value);
input(<<"PosInt">>, Value) -> validate_pos_integer(Value);
input(<<"NonNegInt">>, Value) -> validate_non_neg_integer(Value);
input(Ty, V) ->
    error_logger:info_report({coercing_generic_scalar, Ty, V}),
    {ok, V}.

-spec output(Type, Value) -> {ok, Coerced} | {error, Reason}
  when
    Type :: binary(),
    Value :: binary() | pos_integer(),
    Coerced :: any(),
    Reason :: term().
output(<<"DateTime">>, DT) -> {ok, microseconds_to_binary(DT)};
output(<<"XmlElement">>, Elem) -> {ok, exml:to_binary(Elem)};
output(<<"JID">>, Jid) -> {ok, jid:to_binary(Jid)};
output(<<"UserName">>, User) -> {ok, User};
output(<<"DomainName">>, Domain) -> {ok, Domain};
output(<<"ResourceName">>, Res) -> {ok, Res};
output(<<"NonEmptyString">>, Value) -> binary_to_non_empty_string(Value);
output(<<"PosInt">>, Value) -> validate_pos_integer(Value);
output(<<"NonNegInt">>, Value) -> validate_non_neg_integer(Value);
output(Ty, V) ->
    error_logger:info_report({output_generic_scalar, Ty, V}),
    {ok, V}.

jid_from_binary(Value) ->
    case jid:from_binary(Value) of
        error ->
            {error, failed_to_parse_jid};
        #jid{luser = <<>>} ->
            {error, jid_without_local_part};
        Jid ->
            {ok, Jid}
    end.

bare_jid_from_binary(Value) ->
    case jid:from_binary(Value) of
        error ->
            {error, failed_to_parse_jid};
        #jid{luser = <<>>} ->
            {error, jid_without_local_part};
        Jid = #jid{lresource = <<>>} ->
            {ok, Jid};
        #jid{} ->
            {error, jid_with_resource}
    end.

full_jid_from_binary(Value) ->
    case jid:from_binary(Value) of
        error ->
            {error, failed_to_parse_jid};
        #jid{luser = <<>>} ->
            {error, jid_without_local_part};
        #jid{lresource = <<>>} ->
            {error, jid_without_resource};
        Jid ->
            {ok, Jid}
    end.

user_from_binary(<<>>) ->
    {error, empty_user_name};
user_from_binary(Value) ->
    case jid:nodeprep(Value) of
        error ->
            {error, failed_to_parse_user_name};
        User ->
            {ok, User}
    end.

room_from_binary(<<>>) ->
    {error, empty_room_name};
room_from_binary(Value) ->
    case jid:nodeprep(Value) of
        error ->
            {error, failed_to_parse_room_name};
        Room ->
            {ok, Room}
    end.

domain_from_binary(<<>>) ->
    {error, empty_domain_name};
domain_from_binary(Value) ->
    case jid:nameprep(Value) of
        error ->
            {error, failed_to_parse_domain_name};
        Domain ->
            {ok, Domain}
    end.

resource_from_binary(<<>>) ->
    {error, empty_resource_name};
resource_from_binary(Value) ->
    case jid:resourceprep(Value) of
        error ->
            {error, failed_to_parse_resource_name};
        Res ->
            {ok, Res}
    end.

node_from_binary(<<>>) ->
    {error, empty_node_name};
node_from_binary(NodeName) ->
    case string:lexemes(binary_to_list(NodeName), "@") of
        [_Name, _Host] ->
            {ok, binary_to_atom(NodeName)};
        ["self"] ->
            {ok, node()};
        _ ->
            {error, incorrect_node_name}
    end.

binary_to_microseconds(DT) ->
    case mod_mam_utils:maybe_microseconds(DT) of
        undefined ->
            {error, failed_to_parse_datetime};
        Microseconds ->
            {ok, Microseconds}
    end.

non_empty_string_to_binary(<<>>) ->
    {error, "Given string is empty"};
non_empty_string_to_binary(String) ->
    {ok, String}.

binary_to_non_empty_string(<<>>) ->
    {error, "Empty binary cannot be converted to NonEmptyString"};
binary_to_non_empty_string(Val) ->
    {ok, Val}.

validate_pos_integer(PosInt) when is_integer(PosInt), PosInt > 0 ->
    {ok, PosInt};
validate_pos_integer(_Value) ->
    {error, "Value is not a positive integer"}.

validate_non_neg_integer(NonNegInt) when is_integer(NonNegInt), NonNegInt >= 0 ->
    {ok, NonNegInt};
validate_non_neg_integer(_Value) ->
    {error, "Value is not a non-negative integer"}.

microseconds_to_binary(Microseconds) ->
    Opts = [{offset, "Z"}, {unit, microsecond}],
    list_to_binary(calendar:system_time_to_rfc3339(Microseconds, Opts)).
