-module(mongoose_graphql_enum).

-export([input/2, output/2]).

-ignore_xref([input/2, output/2]).

input(<<"PresenceShow">>, Show) ->
    {ok, string:lowercase(Show)};
input(<<"PresenceType">>, Type) ->
    {ok, string:lowercase(Type)};
input(<<"Affiliation">>, <<"OWNER">>) -> {ok, owner};
input(<<"Affiliation">>, <<"MEMBER">>) -> {ok, member};
input(<<"Affiliation">>, <<"NONE">>) -> {ok, none};
input(<<"AddressTags">>, Name) -> {ok, Name};
input(<<"BlockingAction">>, <<"ALLOW">>) -> {ok, allow};
input(<<"BlockingAction">>, <<"DENY">>) -> {ok, deny};
input(<<"BlockedEntityType">>, <<"USER">>) -> {ok, user};
input(<<"BlockedEntityType">>, <<"ROOM">>) -> {ok, room};
input(<<"EmailTags">>, Name) -> {ok, Name};
input(<<"SubAction">>, <<"INVITE">>) -> {ok, invite};
input(<<"SubAction">>, <<"ACCEPT">>) -> {ok, accept};
input(<<"SubAction">>, <<"DECLINE">>) -> {ok, decline};
input(<<"SubAction">>, <<"CANCEL">>) -> {ok, cancel};
input(<<"MutualSubAction">>, <<"CONNECT">>) -> {ok, connect};
input(<<"MutualSubAction">>, <<"DISCONNECT">>) -> {ok, disconnect};
input(<<"MUCRole">>, <<"VISITOR">>) -> {ok, visitor};
input(<<"MUCRole">>, <<"PARTICIPANT">>) -> {ok, participant};
input(<<"MUCRole">>, <<"MODERATOR">>) -> {ok, moderator};
input(<<"MUCAffiliation">>, <<"NONE">>) -> {ok, none};
input(<<"MUCAffiliation">>, <<"MEMBER">>) -> {ok, member};
input(<<"MUCAffiliation">>, <<"OUTCAST">>) -> {ok, outcast};
input(<<"MUCAffiliation">>, <<"ADMIN">>) -> {ok, admin};
input(<<"MUCAffiliation">>, <<"OWNER">>) -> {ok, owner};
input(<<"PrivacyClassificationTags">>, Name) -> {ok, Name};
input(<<"TelephoneTags">>, Name) -> {ok, Name};
input(<<"LogLevel">>, Name) -> {ok, binary_to_atom(string:lowercase(Name))};
input(<<"MetricType">>, Name) -> {ok, Name}.

output(<<"DomainStatus">>, Type) ->
    {ok, string:uppercase(atom_to_binary(Type))};
output(<<"PresenceShow">>, Show) ->
    {ok, string:uppercase(Show)};
output(<<"PresenceType">>, Type) ->
    {ok, string:uppercase(Type)};
output(<<"AuthStatus">>, Status) ->
    {ok, atom_to_binary(Status, utf8)};
output(<<"AuthType">>, Type) ->
    {ok, string:uppercase(atom_to_binary(Type))};
output(<<"Affiliation">>, Aff) ->
    {ok, string:uppercase(atom_to_binary(Aff))};
output(<<"BlockingAction">>, Action) ->
    {ok, string:uppercase(atom_to_binary(Action))};
output(<<"BlockedEntityType">>, What) ->
    {ok, string:uppercase(atom_to_binary(What))};
output(<<"ContactSub">>, Type) when Type =:= both;
                                    Type =:= from;
                                    Type =:= to;
                                    Type =:= none ->
    {ok, string:uppercase(atom_to_binary(Type))};
output(<<"ContactAsk">>, Type) when Type =:= subscrube;
                                    Type =:= unsubscribe;
                                    Type =:= in;
                                    Type =:= out;
                                    Type =:= both;
                                    Type =:= none ->
    {ok, string:uppercase(atom_to_binary(Type))};
output(<<"MUCRole">>, Role) ->
    {ok, string:uppercase(atom_to_binary(Role))};
output(<<"MUCAffiliation">>, Aff) ->
    {ok, string:uppercase(atom_to_binary(Aff))};
output(<<"AddressTags">>, Name) -> {ok, Name};
output(<<"EmailTags">>, Name) -> {ok, Name};
output(<<"PrivacyClassificationTags">>, Name) -> {ok, Name};
output(<<"LogLevel">>, Name) -> {ok, string:uppercase(atom_to_binary(Name))};
output(<<"TelephoneTags">>, Name) -> {ok, Name};
output(<<"MetricType">>, Type) -> {ok, Type};
output(<<"StatusCode">>, Code) -> {ok, Code}.
