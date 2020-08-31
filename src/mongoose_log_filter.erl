-module(mongoose_log_filter).
-export([fill_metadata/2]).

%% The templater in flatlog works with meta fields.
%% So, we would need a filter, that takes the interesting fields
%% from msg to metadata.
fill_metadata(Event=#{msg := {report, Msg}, meta := Meta}, Fields) ->
    FieldMap = maps:with(Fields, Msg),
    Event#{meta => maps:merge(FieldMap, Meta)}.
