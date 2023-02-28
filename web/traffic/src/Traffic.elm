port module Traffic exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Json.Encode as Encode
import Json.Decode as Decode
import Dict
import Time
import Task

main = Browser.element {init = init, view = view, update = update, subscriptions = subscriptions}

-- TYPES


type ConnectionState = Open
                       | Closed
                       | Lost
                       | Failed

type alias Jid = String
type alias Pid = String
type alias NewTrace = {pid : Pid, bare_jid : Jid, start_time : Float, full_jid : Jid}
type alias Stanza = { dir : String, time : Float, stanza : String}
type alias Mappings = Dict.Dict Pid NewTrace
type alias Model = { tracing : Bool,
                     traced_pids : List Pid,
                     current_pid : Pid,
                     stanzas : List Stanza,
                     mappings : Mappings,
                     announcement : Announcement,
                     timezone : Time.Zone,
                     ws_addr : WsAddr,
                     conn_state : ConnectionState}

type alias DecodeResult a = Result Decode.Error a
type alias UpdateResult = (Model, Cmd Msg)

type Announcement = Empty
                    | Error String

type Msg = SetStatus Bool
           | ClearAll
           | SelectPid Pid
           | RecEvent Encode.Value
           | SetWsAddr WsAddr
           | ChangeConnection
           | Skip
           | TimeZone Time.Zone

type WsAddr = WsAddr String

-- UPDATE

init : String -> (Model, Cmd Msg)
init adr = ({ tracing = False,
            traced_pids = [],
            current_pid = "",
            stanzas = [],
            mappings = Dict.empty,
            announcement = Empty,
            timezone = Time.utc,
            ws_addr = WsAddr adr,
            conn_state = Closed},
          Task.perform TimeZone Time.here)

update : Msg -> Model -> UpdateResult
update msg model =
    do_update msg {model | announcement = Empty}

do_update msg model =
    case msg of
        ClearAll -> (model, outPort(simpleEvent "clear_all"))
        SetStatus st ->
                (model, setTraceEvent st)
        SelectPid pid -> ({model | current_pid = pid}, outPort(outEvent "get_trace" [("pid", Encode.string pid)]))
        SetWsAddr wsaddr -> ({model | ws_addr = wsaddr}, Cmd.none)
        ChangeConnection ->
            case model.ws_addr of
                WsAddr s ->
                    (model, outPort(outEvent "change_connection" [("addr", Encode.string s)]))
        RecEvent v ->
            case Decode.decodeValue (Decode.field "event" Decode.string) v of
                Ok eventName ->
                    let z = Debug.log "v" eventName in
                    handleEvent eventName v model
                Err error ->
                    let x = Debug.log "error" error in
                    (model, Cmd.none)
        TimeZone tz -> ({model | timezone = tz}, Cmd.none)
        Skip -> (model, Cmd.none)


-- INCOMING

handleEvent : String -> Decode.Value -> Model -> UpdateResult
handleEvent ename v model =
    case ename of
        "status" -> handleStatus v model
        "new_trace" -> handleNewTrace v model
        "cleared_all" -> (clearAll model, Cmd.none)
        "error" -> (model
                    |> unTrace
                    |> showErrorMessage v,
                    Cmd.none)
        "get_trace" -> handleGetTrace v model
        "message" -> handleMessage v model
        "reinitialise" -> (clearAll ({model | conn_state = Open}), setTraceEvent model.tracing) -- server was probably restarted, we set our status
        "initialise" -> ({model | conn_state = Open}, outPort(simpleEvent "get_status")) -- server default may change
        "connection_lost" -> ({model | conn_state = Lost}, Cmd.none)
        "connection_failed" -> ({model | conn_state = Failed}, Cmd.none)
        _ -> (model, Cmd.none)

clearAll : Model -> Model
clearAll model = {model | traced_pids = [], stanzas = [], current_pid = ""}

setTraceEvent : Bool -> Cmd Msg
setTraceEvent st = outPort(outEvent "trace_flag" [("value", Encode.bool st)])


handleStatus : Decode.Value -> Model -> UpdateResult
handleStatus v model =
    (v, model)
    |> handleDecodedValue (decodeField "trace_flag" Decode.bool)
                          handleStatusOk


handleDecodedValue : (Decode.Value -> DecodeResult a) -- decoder
                      -> (Model -> a -> UpdateResult) -- handler if ok
                      -> (Decode.Value, Model)
                      -> UpdateResult
handleDecodedValue decoder handler (v, model) =
    case decoder v of
        Ok res -> handler model res
        Err error ->
            let x = Debug.log "error" error in
            (model, Cmd.none)

handleStatusOk : Model -> Bool -> UpdateResult
handleStatusOk model trace_flag = ({model | tracing = trace_flag}, Cmd.none)

unTrace model = {model | tracing = False}

handleNewTrace : Decode.Value -> Model -> UpdateResult
handleNewTrace v model =
    (v, model)
    |> handleDecodedValue decodeNewTrace
                          handleNewTraceOk

handleNewTraceOk : Model -> NewTrace -> UpdateResult
handleNewTraceOk model newtrace =
    (model |> updateMapping newtrace |> updateTraces newtrace,
     Cmd.none)

updateMapping newtrace model =
    {model | mappings = Dict.insert newtrace.pid newtrace model.mappings}

updateTraces newtrace model =
    case List.member newtrace.pid model.traced_pids of
        True -> model
        False -> {model | traced_pids = newtrace.pid :: model.traced_pids}

handleGetTrace : Decode.Value -> Model -> UpdateResult
handleGetTrace v model =
    (v, model)
    |> handleDecodedValue (Decode.decodeValue
                               (Decode.field
                                    "payload"
                                    (Decode.field
                                         "trace"
                                         (Decode.list decodeStanza))))
                          handleGetTraceOk

handleGetTraceOk : Model -> List Stanza -> UpdateResult
handleGetTraceOk model stanzas =
    ({model | stanzas = stanzas}, Cmd.none)

handleMessage : Decode.Value -> Model -> UpdateResult
handleMessage v model =
    (v, model)
    |> handleDecodedValue (Decode.decodeValue (Decode.field "payload" decodeStanza))
                          handleMessageOk

handleMessageOk : Model -> Stanza -> UpdateResult
handleMessageOk model stanza =
    ({model | stanzas = stanza :: model.stanzas}, Cmd.none)


-- SOME USEFUL DECODERS

decodeStanza = Decode.map3 Stanza (Decode.field "dir" Decode.string)
                                  (Decode.field "time" Decode.float)
                                  (Decode.field "stanza" Decode.string)

decodeField : String -> Decode.Decoder a -> Decode.Value -> DecodeResult a
decodeField fieldname decoder v =
    Decode.decodeValue  (Decode.field "payload" (Decode.field fieldname decoder)) v


showErrorMessage v model =
    case decodeField "reason" Decode.string v of
        Ok reason -> {model | announcement = Error reason}
        _ -> model

decodeNewTrace v = Decode.decodeValue  (Decode.field "payload" newTraceDecoder) v

newTraceDecoder = Decode.map4 NewTrace (Decode.field "pid" Decode.string )
                                       (Decode.field "bare_jid" Decode.string)
                                       (Decode.field "start_time" Decode.float)
                                       (Decode.field "full_jid" Decode.string)
-- COMMUNICATION TOOLS

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [incPort RecEvent]

port outPort : (Encode.Value, Encode.Value) -> Cmd msg
port incPort : (Encode.Value -> msg) -> Sub msg

emptyPayload = Encode.object []

simpleEvent : String -> (Encode.Value, Encode.Value)
simpleEvent evt = (Encode.string evt, emptyPayload)


outEvent : String -> List (String, Encode.Value) -> (Encode.Value, Encode.Value)
outEvent evtname payload =
    (Encode.string evtname,
     Encode.object payload)


-- VIEW

view : Model -> Html Msg
view model =
    div [class "all"][
        div [class "top"][
            div [class "header"][text "MongooseIM traffic tracer"],
            showEnableButton model.tracing,
            button [class "clearButton", onClick ClearAll] [ text "clear all"],
            showConnState model
        ],
        div [class "main"][
            div [class "left"][
                viewJids model.current_pid model.traced_pids model.mappings model.timezone
            ],
            div [class "right"][
                div [class "current"][text (displayJid model.current_pid model.mappings)],
                viewAnnouncement model.announcement,
                viewStanzas model.stanzas
            ]
        ]
    ]

viewAnnouncement ann =
    case ann of
        Empty -> div [class "hidden"][]
        Error "too_many_traced_procs" -> div [class "problem"] [text "Too many pids traced, tracing disabled"]
        Error reason -> div [class "problem"] [text reason]

showWsAddr model =
    case model.ws_addr of
        WsAddr a -> input [type_ "text", value a,
                           onInput (\s -> SetWsAddr (WsAddr s)),
                           on "keydown" (Decode.map sendIfEnter keyCode)] []

sendIfEnter : Int-> Msg
sendIfEnter keycode =
    case keycode of
        13 -> ChangeConnection
        _ -> Skip

showConnState model =
    div [class ("connection_state")][
        showWsAddr model,
        div [class (connStateClass model.conn_state)]
            [text (connStateLabel model.conn_state)]
    ]

connStateClass state =
    case state of
        Open -> "good"
        _ -> "problem"

connStateLabel state =
    case state of
        Open -> "connection open"
        Lost -> "trying to reconnect..."
        Closed -> "no connection"
        Failed -> "connection failed"

viewJids : Pid -> List String -> Mappings -> Time.Zone -> Html Msg
viewJids curr_pid traced_pids mappings tz =
    div [class "tracing"] [
        div [class "label"] [text "Traced sessions:"],
        div [class "jids"] (List.map (showJid curr_pid mappings tz) (List.reverse traced_pids))
    ]

showJid : Pid -> Mappings -> Time.Zone -> Pid -> Html Msg
showJid curr_pid mappings tz pid =
    div [class "jid", class (mb_current curr_pid pid)]
        [a [onClick (SelectPid pid)]
           [text (displayJid pid mappings),
            text (showStartTime pid mappings tz)]]

mb_current curr_pid pid =
    if curr_pid == pid then "current_pid" else ""

displayJid : Pid -> Mappings -> Pid
displayJid pid mappings =
    case Dict.get pid mappings of
        Just info ->
            case (info.bare_jid, info.full_jid) of
                ("", f) -> f
                (b, "") -> b
                _ -> pid
        Nothing ->
            pid

showStartTime pid mappings tz =
    case Dict.get pid mappings of
        Just info ->
            let t = Time.millisToPosix ((truncate info.start_time) * 1000) in
            ",   " ++
            timeFormat (Time.toHour tz t) ++ ":"
            ++ timeFormat (Time.toMinute tz t) ++ ":"
            ++ timeFormat (Time.toSecond tz t)
            --String.fromFloat info.start_time
        Nothing ->
            ""

enableClass is_enabled =
    case is_enabled of
        True -> "true"
        False -> "false"

showEnableButton is_enabled =
    div [class "enabled"][
        button [class (enableClass is_enabled),
                onClick (SetStatus (not is_enabled))]
                [ text "Tracing"]
    ]

viewStanzas stanzas =
    div [class "stanzas"] [
        div [class "label"] [text "Stanzas"],
        div [class "stanzalist"] (List.map showStanza (List.reverse stanzas))
    ]


showStanza stanza =
    div [class ("stanza " ++ stanza.dir)]
        (div [class "time"][text (formatTime stanza.time)]
       :: (List.map showStanzaPart (String.split "\n" stanza.stanza)))

showStanzaPart p =
    div [class "part"][text p]

formatTime tm =
    tm |> String.fromFloat |> String.split "." |> formatParts

formatParts parts =
    case parts of
        [i, f] ->
            String.concat [
                           i |> String.slice 0 2 |> String.padLeft 2 ' ',
                           ".",
                           f |> String.slice 0 2 |> String.padRight 2 '0'
                           ]
        ["0"] ->
            "0.00"
        _ ->
             "bueee"

timeFormat i = String.fromInt i |> String.padLeft 2 '0'