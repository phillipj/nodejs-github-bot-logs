import Html exposing (..)
import Html.Attributes exposing (style, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL


type alias LogEntry =
  { msg : String
  , time : String
  , req_id : String
  }

type Progress =
  Initial | Fetching | Fetched | Failed

type alias Model =
  { logs : List LogEntry
  , state : Progress
  , reqIdFilter : Maybe String
  }

init : (Model, Cmd Msg)
init =
  (Model [] Initial Nothing, fetchNewLogs)


-- UPDATE


type Msg
  = FetchLogs
  | NewLogs (Result Http.Error String)
  | FilterByReqId String

logEntryDecoder : Decode.Decoder LogEntry
logEntryDecoder =
  Decode.map3 LogEntry
    (Decode.field "msg" Decode.string)
    (Decode.field "time" Decode.string)
    (Decode.field "req_id" Decode.string)

strToLogEntry : String -> Maybe LogEntry
strToLogEntry str =
  let
    decoded = Decode.decodeString logEntryDecoder str
  in
    case decoded of
      Ok entry ->
        Just entry

      Err msg ->
        Nothing

strToLogEntries : String -> List LogEntry
strToLogEntries rawLogStr =
  let
    lines = (String.split "\n" rawLogStr)
  in
    List.filterMap strToLogEntry lines

fetchNewLogs : Cmd Msg
fetchNewLogs =
  let
    -- request = Http.getString "http://admin:pass@github-bot.nodejs.org:3333/logs/bot.log"
    request = Http.getString "short.bot.log"
  in
    Http.send NewLogs request

resolveReqIdFilter : Maybe String -> String -> Maybe String
resolveReqIdFilter currentFilter wantedFilter =
  case currentFilter of
    (Just req_id) ->
      Nothing
    Nothing ->
      Just wantedFilter

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchLogs ->
      ({ model | state = Fetching }, fetchNewLogs)
    NewLogs (Ok logs) ->
      (Model (strToLogEntries logs) Fetched Nothing, Cmd.none)
    NewLogs (Err _) ->
      ({ model | state = Failed }, Cmd.none)
    FilterByReqId req_id ->
      ({ model | reqIdFilter = (resolveReqIdFilter model.reqIdFilter req_id) }, Cmd.none)


-- VIEW

progressToHtml : Progress -> Html Msg
progressToHtml state =
  case state of
    Initial ->
      p [] [ text "Not started fetching yet.." ]
    Fetching ->
      p [] [ text "Fetching bot logs.." ]
    Fetched ->
      p [] [ text "Logs fetched and displayed." ]
    Failed ->
      p [ style [("color", "red")] ] [ text "Failed to fetch logs :/" ]

entryToHtml : LogEntry -> Html Msg
entryToHtml entry =
  let
    styles = style [ ("background-color", "lightgrey")
                   , ("margin-bottom", "5px")
                   , ("padding", "5px")
                   ]
  in
    div [ styles ]
      [ text (entry.time ++ ": ")
      , text entry.msg
      , div [ style [("font-size", "smaller")] ]
          [ a [ onClick (FilterByReqId entry.req_id), href "#" ] [ text entry.req_id ] ]
      ]

filterLogs : Maybe String -> List LogEntry -> List LogEntry
filterLogs reqIdFilter logs =
  case reqIdFilter of
    Just req_id ->
      List.filter (\entry -> entry.req_id == req_id) logs
    Nothing ->
      logs

logsToHtml : Maybe String -> List LogEntry -> List (Html Msg)
logsToHtml reqIdFilter allLogs =
  let
    logs = filterLogs reqIdFilter allLogs
  in
  List.map (\entry -> entryToHtml entry) logs

view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text ("nodejs-github-bot logs") ]
    , progressToHtml model.state
    , div [] (logsToHtml model.reqIdFilter model.logs)
    ]
