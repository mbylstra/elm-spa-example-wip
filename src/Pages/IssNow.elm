module Pages.IssNow exposing (..)

import Http
import Json.Decode as Json
import Html exposing (Html, div, a, text, button, p)
import Types exposing (PageLoadingStatus(..))


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { maybeLocation : Maybe Location
    , loadingStatus : PageLoadingStatus
    }


type alias Location =
    { latitude : String
    , longitute : String
    }


initModel : Model
initModel =
    { maybeLocation = Nothing
    , loadingStatus = Loading
    }


initCmd : Cmd Msg
initCmd =
    sendRequest


getLoadingStatus : Model -> PageLoadingStatus
getLoadingStatus model =
    model.loadingStatus



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = DataFetched (Result Http.Error Location)


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataFetched result ->
            case result of
                Ok people ->
                    { model | maybeLocation = Just people, loadingStatus = Loaded }

                Err err ->
                    { model | loadingStatus = DataFetchError <| toString err }


navigateTo : Model -> ( Model, Cmd Msg )
navigateTo model =
    { model | loadingStatus = Loading } ! [ sendRequest ]


reload : Model -> ( Model, Cmd Msg )
reload model =
    { model | loadingStatus = Reloading } ! [ sendRequest ]



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html msg
view model =
    case model.maybeLocation of
        Just model ->
            div []
                [ text <| toString model ]

        Nothing ->
            div []
                [ text "loading current ISS location" ]



--------------------------------------------------------------------------------
-- Data Fetching
--------------------------------------------------------------------------------


internationalSpaceStationNowUrl : String
internationalSpaceStationNowUrl =
    "http://api.open-notify.org/iss-now.json"



--- example response
-- {
-- "timestamp": 1499469359,
-- "iss_position": {
-- "latitude": "-12.1526",
-- "longitude": "-150.5445"
-- },
-- "message": "success"
-- }


request : Http.Request Location
request =
    Http.get internationalSpaceStationNowUrl locationDecoder


sendRequest : Cmd Msg
sendRequest =
    Http.send DataFetched request


timestampDecoder : Json.Decoder Int
timestampDecoder =
    Json.field "timestamp" Json.int


locationDecoder : Json.Decoder Location
locationDecoder =
    Json.map2 Location
        (Json.at [ "iss_position", "latitude" ] Json.string)
        (Json.at [ "iss_position", "longitude" ] Json.string)
