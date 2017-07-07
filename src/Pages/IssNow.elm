module Pages.IssNow exposing (..)

import Http
import Json.Decode as Json
import Html exposing (Html, div, a, text, button, p)


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { timestamp : Int
    , location : Location
    }


type alias Location =
    { latitude : String
    , longitute : String
    }



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Maybe Model -> Html msg
view maybeModel =
    case maybeModel of
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


request : Http.Request Model
request =
    Http.get internationalSpaceStationNowUrl decoder


sendRequest : (Result Http.Error Model -> msg) -> Cmd msg
sendRequest tagger =
    Http.send tagger request


decoder : Json.Decoder Model
decoder =
    Json.map2 Model timestampDecoder locationDecoder


timestampDecoder : Json.Decoder Int
timestampDecoder =
    Json.field "timestamp" Json.int


locationDecoder : Json.Decoder Location
locationDecoder =
    Json.map2 Location
        (Json.at [ "iss_position", "latitude" ] Json.string)
        (Json.at [ "iss_position", "longitude" ] Json.string)
