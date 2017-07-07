module Pages.PeopleInSpace exposing (..)

import Json.Decode as Json
import Http
import Html exposing (Html, div, a, text, button, p)


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { people : List Person }


type alias Person =
    { name : String
    , craft : String
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
                [ text "loading the people in space data" ]



--------------------------------------------------------------------------------
-- Data Fetching
--------------------------------------------------------------------------------


peopleInSpaceUrl : String
peopleInSpaceUrl =
    "http://api.open-notify.org/astros.json"


request : Http.Request Model
request =
    Http.get peopleInSpaceUrl decoder


sendRequest : (Result Http.Error Model -> msg) -> Cmd msg
sendRequest tagger =
    Http.send tagger request


decoder : Json.Decoder Model
decoder =
    let
        personDecoder =
            Json.map2 Person
                (Json.field "name" Json.string)
                (Json.field "craft" Json.string)
    in
        Json.map Model
            (Json.field "people" (Json.list personDecoder))
