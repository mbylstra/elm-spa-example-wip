module Pages.IssNow exposing (..)

import Http
import Json.Decode as Json
import Html exposing (Html, div, a, text, button, p)
import Types exposing (PageLoadingStatus(..))
import Maps exposing (defaultOptions)
import Maps.LatLng exposing (LatLng)


mapsDefaultOptions : Maps.Options
mapsDefaultOptions =
    Maps.defaultOptions



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { maybeLocation : Maybe LatLng
    , loadingStatus : PageLoadingStatus
    , map : Maps.Model
    }


type alias Location =
    { lat : Float
    , lng : Float
    }


mapFuncs :
    { subscriptions : Maps.Model -> Sub Maps.Msg
    , update : Maps.Msg -> Maps.Model -> ( Maps.Model, Cmd Maps.Msg )
    , view : Maps.Model -> Html Maps.Msg
    , init : ( Maps.Model, Cmd Maps.Msg )
    }
mapFuncs =
    Maps.map
        { mapsDefaultOptions
            | height = 500
            , width = 500
        }


initModel : ( Model, Cmd Msg )
initModel =
    let
        ( map, mapMsg ) =
            mapFuncs.init
    in
        { maybeLocation = Nothing
        , loadingStatus = Loading
        , map = map
        }
            ! [ Cmd.map MapMsg mapMsg ]


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
    | MapMsg Maps.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataFetched result ->
            case result of
                Ok location ->
                    let
                        map =
                            model.map

                        innerMap =
                            map.map

                        newMap =
                            { map | map = { innerMap | center = location, zoom = 8.0 } }
                    in
                        { model | maybeLocation = Just location, loadingStatus = Loaded, map = newMap } ! []

                Err err ->
                    { model | loadingStatus = DataFetchError <| toString err } ! []

        MapMsg mapMsg ->
            Maps.update mapMsg model.map
                |> Tuple.mapFirst (\map -> { model | map = map })
                |> Tuple.mapSecond (Cmd.map MapMsg)


navigateTo : Model -> ( Model, Cmd Msg )
navigateTo model =
    { model | loadingStatus = Loading } ! [ sendRequest ]


reload : Model -> ( Model, Cmd Msg )
reload model =
    { model | loadingStatus = Reloading } ! [ sendRequest ]



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    case model.maybeLocation of
        Just location ->
            div []
                [ text <| toString location
                , mapFuncs.view model.map |> Html.map MapMsg
                ]

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
        (Json.at [ "iss_position", "latitude" ] Json.string
            |> Json.map (String.toFloat >> Result.withDefault 0)
        )
        (Json.at [ "iss_position", "longitude" ] Json.string
            |> Json.map (String.toFloat >> Result.withDefault 0)
        )
