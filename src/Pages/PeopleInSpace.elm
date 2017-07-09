module Pages.PeopleInSpace exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, a, button, dd, div, dl, dt, p, text, textarea)
import Http
import Json.Decode as Json
import Types exposing (PageLoadingStatus(..))


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { maybePeople : Maybe People

    -- A list of Work In Progress messages to send to the maybePeople
    , messages : Dict PersonId Message
    , loadingStatus : PageLoadingStatus
    }


type alias People =
    Dict PersonId Person


type alias PersonId =
    String


type alias Message =
    { content : String
    , status : MessageStatus
    }


type MessageStatus
    = WIP
    | Sent


type alias Person =
    { name : String
    , craft : String
    }


initModel : Model
initModel =
    { maybePeople = Nothing
    , messages = Dict.empty
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
    = DataFetched (Result Http.Error People)


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataFetched result ->
            case result of
                Ok people ->
                    { model | maybePeople = Just people, loadingStatus = Loaded }

                Err err ->
                    { model | loadingStatus = DataFetchError <| toString err }



-- setPeople : People -> Model -> Model
-- setPeople people model =
--     { model | maybePeople = Just people }


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
    case model.loadingStatus of
        Loading ->
            div [] []

        Reloading ->
            viewWithVisibleData model

        DataFetchError _ ->
            viewWithVisibleData model

        Loaded ->
            viewWithVisibleData model


viewWithVisibleData : Model -> Html msg
viewWithVisibleData model =
    case model.maybePeople of
        Just people ->
            loadedDataView people model

        Nothing ->
            div []
                [ text "loading the people in space data" ]


loadedDataView : People -> Model -> Html msg
loadedDataView people model =
    div []
        (people
            |> Dict.toList
            |> List.map
                (\( personId, person ) ->
                    personView person (Dict.get personId model.messages)
                )
        )


dlItem : String -> Html msg -> Html msg
dlItem term ddContent =
    div []
        [ dt [] [ text term ]
        , dd [] [ ddContent ]
        ]


personView : Person -> Maybe Message -> Html msg
personView person maybeMessage =
    div []
        [ dl []
            [ dlItem "name" (text person.name)
            , dlItem "craft" (text person.craft)
            , dlItem "your message" (messageView maybeMessage)
            ]
        ]


messageView : Maybe Message -> Html msg
messageView maybeMessage =
    case maybeMessage of
        Just message ->
            case message.status of
                WIP ->
                    textarea [] [ text message.content ]

                Sent ->
                    p
                        []
                        [ text message.content ]

        Nothing ->
            button [] [ text "write a message" ]



--------------------------------------------------------------------------------
-- Data Fetching
--------------------------------------------------------------------------------


maybePeopleInSpaceUrl : String
maybePeopleInSpaceUrl =
    "http://api.open-notify.org/astros.json"


request : Http.Request People
request =
    Http.get maybePeopleInSpaceUrl maybePeopleDecoder


sendRequest : Cmd Msg
sendRequest =
    Http.send DataFetched request


maybePeopleDecoder : Json.Decoder People
maybePeopleDecoder =
    let
        fieldsToPersonTuple : String -> String -> ( PersonId, Person )
        fieldsToPersonTuple name craft =
            ( name, { name = name, craft = craft } )

        personTupleDecoder =
            --  make a function that takes the two bits we need (name and craft)
            -- and turns that into what we want for the dict
            Json.map2 fieldsToPersonTuple
                (Json.field "name" Json.string)
                (Json.field "craft" Json.string)
    in
        (Json.field "people" (Json.list personTupleDecoder))
            |> Json.map Dict.fromList
