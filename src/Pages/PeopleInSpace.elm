module Pages.PeopleInSpace exposing (..)

import Dict exposing (Dict)
import Dom
import Html exposing (Html, a, button, dd, div, dl, dt, p, text, textarea)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
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
    | StartWritingMessage PersonId
    | UpdateMessage PersonId String
    | SendMessage PersonId


update : Msg -> Model -> Model
update msg model =
    case msg of
        DataFetched result ->
            case result of
                Ok people ->
                    { model | maybePeople = Just people, loadingStatus = Loaded }

                Err err ->
                    { model | loadingStatus = DataFetchError <| toString err }

        StartWritingMessage personId ->
            { model
                | messages =
                    Dict.insert personId { content = "", status = WIP } model.messages
            }

        UpdateMessage personId content ->
            { model
                | messages =
                    Dict.insert personId { content = content, status = WIP } model.messages
            }

        SendMessage personId ->
            { model
                | messages =
                    Dict.update
                        personId
                        (Maybe.map (\message -> { message | status = Sent }))
                        model.messages
            }



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


view : Model -> Html Msg
view model =
    case ( model.loadingStatus, model.maybePeople ) of
        ( Loaded, Just people ) ->
            loadedDataView people model

        ( Loaded, Nothing ) ->
            Debug.crash "This is an impossible state. There must be a bug!"

        ( Loading, Just _ ) ->
            div [] []

        ( Loading, Nothing ) ->
            div [] [ text "loading people.." ]

        ( Reloading, Just people ) ->
            loadedDataView people model

        ( Reloading, Nothing ) ->
            Debug.crash "This is an impossible state. There must be a bug!"

        ( DataFetchError message, Just people ) ->
            div []
                [ text "THere was an error getting the most recent version. Showing an older version of the data"
                , loadedDataView people model
                ]

        ( DataFetchError message, Nothing ) ->
            div [] []


loadedDataView : People -> Model -> Html Msg
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
    div [ class "dl-item" ]
        [ dt [] [ text term ]
        , dd [] [ ddContent ]
        ]


personView : Person -> Maybe Message -> Html Msg
personView person maybeMessage =
    div [ class "person" ]
        [ dl []
            [ dlItem "name" (text person.name)
            , dlItem "craft" (text person.craft)
            , dlItem "your message" (messageView person.name maybeMessage)
            ]
        ]


messageView : PersonId -> Maybe Message -> Html Msg
messageView personId maybeMessage =
    case maybeMessage of
        Just message ->
            case message.status of
                WIP ->
                    div []
                        [ textarea [ onInput <| UpdateMessage personId ] [ text message.content ]
                        , button [ onClick <| SendMessage personId ] [ text "send" ]
                        ]

                Sent ->
                    p
                        []
                        [ text message.content ]

        Nothing ->
            button [ onClick <| StartWritingMessage personId ] [ text "write a message" ]



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
