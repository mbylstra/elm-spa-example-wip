module Pages.PeopleInSpace exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, a, button, div, dt, p, text, dl, dt, dd, textarea)
import Http
import Json.Decode as Json


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { maybePeople : Maybe People

    -- A list of Work In Progress messages to send to the maybePeople
    , messages : Dict PersonId Message
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


init : Model
init =
    { maybePeople = Nothing
    , messages = Dict.empty
    }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


setPeople : People -> Model -> Model
setPeople people model =
    { model | maybePeople = Just people }



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html msg
view model =
    case model.maybePeople of
        Just people ->
            div []
                (people
                    |> Dict.toList
                    |> List.map
                        (\( personId, person ) ->
                            personView person (Dict.get personId model.messages)
                        )
                )

        Nothing ->
            div []
                [ text "loading the people in space data" ]


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


sendRequest : (Result Http.Error People -> msg) -> Cmd msg
sendRequest tagger =
    Http.send tagger request


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
