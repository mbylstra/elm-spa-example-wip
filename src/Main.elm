module Main exposing (..)

import AddressBar exposing (getUrl, parseLocation)
import Html exposing (Html, a, button, div, p, text)
import Html.Events exposing (onClick)
import Navigation exposing (newUrl)
import Pages.IssNow as IssNow
import Pages.PeopleInSpace as PeopleInSpace
import Taco exposing (Taco)
import Types exposing (Page(..))
import LoadingStatusBar


--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------


main : Program Never Model Msg
main =
    Navigation.program
        UrlChanged
        { init = init
        , update = update
        , view = view
        , subscriptions = (\model -> Sub.none)
        }



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { taco : Taco -- every SPA needs a taco. Yum!

    -- page state
    , issNow : IssNow.Model
    , peopleInSpace : PeopleInSpace.Model

    -- top level "private" fields -- Don't pay too much attention to these yet! Pages should not need to know about them.
    , linkClicked : Bool
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        taco =
            Taco.init location

        model =
            { taco = taco

            -- page state
            , issNow = IssNow.initModel
            , peopleInSpace = PeopleInSpace.initModel

            -- top level "private" fields
            , linkClicked = False
            }

        cmd =
            case taco.currentPage of
                PeopleInSpacePage ->
                    Cmd.map PeopleInSpaceMsg PeopleInSpace.initCmd

                IssNowPage ->
                    Cmd.map IssNowMsg IssNow.initCmd

                NotFound ->
                    Cmd.none
    in
        model ! [ cmd ]



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = UrlChanged Navigation.Location
    | RouteChangeRequested Page
    | PeopleInSpaceMsg PeopleInSpace.Msg
    | IssNowMsg IssNow.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        UrlChanged location ->
            handleUrlChanged location model

        RouteChangeRequested page ->
            handleRouteChangeRequested page model

        PeopleInSpaceMsg pisMsg ->
            { model | peopleInSpace = PeopleInSpace.update pisMsg model.peopleInSpace } ! []

        IssNowMsg issNowMsg ->
            { model | issNow = IssNow.update issNowMsg model.issNow } ! []


handleUrlChanged : Navigation.Location -> Model -> ( Model, Cmd Msg )
handleUrlChanged location model =
    if model.linkClicked == True then
        -- If we got a UrlChanged msg via a link click in our app, then we are not interested. We  are
        -- handling the state of the app. The Address bar merely reflects the current state of the app
        -- for informational and bookmarking purposes. (It is not the source of truth for state)
        -- Reset linkClicked to False so we can detect
        { model | linkClicked = False } ! []
    else
        -- Seeing as we are using HTML5 history for clean urls, and we know that
        -- a link wasn't clicked, we can deduce that the back or forward button
        -- was clicked. How do we get the state? Seeing as the Elm Navigation does
        -- not support serializing state when calling pushState (it should, but it doesn't)
        -- and we haven't implementation our own custom history API in elm (this would be possible),
        -- all we have to work with is the URL and to deduce as much state as we
        -- can from the URL. Depending on how much state we choose to store
        -- in the url we may not be able to bring the page to exactly the
        -- same state the page was in.
        (model
            |> updatePage (parseLocation location)
        )
            ! []


handleRouteChangeRequested : Page -> Model -> ( Model, Cmd Msg )
handleRouteChangeRequested page model =
    let
        newUrlCmd =
            newUrl (getUrl page)

        reloading =
            page == model.taco.currentPage

        ( model2, cmd ) =
            case page of
                PeopleInSpacePage ->
                    let
                        f =
                            if reloading then
                                PeopleInSpace.reload
                            else
                                PeopleInSpace.navigateTo

                        ( newPeopleInSpace, pisMsg ) =
                            f model.peopleInSpace
                    in
                        { model | peopleInSpace = newPeopleInSpace } ! [ Cmd.map PeopleInSpaceMsg pisMsg ]

                IssNowPage ->
                    let
                        f =
                            if reloading then
                                IssNow.reload
                            else
                                IssNow.navigateTo

                        ( newIssNow, issNowMsg ) =
                            f model.issNow
                    in
                        { model | issNow = newIssNow } ! [ Cmd.map IssNowMsg issNowMsg ]

                NotFound ->
                    model ! [ Cmd.none ]
    in
        (model2
            |> updatePage page
            |> setLinkClicked True
        )
            ! [ newUrlCmd, cmd ]



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div
        []
        [ loadingBar model
        , topNav
        , pageView model
        ]


topNav : Html Msg
topNav =
    div []
        [ button [ onClick <| RouteChangeRequested IssNowPage ] [ text "Home Page" ]
        , button [ onClick <| RouteChangeRequested PeopleInSpacePage ] [ text "People In Space Page" ]
        ]


loadingBar : Model -> Html Msg
loadingBar model =
    case model.taco.currentPage of
        PeopleInSpacePage ->
            LoadingStatusBar.view PeopleInSpace.getLoadingStatus model.peopleInSpace

        IssNowPage ->
            LoadingStatusBar.view IssNow.getLoadingStatus model.issNow

        NotFound ->
            text "Not Found"


pageView : Model -> Html Msg
pageView model =
    let
        taco =
            model.taco
    in
        case taco.currentPage of
            IssNowPage ->
                IssNow.view model.issNow

            PeopleInSpacePage ->
                PeopleInSpace.view model.peopleInSpace

            NotFound ->
                div [] [ text "Not Found" ]



--------------------------------------------------------------------------------
-- Model Setter Boilerplate
-- Hopefully one day there will be an Elm syntax for this :)
--------------------------------------------------------------------------------


updatePage : Page -> Model -> Model
updatePage page model =
    let
        taco =
            model.taco
    in
        { model | taco = Taco.updatePage page taco }


setLinkClicked : Bool -> Model -> Model
setLinkClicked b model =
    { model | linkClicked = b }
