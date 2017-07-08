module Main exposing (..)

import AddressBar exposing (getUrl, parseLocation)
import Html exposing (Html, a, button, div, p, text)
import Html.Events exposing (onClick)
import Http
import Navigation exposing (newUrl)
import Pages.IssNow as IssNow
import Pages.PeopleInSpace as PeopleInSpace
import Taco exposing (Taco)
import Types exposing (Page(..), PageStatus(..))


-- Notes: should the remote data stuff really be inside each one of the models?
-- there's quite a bit of overlap between Unvisited (so what), Loading, Loaded,
-- Updating and Error. However, it could be useful if you wanted to fetch
-- multiple pages at once (prefetch)
-- Also, updating is not longer relevant
-- We need to distinguish between transitioning from and reloading a page.
-- When transitioning to we don't want to show the previous page (not sure about this).
-- When reloading, we want to continue to show the current page.
-- It's not even clear if we want to even ever bother reloading?
-- the sensible default would be to reload! Because you can easily get caching bugs.
-- So in this case, let's just clear all caches when fetching a new page.
-- TRANSITIONS
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
    , maybeIssNowPageData : Maybe IssNow.Model
    , peopleInSpace : PeopleInSpace.Model

    -- top level "private" fields -- Don't pay too much attention to these yet! Pages should not need to know about them.
    -- , location : Navigation.Location -- we don't really need to keep this around, but it's useful for debugging
    , linkClicked : Bool
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { taco = Taco.init location

    -- page state
    , maybeIssNowPageData = Nothing
    , peopleInSpace = PeopleInSpace.init

    -- top level "private" fields
    , linkClicked = False
    }
        ! [ IssNow.sendRequest IssDataFetched ]



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = UrlChanged Navigation.Location
    | RouteChangeRequested Page
    | IssDataFetched (Result Http.Error IssNow.Model)
    | PeopleInSpaceDataFetched (Result Http.Error PeopleInSpace.People)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        UrlChanged location ->
            if model.linkClicked then
                -- If we got a UrlChanged msg via a link click in our app, then we are not interested. We  are
                -- handling the state of the app. The Address bar merely reflects the current state of the app
                -- for informational and bookmarking purposes. (It is not the source of truth for state)
                { model | linkClicked = False } ! []
            else
                -- cmds =
                --     case page of
                --         -- PassoverPage ->
                --         --     [ sendPassoverRequest ]
                --         PeopleInSpacePage ->
                --             []
                --
                --         IssNowPage ->
                --             []
                --
                --         NotFound ->
                --             []
                -- We don't care!
                -- Do we need to re-initialise the model then? And refetch stuff
                -- we need to refetch? This doesn't seem right.
                -- This is a bit of a manual process really.
                (model
                    |> updatePage (parseLocation location)
                    |> setLinkClicked False
                )
                    ! []

        -- model ! []
        RouteChangeRequested page ->
            let
                newUrlCmd =
                    newUrl (getUrl page)

                dataFetchCmd =
                    case page of
                        -- PassoverPage ->
                        --     [ sendPassoverRequest ]
                        PeopleInSpacePage ->
                            PeopleInSpace.sendRequest PeopleInSpaceDataFetched

                        IssNowPage ->
                            IssNow.sendRequest IssDataFetched

                        NotFound ->
                            Cmd.none

                taco =
                    model.taco
            in
                (model
                    |> updatePage page
                    |> setLinkClicked True
                )
                    ! [ newUrlCmd, dataFetchCmd ]

        IssDataFetched result ->
            case result of
                Ok data ->
                    (model
                        |> setPageStatus Loaded
                        |> setMaybeIssNowPageData (Just data)
                    )
                        ! []

                Err httpError ->
                    (model
                        |> setPageStatus (DataFetchError <| toString httpError)
                    )
                        ! []

        PeopleInSpaceDataFetched result ->
            case result of
                Ok peopleData ->
                    (model
                        |> setPageStatus Loaded
                        |> setPeopleInSpacePeople peopleData
                    )
                        ! []

                Err httpError ->
                    (model
                        |> setPageStatus (DataFetchError <| toString httpError)
                    )
                        ! []



-- case result of
--     Ok peopleData ->
--         let
--             _ =
--                 ""
--
--             -- newPeople =
--             --     Just people
--             -- newPeopl
--         in
--             -- The interesting thing here is that we want to keep the current local state,
--             -- so wrapping it in a maybe doesn't work! The data itself needs to be in a maybe.
--             { model | maybePeopleInSpacePageData = Just peopleData, pageStatus = Loaded } ! []
--
--     Err httpError ->
--         { model | pageStatus = DataFetchError <| toString httpError } ! []
--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div
        []
        [ statusBar model.taco.pageStatus
        , topNav
        , pageView model

        -- , text <| toString model
        ]


topNav : Html Msg
topNav =
    div []
        [ button [ onClick <| RouteChangeRequested IssNowPage ] [ text "Home Page" ]
        , button [ onClick <| RouteChangeRequested PeopleInSpacePage ] [ text "People In Space Page" ]
        ]


statusBar : PageStatus -> Html Msg
statusBar pageStatus =
    case pageStatus of
        LoadingFirstPage ->
            div []
                [ text "loading first page"
                ]

        -- can we pass the view and the data, and choose whether to pass in the
        -- data or just nothing?
        LoadingNextPageData ->
            div []
                [ text "loading new page"
                ]

        Reloading ->
            div []
                [ text "reloading"
                ]

        -- Transitioning ->
        --     div []
        --         []
        Loaded ->
            div []
                []

        DataFetchError error ->
            div []
                [ text <| toString error ]


pageView : Model -> Html Msg
pageView model =
    let
        taco =
            model.taco

        showCachedData =
            figureOutWhetherToShowCachedData taco.pageStatus
    in
        case taco.currentPage of
            IssNowPage ->
                case showCachedData of
                    True ->
                        IssNow.view model.maybeIssNowPageData

                    False ->
                        IssNow.view Nothing

            PeopleInSpacePage ->
                case showCachedData of
                    True ->
                        PeopleInSpace.view model.peopleInSpace

                    False ->
                        PeopleInSpace.view model.peopleInSpace

            -- False ->
            --     PeopleInSpace.view Nothing
            NotFound ->
                div [] [ text "Not Found" ]



--------------------------------------------------------------------------------
-- SPA Routing
--------------------------------------------------------------------------------


figureOutWhetherToShowCachedData : PageStatus -> Bool
figureOutWhetherToShowCachedData pageStatus =
    case pageStatus of
        LoadingFirstPage ->
            False

        -- can we pass the view and the data, and choose whether to pass in the
        -- data or just nothing?
        LoadingNextPageData ->
            False

        Reloading ->
            True

        -- Transitioning ->
        --     False
        Loaded ->
            True

        DataFetchError error ->
            True



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


setPageStatus : PageStatus -> Model -> Model
setPageStatus pageStatus model =
    let
        taco =
            model.taco
    in
        { model | taco = { taco | pageStatus = pageStatus } }


setMaybeIssNowPageData : Maybe IssNow.Model -> Model -> Model
setMaybeIssNowPageData v m =
    { m | maybeIssNowPageData = v }


setPeopleInSpacePeople : PeopleInSpace.People -> Model -> Model
setPeopleInSpacePeople people model =
    { model | peopleInSpace = PeopleInSpace.setPeople people model.peopleInSpace }


setPeopleInSpace : PeopleInSpace.Model -> Model -> Model
setPeopleInSpace v m =
    { m | peopleInSpace = v }
