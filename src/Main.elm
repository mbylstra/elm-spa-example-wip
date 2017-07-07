module Main exposing (..)

import Navigation exposing (newUrl)
import Html exposing (Html, div, a, text, button, p)
import Html.Events exposing (onClick)
import UrlParser
import Http
import Pages.PeopleInSpace
import Pages.IssNow


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
-- Types
--------------------------------------------------------------------------------


type Page
    = IssNowPage
      -- | PassoverPage
    | PeopleInSpacePage
    | NotFound


type PageStatus
    = LoadingFirstPage
    | LoadingNextPageData
      -- you can choose to keep displaying currentPage, show next page skeleton with no data, or a blank loading screen. It's up to you!
      -- | Transitioning
      -- Here you can do something fancy with transition animations. You have data for both pages, and views for both pages, so it's not that hard to do!
    | Reloading
      -- Here you might want to show a spinner somewhere
    | Loaded
    | DataFetchError String



--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------


type alias Model =
    { location : Navigation.Location -- we don't really need to keep this around, but it's useful for debugging
    , linkClicked : Bool

    -- which page should we be showing?
    , currentPage : Page
    , previousPage : Maybe Page
    , nextPage : Maybe Page
    , pageStatus : PageStatus

    -- consider putting this in a dict, so that it's less error prone clearing all the caches. But then you lose type safety!
    -- But the worst case scenario is that old data is shown while the new one is loading (just a bit clunkly, but not a catastrophe)
    -- page data
    -- You can't make it a dict, because the value alwyas has to be the same type!
    , maybeIssNowPageData : Maybe Pages.IssNow.Model
    , maybePeopleInSpacePageData : Maybe Pages.PeopleInSpace.Model
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { location = location
    , linkClicked = False
    , currentPage = parseLocation location
    , previousPage = Nothing
    , nextPage = Nothing
    , pageStatus = LoadingFirstPage
    , maybeIssNowPageData = Nothing

    -- , passoverPage = Unvisited
    , maybePeopleInSpacePageData = Nothing
    }
        ! [ Pages.IssNow.sendRequest IssDataFetched ]


setLocation : Navigation.Location -> Model -> Model
setLocation location model =
    { model | location = location }


setCurrentPage : Page -> Model -> Model
setCurrentPage page model =
    { model | currentPage = page }


clearPageCaches : Model -> Model
clearPageCaches model =
    { model
        | maybeIssNowPageData = Nothing
        , maybePeopleInSpacePageData = Nothing
    }



--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------


type Msg
    = UrlChanged Navigation.Location
    | RouteChangeRequested Page
    | IssDataFetched (Result Http.Error Pages.IssNow.Model)
    | PeopleInSpaceDataFetched (Result Http.Error Pages.PeopleInSpace.Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "msg" msg) of
        UrlChanged location ->
            if model.linkClicked then
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
                { model | currentPage = parseLocation location, location = location }
                    ! []

        -- model ! []
        RouteChangeRequested page ->
            let
                newUrlCmd =
                    newUrl (getUrl page)

                status =
                    if page == model.currentPage then
                        Reloading
                    else
                        LoadingNextPageData

                dataFetchCmds =
                    case page of
                        -- PassoverPage ->
                        --     [ sendPassoverRequest ]
                        PeopleInSpacePage ->
                            [ Pages.PeopleInSpace.sendRequest PeopleInSpaceDataFetched ]

                        IssNowPage ->
                            [ Pages.IssNow.sendRequest IssDataFetched ]

                        NotFound ->
                            []
            in
                { model | linkClicked = True, currentPage = page, pageStatus = status } ! (newUrlCmd :: dataFetchCmds)

        IssDataFetched result ->
            case result of
                Ok homePageData ->
                    { model | maybeIssNowPageData = Just homePageData, pageStatus = Loaded } ! []

                Err httpError ->
                    { model | pageStatus = DataFetchError <| toString httpError } ! []

        PeopleInSpaceDataFetched result ->
            case result of
                Ok data ->
                    { model | maybePeopleInSpacePageData = Just data, pageStatus = Loaded } ! []

                Err httpError ->
                    { model | pageStatus = DataFetchError <| toString httpError } ! []



--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div
        []
        [ statusBar model.pageStatus
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
        useCachedData =
            figureOutWhetherToUseCachedData model.pageStatus
    in
        case model.currentPage of
            IssNowPage ->
                case useCachedData of
                    True ->
                        Pages.IssNow.view model.maybeIssNowPageData

                    False ->
                        Pages.IssNow.view Nothing

            PeopleInSpacePage ->
                case useCachedData of
                    True ->
                        Pages.PeopleInSpace.view model.maybePeopleInSpacePageData

                    False ->
                        Pages.PeopleInSpace.view Nothing

            NotFound ->
                div [] [ text "Not Found" ]



--------------------------------------------------------------------------------
-- SPA Routing
--------------------------------------------------------------------------------


getUrl : Page -> String
getUrl page =
    case page of
        IssNowPage ->
            "/international-space-station-current-location"

        PeopleInSpacePage ->
            "/people-in-space"

        NotFound ->
            "/not-found"


parseLocation : Navigation.Location -> Page
parseLocation location =
    let
        parser : UrlParser.Parser (Page -> a) a
        parser =
            UrlParser.oneOf
                [ UrlParser.top |> UrlParser.map IssNowPage
                , UrlParser.s "international-space-station-current-location" |> UrlParser.map IssNowPage
                , UrlParser.s "people-in-space" |> UrlParser.map PeopleInSpacePage
                ]
    in
        location
            |> UrlParser.parsePath parser
            |> Maybe.withDefault NotFound


figureOutWhetherToUseCachedData : PageStatus -> Bool
figureOutWhetherToUseCachedData pageStatus =
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
