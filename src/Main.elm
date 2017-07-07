module Main exposing (..)

import Navigation exposing (newUrl)
import Html exposing (Html, div, a, text, button, p)
import Html.Events exposing (onClick)
import UrlParser
import Json.Decode as Json
import Http
import PeopleInSpacePage


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


type Page
    = HomePage
      -- | PassoverPage
    | PeopleInSpacePage
    | NotFound


type PageStatus
    = LoadingFirstPage
    | LoadingNextPageData
      -- you can choose to keep displaying currentPage, show next page skeleton with no data, or a blank loading screen. It's up to you!
    | Transitioning
      -- Here you can do something fancy with transition animations. You have data for both pages, and views for both pages, so it's not that hard to do!
    | Reloading
      -- Here you might want to show a spinner somewhere
    | Loaded
    | DataFetchError String


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
    , maybeHomePageData : Maybe HomePageModel

    -- , passoverPage : RemoteData PassoverPageModel
    , maybePeopleInSpacePageData : Maybe PeopleInSpacePage.Model
    }


type alias HomePageModel =
    { timestamp : Int }


type Msg
    = UrlChanged Navigation.Location
    | RouteChangeRequested Page
    | IssDataFetched (Result Http.Error HomePageModel)
      -- | PassoverDataFetched (Result Http.Error PassoverPageModel)
    | PeopleInSpaceDataFetched (Result Http.Error PeopleInSpacePage.Model)



-- type alias PassoverPageModel =
--     { passoverTimes : List PassoverTime }
-- type alias PassoverTime =
--     { duration : Int, riseTime : Int }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { location = location
    , linkClicked = False
    , currentPage = parseLocation location
    , previousPage = Nothing
    , nextPage = Nothing
    , pageStatus = LoadingFirstPage
    , maybeHomePageData = Nothing

    -- , passoverPage = Unvisited
    , maybePeopleInSpacePageData = Nothing
    }
        ! [ sendIssNowRequest ]


setLocation : Navigation.Location -> Model -> Model
setLocation location model =
    { model | location = location }


setCurrentPage : Page -> Model -> Model
setCurrentPage page model =
    { model | currentPage = page }


clearPageCaches : Model -> Model
clearPageCaches model =
    { model
        | maybeHomePageData = Nothing
        , maybePeopleInSpacePageData = Nothing
    }


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
                --         HomePage ->
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
                            [ PeopleInSpacePage.sendRequest PeopleInSpaceDataFetched ]

                        HomePage ->
                            [ sendIssNowRequest ]

                        NotFound ->
                            []
            in
                { model | linkClicked = True, currentPage = page, pageStatus = status } ! (newUrlCmd :: dataFetchCmds)

        IssDataFetched result ->
            case result of
                Ok homePageData ->
                    { model | maybeHomePageData = Just homePageData, pageStatus = Loaded } ! []

                Err httpError ->
                    { model | pageStatus = DataFetchError <| toString httpError } ! []

        -- PassoverDataFetched result ->
        --     case result of
        --         Ok data ->
        --             { model | passoverPage = Loaded data } ! []
        --
        --         Err httpError ->
        --             { model | passoverPage = RemoteDataError <| toString httpError } ! []
        PeopleInSpaceDataFetched result ->
            case result of
                Ok data ->
                    { model | maybePeopleInSpacePageData = Just data, pageStatus = Loaded } ! []

                Err httpError ->
                    { model | pageStatus = DataFetchError <| toString httpError } ! []



-- etc


view : Model -> Html Msg
view model =
    let
        useCachedData =
            figureOutWhetherToUseCachedData model.pageStatus

        pageView =
            case model.currentPage of
                HomePage ->
                    case useCachedData of
                        True ->
                            homePageView model.maybeHomePageData

                        False ->
                            homePageView Nothing

                PeopleInSpacePage ->
                    case useCachedData of
                        True ->
                            PeopleInSpacePage.view model.maybePeopleInSpacePageData

                        False ->
                            PeopleInSpacePage.view Nothing

                NotFound ->
                    div [] [ text "Not Found" ]
    in
        div
            []
            [ statusBar model.pageStatus
            , button [ onClick <| RouteChangeRequested HomePage ] [ text "Home Page" ]

            -- , button [ onClick <| RouteChangeRequested PassoverPage ] [ text "Passover Page" ]
            , button [ onClick <| RouteChangeRequested PeopleInSpacePage ] [ text "People In Space Page" ]
            , pageView

            -- , text <| toString model
            ]



-- what if pageWrapper is a function that returns a function?


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

        Transitioning ->
            div []
                []

        Loaded ->
            div []
                []

        DataFetchError error ->
            div []
                [ text <| toString error ]


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

        Transitioning ->
            False

        Loaded ->
            True

        DataFetchError error ->
            True


homePageView : Maybe HomePageModel -> Html Msg
homePageView maybeModel =
    case maybeModel of
        Just model ->
            div []
                [ text <| toString model ]

        Nothing ->
            div []
                [ text "loading current ISS position" ]


main : Program Never Model Msg
main =
    Navigation.program
        UrlChanged
        { init = init
        , update = update
        , view = view
        , subscriptions = (\model -> Sub.none)
        }


getUrl : Page -> String
getUrl page =
    case page of
        HomePage ->
            "/home-page"

        -- PassoverPage ->
        --     "#/passoverPage"
        PeopleInSpacePage ->
            "/people-in-space"

        NotFound ->
            "/not-found"


pageParser : UrlParser.Parser (Page -> a) a
pageParser =
    UrlParser.oneOf
        [ UrlParser.top |> UrlParser.map HomePage
        , UrlParser.s "home-page" |> UrlParser.map HomePage

        -- , UrlParser.s "passoverPage" |> UrlParser.map PassoverPage
        , UrlParser.s "people-in-space" |> UrlParser.map PeopleInSpacePage
        ]


parseLocation : Navigation.Location -> Page
parseLocation location =
    location
        |> UrlParser.parsePath pageParser
        |> Maybe.withDefault NotFound


timestampDecoder : Json.Decoder Int
timestampDecoder =
    Json.field "timestamp" Json.int


homePageDecoder : Json.Decoder HomePageModel
homePageDecoder =
    Json.map HomePageModel timestampDecoder


internationalSpaceStationNowUrl : String
internationalSpaceStationNowUrl =
    "http://api.open-notify.org/iss-now.json"


issNowRequest : Http.Request HomePageModel
issNowRequest =
    Http.get internationalSpaceStationNowUrl homePageDecoder


sendIssNowRequest : Cmd Msg
sendIssNowRequest =
    Http.send IssDataFetched
        issNowRequest



-- This particular url doesn't work cos of CORS :(
-- cultureampIssPassoverUrl : String
-- cultureampIssPassoverUrl =
--     "http://api.open-notify.org/iss-pass.json?lat=-37.818203&lon=-144.96210"
--
--
-- passoverPageDecoder : Json.Decoder PassoverPageModel
-- passoverPageDecoder =
--     let
--         passoverTimeDecoder =
--             Json.map2 PassoverTime
--                 (Json.field "duration" Json.int)
--                 (Json.field "risetime" Json.int)
--     in
--         Json.map PassoverPageModel
--             (Json.list passoverTimeDecoder)
--
--
-- passoverRequest : Http.Request PassoverPageModel
-- passoverRequest =
--     Http.get cultureampIssPassoverUrl passoverPageDecoder
--
--
-- sendPassoverRequest : Cmd Msg
-- sendPassoverRequest =
--     Http.send PassoverDataFetched passoverRequest
-- "http://api.open-ntify.org/astros.json"
-- {
-- "timestamp": 1499248894,
-- "message": "success",
-- "iss_position": {
--     "longitude": "-163.3590",
--     "latitude": "-37.1211"
--     }
-- }
