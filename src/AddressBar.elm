module AddressBar exposing (..)

import Navigation exposing (newUrl)
import UrlParser
import Types exposing (Page(..))


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
