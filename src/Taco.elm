module Taco exposing (..)

import AddressBar exposing (parseLocation)
import Navigation
import Types exposing (..)


type alias Taco =
    { currentPage : Page
    , previousPage : Maybe Page
    , nextPage : Maybe Page
    }


init : Navigation.Location -> Taco
init location =
    { currentPage = parseLocation location
    , previousPage = Nothing
    , nextPage = Nothing
    }


updatePage : Page -> Taco -> Taco
updatePage page taco =
    { taco
        | currentPage = page
        , previousPage = Just taco.currentPage
        , nextPage = Nothing

        -- , pageStatus =
        --     if page == taco.currentPage then
        --         Reloading
        --     else
        --         LoadingNextPageData
    }
