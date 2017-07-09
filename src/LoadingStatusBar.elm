module LoadingStatusBar exposing (..)

-- the idea is that the child decides what state to show
-- So, it's a function that takes any state, and returns whether it is loading or not
-- The child is responsible for keep the state of whether or not it is loading.

import Html exposing (Html, text)
import Types exposing (PageLoadingStatus(..))


view : (model -> PageLoadingStatus) -> model -> Html msg
view isLoadingFunction model =
    case isLoadingFunction model of
        Loading ->
            text "loading..."

        Reloading ->
            text "reloading..."

        Loaded ->
            text "loaded"

        DataFetchError message ->
            text message
