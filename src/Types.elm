module Types exposing (..)

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
