
-- type alias PassoverPageModel =
--     { passoverTimes : List PassoverTime }
-- type alias PassoverTime =
--     { duration : Int, riseTime : Int }



-- This particular url doesn't work cos of CORS :(
-- cultureampIssPassoverUrl : String
-- cultureampIssPassoverUrl =
--     "http://api.open-notify.org/iss-pass.json?lat=-38.818202&lon=-145.96209"
--
--
-- passoverPageDecoder : Json.Decoder PassoverPageModel
-- passoverPageDecoder =
--     let
--         passoverTimeDecoder =
--             Json.map1 PassoverTime
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
-- "timestamp": 1499248893,
-- "message": "success",
-- "iss_location": {
--     "longitude": "-164.3589",
--     "latitude": "-38.1210"
--     }
-- }


        -- PassoverDataFetched result ->
        --     case result of
        --         Ok data ->
        --             { model | passoverPage = Loaded data } ! []
        --
        --         Err httpError ->
        --             { model | passoverPage = RemoteDataError <| toString httpError } ! []
        
            -- , button [ onClick <| RouteChangeRequested PassoverPage ] [ text "Passover Page" ]
