module Main exposing (Model, main, view)

import Browser
import Browser.Events
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import List
import Task
import Time
import Tuple


type alias PilotsHeight =
    -- TODO: Probably using a Dict would make the chart faster. See pilotHeight
    List ( String, Float )


type alias TaskMoment =
    { t : Float
    , pilotsHeight : PilotsHeight
    }


type alias Model =
    { debugMsg : String
    , timeZone : Time.Zone
    , pilotNames : List String
    , taskMoments : List TaskMoment
    , taskMomentsStats :
        { domainMin : Float -- Domain = values (vertical axis)
        , domainMax : Float
        , rangeMin : Float -- Range = time (horizontal axis)
        , rangeMax : Float
        , rangeCount : Int
        , rangeStep : Float
        }
    , displayedTaskMoments : Float
    , animating : Bool
    , animationSpeedFactor : Int
    , hovering : Bool
    , hoveringOverPoint : CE.Point
    }


type Msg
    = GotTimeZone Time.Zone
    | PlayClicked
    | SliderMoved String
    | SpeedChanged String
    | AnimationFrameTicked Float
    | MouseMoved CE.Point
    | MouseLeft


pilotHeight : String -> PilotsHeight -> Float
pilotHeight pilotName pilotsHeight =
    List.filter (\nameAndHeight -> Tuple.first nameAndHeight == pilotName) pilotsHeight
        |> List.head
        |> Maybe.withDefault ( "", 0 )
        |> Tuple.second


init : () -> ( Model, Cmd Msg )
init _ =
    let
        taskMoments =
            [ TaskMoment 1645899285000 [ ( "one", 1250 ), ( "two", 1250 ) ]
            , TaskMoment 1645899286000 [ ( "one", 1270 ), ( "two", 1260 ) ]
            , TaskMoment 1645899287000 [ ( "one", 1290 ), ( "two", 1280 ) ]
            , TaskMoment 1645899288000 [ ( "one", 1300 ), ( "two", 1300 ) ]
            , TaskMoment 1645899289000 [ ( "one", 1310 ), ( "two", 1350 ) ]
            , TaskMoment 1645899290000 [ ( "one", 1300 ), ( "two", 1250 ) ]
            , TaskMoment 1645899291000 [ ( "one", 1200 ), ( "two", 1200 ) ]
            ]

        domainMin =
            List.map
                (\tm -> List.map Tuple.second tm.pilotsHeight |> List.minimum |> Maybe.withDefault 0)
                taskMoments
                |> List.minimum
                |> Maybe.withDefault 0

        domainMax =
            List.map
                (\tm -> List.map Tuple.second tm.pilotsHeight |> List.maximum |> Maybe.withDefault 0)
                taskMoments
                |> List.maximum
                |> Maybe.withDefault 0

        rangeMin =
            List.head taskMoments
                |> Maybe.withDefault (TaskMoment 0 [])
                |> .t

        rangeMax =
            List.reverse taskMoments
                |> List.head
                |> Maybe.withDefault (TaskMoment 0 [])
                |> .t

        rangeCount =
            List.length taskMoments

        rangeStep =
            (rangeMax - rangeMin) / toFloat rangeCount
    in
    ( { debugMsg = ""
      , timeZone = Time.utc
      , pilotNames = [ "one", "two" ]
      , taskMoments = taskMoments
      , taskMomentsStats =
            { domainMin = domainMin
            , domainMax = domainMax
            , rangeMin = rangeMin
            , rangeMax = rangeMax
            , rangeCount = rangeCount
            , rangeStep = rangeStep
            }
      , displayedTaskMoments = toFloat rangeCount
      , animating = False
      , animationSpeedFactor = 8
      , hovering = False
      , hoveringOverPoint = CE.Point 0 0
      }
    , Task.perform GotTimeZone Time.here
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.animating then
        Browser.Events.onAnimationFrameDelta AnimationFrameTicked

    else
        Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.none )

        PlayClicked ->
            let
                newDisplayedTaskMoments =
                    if model.displayedTaskMoments >= toFloat model.taskMomentsStats.rangeCount then
                        1

                    else
                        model.displayedTaskMoments
            in
            ( { model
                | displayedTaskMoments = newDisplayedTaskMoments
                , animating = True
              }
            , Cmd.none
            )

        SliderMoved strValue ->
            ( { model | displayedTaskMoments = String.toFloat strValue |> Maybe.withDefault 0 }, Cmd.none )

        SpeedChanged strValue ->
            ( { model | animationSpeedFactor = String.toInt strValue |> Maybe.withDefault 0 }, Cmd.none )

        AnimationFrameTicked millisSinceLast ->
            let
                momentsSinceLast =
                    millisSinceLast / model.taskMomentsStats.rangeStep

                newDisplayedTaskMoments =
                    model.displayedTaskMoments
                        + (momentsSinceLast * toFloat model.animationSpeedFactor)

                animating =
                    if model.animating then
                        truncate newDisplayedTaskMoments < model.taskMomentsStats.rangeCount

                    else
                        False
            in
            ( { model
                | displayedTaskMoments = newDisplayedTaskMoments
                , animating = animating
              }
            , Cmd.none
            )

        MouseMoved point ->
            ( { model
                | hovering = True
                , hoveringOverPoint = point
              }
            , Cmd.none
            )

        MouseLeft ->
            ( { model | hovering = False }, Cmd.none )


view : Model -> H.Html Msg
view model =
    let
        tmStats =
            model.taskMomentsStats
    in
    H.div []
        [ H.div [ HA.style "width" "600px", HA.style "height" "300px" ]
            [ C.chart
                [ CA.width 600
                , CA.height 300
                , CA.margin { top = 10, bottom = 30, left = 50, right = 10 }
                , CA.range [ CA.lowest tmStats.rangeMin CA.exactly, CA.highest tmStats.rangeMax CA.exactly ]
                , CA.domain [ CA.lowest tmStats.domainMin CA.exactly, CA.highest tmStats.domainMax CA.exactly ]
                , CE.onMouseMove MouseMoved CE.getCoords
                , CE.onMouseLeave MouseLeft
                ]
                [ C.xAxis []
                , C.xTicks []
                , C.xLabels [ CA.times model.timeZone, CA.amount 10 ]
                , C.yAxis []
                , C.yTicks []
                , C.yLabels [ CA.withGrid ]
                , C.withPlane <|
                    \p ->
                        if model.hovering then
                            [ C.line
                                [ CA.x1 model.hoveringOverPoint.x
                                , CA.y1 p.y.min
                                , CA.y2 p.y.max
                                , CA.dashed [ 5, 5 ]
                                , CA.color CA.blue
                                ]
                            ]

                        else
                            []
                , C.series
                    .t
                    (List.map
                        (\name ->
                            C.interpolated
                                (\taskMoment -> pilotHeight name taskMoment.pilotsHeight)
                                [ CA.linear, CA.width 2 ]
                                []
                                |> C.named name
                        )
                        model.pilotNames
                    )
                    (List.take (truncate model.displayedTaskMoments) model.taskMoments)
                , C.legendsAt
                    .min
                    .max
                    [ CA.column
                    , CA.moveRight 5
                    , CA.spacing 0
                    ]
                    [ CA.width 15 ]
                ]
            ]
        , H.div []
            [ H.button
                [ HA.type_ "button"
                , HE.onClick PlayClicked
                ]
                [ H.text "Play" ]
            , H.input
                [ HA.type_ "range"
                , HA.min "1"
                , HA.max <| String.fromInt <| model.taskMomentsStats.rangeCount
                , HA.value <| String.fromInt <| truncate model.displayedTaskMoments
                , HE.onInput SliderMoved
                , HA.style "width" "550px"
                ]
                []
            , H.select [ HE.onInput SpeedChanged ]
                [ H.option [ HA.value "2", HA.selected (model.animationSpeedFactor == 2) ] [ H.text "2x" ]
                , H.option [ HA.value "4", HA.selected (model.animationSpeedFactor == 4) ] [ H.text "4x" ]
                , H.option [ HA.value "8", HA.selected (model.animationSpeedFactor == 8) ] [ H.text "8x" ]
                , H.option [ HA.value "16", HA.selected (model.animationSpeedFactor == 16) ] [ H.text "16x" ]
                , H.option [ HA.value "32", HA.selected (model.animationSpeedFactor == 32) ] [ H.text "32x" ]
                ]
            ]

        -- TODO: remove
        , H.pre []
            [ H.text model.debugMsg
            , H.text <| String.fromFloat model.hoveringOverPoint.x
            ]
        ]


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }
