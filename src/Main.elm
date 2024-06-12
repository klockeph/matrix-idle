module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Bars exposing (..)
import Browser
import Css
import Css.Global
import Html as OldHtml
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick)
import Random
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttr
import Svg.Styled.Events as SvgE
import Tailwind.Breakpoints as Breakpoints
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw
import Task
import Time


tickspeedPrice =
    10


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { coins : Int
    , tickspeed : Float
    , active_bar : Maybe Int
    , bars : List Int
    , algorithms : Dict AlgorithmType Algorithm
    , active_algorithm : Maybe AlgorithmType
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 1000 Nothing kBARS kAlgorithmDict Nothing, Cmd.none )


type Msg
    = BuyTickspeed
    | ClickBar Int
    | AddCoins Int
    | GenerateBars (List Int)
    | ChangeBarCount Int
    | BuyAlgorithm AlgorithmType
    | ActivateAlgorithm AlgorithmType
    | NoMsg
    | SwapBars Int Int
    | AlgorithmStep (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuyTickspeed ->
            ( { model | coins = model.coins - tickspeedPrice, tickspeed = model.tickspeed * 0.95 }, Cmd.none )

        ClickBar x ->
            clickBar model x

        AddCoins i ->
            ( { model | coins = model.coins + i }, Cmd.none )

        GenerateBars newBars ->
            if barsSorted newBars then
                ( model, generateRandomBars (List.length newBars) )

            else
                ( { model | bars = newBars }, Cmd.none )

        ChangeBarCount bar_count ->
            -- reuse the randomBars logic but use 0 as first argument for GenerateBars because no points shall be given.
            ( model, generateRandomBars bar_count )

        BuyAlgorithm a ->
            ( buyAlgorithm model a, Cmd.none )

        ActivateAlgorithm a ->
            ( activateAlgorithm model a, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )

        SwapBars b1 b2 ->
            let
                new_model =
                    { model | bars = swapBars model.bars b1 b2 }
            in
            ( new_model, checkSortedAndCmd new_model )

        AlgorithmStep newBars ->
            let
                new_model =
                    { model | bars = newBars }
            in
            ( new_model, checkSortedAndCmd new_model )



-- TODO: mark algorithm as bought and active
-- TODO: subtract  the money


buyAlgorithm : Model -> AlgorithmType -> Model
buyAlgorithm model algoT =
    case Dict.get algoT model.algorithms of
        -- shouldn't happen
        Nothing ->
            Debug.log "ERROR: Couldn't find algorithm" model

        Just algo ->
            let
                algos =
                    Dict.insert algoT { algo | unlocked = True } model.algorithms
            in
            { model | coins = model.coins - algo.price, algorithms = algos }


activateAlgorithm : Model -> AlgorithmType -> Model
activateAlgorithm model algoT =
    case Dict.get algoT model.algorithms of
        -- shouldn't happen
        Nothing ->
            Debug.log "ERROR: Couldn't find algorithm" model

        Just algo ->
            let
                now_active =
                    not algo.active

                all_inactive : Dict AlgorithmType Algorithm
                all_inactive =
                    Dict.map (\_ x -> { x | active = False }) model.algorithms

                algos =
                    Dict.insert algoT { algo | active = now_active } all_inactive
            in
            { model
                | algorithms = algos
                , active_algorithm =
                    if now_active then
                        Just algoT

                    else
                        Nothing
            }


generateRandomBars : Int -> Cmd Msg
generateRandomBars cnt =
    Random.generate GenerateBars (randomBars cnt)


checkSortedAndCmd : Model -> Cmd Msg
checkSortedAndCmd model =
    if barsSorted model.bars then
        let
            count =
                List.length model.bars
        in
        Cmd.batch
            [ generateRandomBars count
            , Task.perform (\_ -> AddCoins (count * 4)) (Task.succeed ())
            ]

    else
        Cmd.none


clickBar : Model -> Int -> ( Model, Cmd Msg )
clickBar model clicked =
    case model.active_bar of
        Nothing ->
            ( { model | active_bar = Just clicked }, Cmd.none )

        Just active_bar ->
            if active_bar == clicked then
                ( { model | active_bar = Nothing }, Cmd.none )

            else
                let
                    mi =
                        Basics.min active_bar clicked

                    ma =
                        Basics.max active_bar clicked
                in
                ( { model | active_bar = Nothing }, Task.perform (\_ -> SwapBars mi ma) (Task.succeed ()) )


splitSortedPart : List Int -> ( List Int, List Int )
splitSortedPart ls =
    let
        splitSortedPartI l s =
            case l of
                b1 :: b2 :: bs ->
                    if b1 <= b2 then
                        splitSortedPartI (b2 :: bs) (b1 :: s)

                    else
                        ( List.reverse (b1 :: s), b2 :: bs )

                [ b ] ->
                    ( List.reverse (b :: s), [] )

                _ ->
                    ( List.reverse s, l )
    in
    case ls of
        x :: xs ->
            splitSortedPartI xs [ x ]

        _ ->
            ( [], ls )



{-
   insertionSortFn : List Int -> Msg
   insertionSortFn l =
       let
           ( sorted, unsorted ) =
               splitSortedPart l
       in
       case unsorted of
           x :: xs ->
               let
                   newBars =
                       List.sort (x :: sorted) ++ xs
               in
               AlgorithmStep newBars

           _ ->
               NoMsg
-}


insertionSortFn : List Int -> Msg
insertionSortFn l =
    let
        insertionSortFnI idx b =
            case b of
                b1 :: b2 :: bs ->
                    if b1 > b2 then
                        SwapBars idx (idx + 1)

                    else
                        insertionSortFnI (idx + 1) (b2 :: bs)

                _ ->
                    NoMsg
    in
    insertionSortFnI 0 l


stepAlgorithm : Model -> t -> Msg
stepAlgorithm m _ =
    case m.active_algorithm of
        Nothing ->
            NoMsg

        Just InsertionSort ->
            insertionSortFn m.bars

        _ ->
            NoMsg


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (model.tickspeed + 1000) (stepAlgorithm model)



-- VIEW


myOnPress onPress enabled =
    if enabled then
        [ onClick onPress ]

    else
        []


myButtonSpec enabled =
    if enabled then
        [ Tw.bg_color Theme.black
        , Tw.text_color Theme.lime_400
        , Tw.font_bold
        , Tw.py_2
        , Tw.px_4
        , Tw.rounded
        , Css.hover
            [ Tw.bg_color Theme.lime_700
            ]
        ]

    else
        [ Tw.bg_color Theme.black
        , Tw.text_color Theme.lime_400
        , Tw.font_bold
        , Tw.py_2
        , Tw.px_4
        , Tw.rounded
        , Tw.opacity_50
        , Tw.cursor_not_allowed
        ]


myToggleButtonSpec active =
    [ Attr.css
        [ Tw.bg_color
            (if active then
                Theme.lime_900

             else
                Theme.black
            )
        , Tw.text_color Theme.lime_400
        , Tw.font_bold
        , Tw.rounded
        , Tw.py_2
        , Tw.px_4
        , Css.hover
            [ Tw.bg_color Theme.lime_700
            ]
        ]
    ]


myButton : { enabled : Bool, label : String, onPress : Msg } -> Html.Html Msg
myButton spec =
    Html.button
        (myOnPress spec.onPress spec.enabled ++ [ Attr.css (myButtonSpec spec.enabled) ])
        [ Html.pre [] [ Html.text spec.label ] ]


myToggleButton : { label : String, onPress : Msg, active : Bool } -> Html.Html Msg
myToggleButton spec =
    Html.button
        (myOnPress spec.onPress True ++ myToggleButtonSpec spec.active)
        [ Html.pre [] [ Html.text spec.label ] ]



-- SVG widget size:


kHEIGHT =
    600


kWIDTH =
    800


drawBarsX : Int -> Int -> Int -> Maybe Int -> Bool -> Int -> List Int -> List (Svg.Svg Msg)
drawBarsX max_height w padding active_bar algorithm_active idx bars =
    case bars of
        [] ->
            []

        b :: bs ->
            Svg.rect
                ([ SvgAttr.x (getBarPos w padding idx)
                 , SvgAttr.y (String.fromInt (max_height - getBarHeight max_height b))
                 , SvgAttr.width (String.fromInt w)
                 , SvgAttr.height (String.fromInt (getBarHeight max_height b))

                 {- TODO: unfortunately the CSS does not work ... but in general Tailwind variables are available, which is great! -}
                 , SvgAttr.css (myButtonSpec (not algorithm_active))
                 ]
                    ++ (if algorithm_active then
                            []

                        else
                            [ SvgE.onClick (ClickBar idx) ]
                       )
                )
                []
                :: drawBarsX max_height w padding active_bar algorithm_active (idx + 1) bs


drawBars model =
    let
        algorithm_active =
            model.active_algorithm /= Nothing

        active_bar =
            if algorithm_active then
                Nothing

            else
                model.active_bar
    in
    drawBarsX kHEIGHT 40 5 active_bar algorithm_active 0 model.bars


problemSizeButtons bar_count =
    Html.div []
        [ myButton { onPress = ChangeBarCount (bar_count - 1), label = "<", enabled = bar_count > kMIN_BARS }
        , Html.text "Problem Size"
        , myButton { onPress = ChangeBarCount (bar_count + 1), label = ">", enabled = bar_count < kMAX_BARS }
        ]


algoBuyButton : Int -> Algorithm -> Html.Html Msg
algoBuyButton coins algo =
    myButton { onPress = BuyAlgorithm algo.algo, label = algo.name ++ "\nCost: " ++ String.fromInt algo.price, enabled = coins > algo.price }



-- TODO: set an "active" state for the button



algoActivateButton : Algorithm -> Html.Html Msg
algoActivateButton algo =
    myToggleButton { onPress = ActivateAlgorithm algo.algo, label = algo.name, active = algo.active }


algoButton coins algo =
    if algo.unlocked then
        algoActivateButton algo

    else
        algoBuyButton coins algo


algoButtons : List Algorithm -> Int -> Html.Html Msg
algoButtons algos coins =
    Html.div []
        (List.map (algoButton coins) (List.sortBy (\x -> x.price) algos))


view model =
    Html.toUnstyled
        (Html.div []
            [ Html.h1 [] [ Html.text "CS Idle" ]
            , Html.h2 [] [ Html.text ("You have " ++ String.fromInt model.coins ++ "$") ]
            , Html.div [ Attr.css [ Css.float Css.left ] ]
                [ Svg.svg
                    [ SvgAttr.width (String.fromInt kWIDTH)
                    , SvgAttr.height (String.fromInt kHEIGHT)
                    , SvgAttr.viewBox ("0 0 " ++ String.fromInt kWIDTH ++ " " ++ String.fromInt kHEIGHT)
                    ]
                    ([ Svg.rect
                        [ SvgAttr.x "0"
                        , SvgAttr.y "0"
                        , SvgAttr.width "800"
                        , SvgAttr.height "600"
                        , SvgAttr.fill "lightgrey"
                        , SvgAttr.stroke "black"
                        ]
                        []
                     ]
                        ++ drawBars model
                    )
                , problemSizeButtons (List.length model.bars)
                ]
            , algoButtons (Dict.values model.algorithms) model.coins
            ]
        )
