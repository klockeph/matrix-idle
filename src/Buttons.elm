module Buttons exposing (..)

import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (onClick)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


baseButtonStyle : List Css.Style
baseButtonStyle =
    [ Tw.text_color Theme.lime_400
    , Tw.font_bold
    , Tw.py_2
    , Tw.px_4
    , Tw.rounded
    ]


coloredButtonStyle : Theme.Color -> List Css.Style
coloredButtonStyle color =
    baseButtonStyle
        ++ [ Tw.bg_color color
           , Tw.fill_color color
           ]


clickableButtonStyle : List Css.Style
clickableButtonStyle =
    [ Css.hover
        [ Tw.bg_color Theme.lime_700
        , Tw.fill_color Theme.lime_700
        ]
    ]


inactiveButtonStyle : List Css.Style
inactiveButtonStyle =
    coloredButtonStyle Theme.black ++ clickableButtonStyle


activeButtonStyle : List Css.Style
activeButtonStyle =
    coloredButtonStyle Theme.lime_900 ++ clickableButtonStyle


disabledButtonStyle : List Css.Style
disabledButtonStyle =
    baseButtonStyle
        ++ [ Tw.bg_color Theme.black
           , Tw.opacity_50
           , Tw.cursor_not_allowed
           ]


myOnPress onPress state =
    case state of
        Disabled ->
            []

        _ ->
            [ onClick onPress ]


myEnabledButtonStyle : Bool -> List Css.Style
myEnabledButtonStyle active =
    if active then
        activeButtonStyle

    else
        inactiveButtonStyle


myButtonStyle : ButtonState -> List Css.Style
myButtonStyle s =
    case s of
        Inactive ->
            inactiveButtonStyle

        Active ->
            activeButtonStyle

        Disabled ->
            disabledButtonStyle


type ButtonState
    = Inactive
    | Active
    | Disabled


type alias ButtonSpec msg =
    { state : ButtonState, label : String, onPress : msg }


myButton : ButtonSpec msg -> Html.Html msg
myButton spec =
    Html.button
        (myOnPress spec.onPress spec.state ++ [ Attr.css (myButtonStyle spec.state) ])
        [ Html.pre [] [ Html.text spec.label ] ]


type alias SimpleButtonSpec msg =
    { enabled : Bool, label : String, onPress : msg }



{- A simple button that can be enabled or disabled -}


mySimpleButton : SimpleButtonSpec msg -> Html.Html msg
mySimpleButton spec =
    myButton
        { state =
            if spec.enabled then
                Inactive

            else
                Disabled
        , label = spec.label
        , onPress = spec.onPress
        }
