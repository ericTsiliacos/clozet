module ButtonStateStyle exposing (styleButtonStates)

{-| Wrap any elements defined in [Html](http://package.elm-lang.org/packages/elm-lang/html/1.0.0/Html)
# Make Special Elements
@docs active
-}

import Html exposing (text, node, Html, Attribute)
import Html.Attributes exposing (attribute)
import String
import Char


{-| Make a special element that can notice active state.
```
main =
  ul []
    [ active styles li [] [ text "Hello" ]
    , active styles li [] [ text "World" ]
    ]

styles =
  [("background", "#abd")]
```
-}
styleButtonStates :
    List ( String, String )
    -> List ( String, String )
    -> List ( String, String )
    -> (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
styleButtonStates initialStyles activeStyles hoverStyles tag attrs children =
    let
        validInitialStyles =
            List.filter (\( k, v ) -> isValidKey k) initialStyles

        validActiveStyles =
            List.filter (\( k, v ) -> isValidKey k) activeStyles

        validHoverStyles =
            List.filter (\( k, v ) -> isValidKey k) hoverStyles

        onDown =
            attribute "onmousedown" <| String.join ";" <| List.map enterEach validActiveStyles

        onHover =
            attribute "onmouseover" <| String.join ";" <| List.map enterEach validHoverStyles

        onUp =
            attribute "onmouseup" <| String.join ";" <| List.map leaveEach validHoverStyles

        onOut =
            attribute "onmouseout" <| String.join ";" <| List.map enterEach validInitialStyles
    in
        tag ([ onDown, onHover, onUp, onOut ] ++ attrs)
            children


toCamelCase : String -> String
toCamelCase s =
    String.fromList
        <| List.reverse
        <| snd
        <| List.foldl
            (\c ( cap, memo ) ->
                if c == '-' then
                    ( True, memo )
                else if cap then
                    ( False, Char.toUpper c :: memo )
                else
                    ( False, c :: memo )
            )
            ( False, [] )
            (String.toList s)


isValidKey : String -> Bool
isValidKey s =
    s
        /= ""
        && List.any Char.isLower (String.toList s)
        && List.all (\c -> Char.isLower c || c == '-') (String.toList s)


isValidChars : List Char -> Bool
isValidChars list =
    case list of
        head :: tail ->
            if Char.isLower head || head == '-' then
                isValidChars tail
            else
                False

        _ ->
            True


enterEach : ( String, String ) -> String
enterEach ( key, value ) =
    let
        keyCamel =
            toCamelCase key

        escapedValue =
            (String.join "\"" << String.split "'") value
    in
        "this.setAttribute('data-active-"
            ++ key
            ++ "', this.style."
            ++ keyCamel
            ++ "||'');"
            ++ "this.style."
            ++ keyCamel
            ++ "='"
            ++ escapedValue
            ++ "'"


leaveEach : ( String, String ) -> String
leaveEach ( key, value ) =
    let
        keyCamel =
            toCamelCase key
    in
        "this.style." ++ keyCamel ++ "=this.getAttribute('data-active-" ++ key ++ "')||'';"
