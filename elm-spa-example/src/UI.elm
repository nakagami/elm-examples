module UI exposing (layout)

import Html exposing (Html)
import Html.Attributes as Attr

import Gen.Route as Route

layout : List (Html msg) -> List (Html msg)
layout children =
    let
        viewLink : String -> Route -> Html msg
        viewLink label route =
            Html.a [ Attr.href (Route.toHref route) ] [ Html.text label ]
    in
    [ Html.div [ Attr.class "container" ]
        [ Html.header [ Attr.class "navbar" ]
            [ Html.strong [ Attr.class "brand" ] [ viewLink "Home" Route.Home_ ]
            , viewLink "Static" Route.Static
            , viewLink "Sandbox" Route.Sandbox
            , viewLink "Element" Route.Element
            , viewLink "Advanced" Route.Advanced
            , Html.div [ Attr.class "splitter" ] []
            , viewLink "Dynamic: Apple" (Route.Dynamic__Name_ { name = "apple" })
            , viewLink "Dynamic: Banana" (Route.Dynamic__Name_ { name = "banana" })
            ]
        , Html.main_ [] children
        ]
    ]
