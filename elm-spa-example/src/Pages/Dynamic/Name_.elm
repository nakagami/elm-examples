module Pages.Dynamic.Name_ exposing (page)

import Gen.Params.Dynamic.Name_ exposing (Params)
import Page exposing (Page)
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page
page _ req =
    Page.static
        { view = view req.params
        }


view : Params -> View msg
view params =
    { title = "Dynamic: " ++ params.name
    , body =
        UI.layout
            [ UI.h1 "Dynamic Page"
            , Html.h2 [] [ Html.text params.name ]
            ]
    }
