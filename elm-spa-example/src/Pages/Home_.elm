module Pages.Home_ exposing (view)

import Html
import UI
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body = UI.layout [ Html.text "Hello, world!" ]
    }
