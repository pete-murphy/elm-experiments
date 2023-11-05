module Page.Login exposing
    ( Model
    , init
    , view
    )

import Accessibility as Html exposing (Html)
import Browser exposing (Document)
import Flags exposing (Flags)



-- MODEL


type alias Model =
    ()


init : Flags -> ( Model, Cmd msg )
init _ =
    ( (), Cmd.none )



-- VIEW


view : Model -> Document msg
view _ =
    { title = "Login"
    , body = [ Html.h1 [] [ Html.text "Login" ] ]
    }
