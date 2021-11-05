module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Date exposing (Date, Unit(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Task
import Throttle exposing (Throttle)
import Time exposing (Month(..))
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, top)



-- APP


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- MODEL


type alias Model =
    { 
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    ( {     
    }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = (model, Cmd.none)

-- VIEW


view : Model -> Html Msg
view model =
        div [ class "text-red-800" ] [ text "Hello world!" ]