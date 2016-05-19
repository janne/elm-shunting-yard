module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (onInput)


main : Program Never
main =
    Html.App.beginnerProgram { model = model, update = update, view = view }



-- MODEL


model : Model
model =
    { expression = "" }


type alias Model =
    { expression : String
    }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change expression ->
            { model | expression = expression }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 []
            [ text "Elm Calculator" ]
        , input [ type' "text", onInput Change ] []
        ]
