module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Regex exposing (HowMany(All), regex)
import Html.Events exposing (onInput)


main : Program Never
main =
    Html.App.beginnerProgram { model = model, update = update, view = view }



-- MODEL


model : Model
model =
    { expression = ""
    , parsed = []
    }


type alias Model =
    { expression : String
    , parsed : List String
    }



-- PARSER


tokenize : String -> List String
tokenize str =
    let
        exp =
            regex "[+\\-*/()]|[^+\\-*/()\\s]+"

        matches =
            Regex.find All exp str
    in
        List.map .match matches



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change expression ->
            { model | expression = expression, parsed = tokenize expression }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 []
            [ text "Elm Calculator" ]
        , input [ type' "text", onInput Change ] []
        , div [] [ text <| toString model.parsed ]
        ]
