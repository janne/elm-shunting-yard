module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import List
import String
import Regex exposing (HowMany(All), regex)
import Html.Events exposing (onInput)


main : Program Never
main =
    Html.App.beginnerProgram { model = model, update = update, view = view }



-- MODEL


model : Model
model =
    { expression = ""
    , parsed = Ok []
    }


type Token
    = Number Int
    | OpenParen
    | CloseParen
    | AddOp
    | SubOp
    | MulOp
    | DivOp
    | Illegal String


isOp : Token -> Bool
isOp token =
    List.member token [ AddOp, SubOp, MulOp, DivOp ]


prec : Token -> Int
prec token =
    case token of
        AddOp ->
            0

        SubOp ->
            0

        MulOp ->
            1

        DivOp ->
            1

        OpenParen ->
            2

        CloseParen ->
            2

        Number _ ->
            3

        Illegal _ ->
            99


type alias Model =
    { expression : String
    , parsed : Result String (List Token)
    }



-- PARSER


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                x :: takeWhile predicate xs
            else
                []


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                dropWhile predicate xs
            else
                list


transform : List Token -> Result String (List Token)
transform tokens =
    let
        transform' input stack output =
            case input of
                [] ->
                    case stack of
                        [] ->
                            Ok output

                        head :: tail ->
                            if head == OpenParen then
                                Err "Mismatched open parentheses"
                            else
                                transform' [] tail (output ++ [ head ])

                head :: tail ->
                    case head of
                        Illegal s ->
                            Err <| "Illegal token: " ++ s

                        Number n ->
                            transform' tail stack (output ++ [ Number n ])

                        OpenParen ->
                            transform' tail (head :: stack) output

                        CloseParen ->
                            let
                                stack' =
                                    List.tail <| dropWhile ((/=) OpenParen) stack

                                output' =
                                    output ++ takeWhile ((/=) OpenParen) stack
                            in
                                case stack' of
                                    Nothing ->
                                        Err "Mismatched close parentheses"

                                    Just list ->
                                        transform' tail list output'

                        op ->
                            let
                                cond op' =
                                    isOp op' && (prec op < prec op')

                                stack' =
                                    op :: dropWhile cond stack

                                output' =
                                    output ++ takeWhile cond stack
                            in
                                transform' tail stack' output'
    in
        transform' tokens [] []


parse : List String -> List Token
parse ts =
    let
        toToken str =
            case str of
                "+" ->
                    AddOp

                "-" ->
                    SubOp

                "*" ->
                    MulOp

                "/" ->
                    DivOp

                "(" ->
                    OpenParen

                ")" ->
                    CloseParen

                _ ->
                    case String.toInt str of
                        Ok n ->
                            Number n

                        _ ->
                            Illegal str
    in
        List.map toToken ts


tokenize : String -> List String
tokenize str =
    let
        exp =
            regex "[+\\-*/()]|[^+\\-*/()\\s]+"

        matches =
            Regex.find All exp str
    in
        List.map .match matches



-- CALC
--calc : List Token -> Int


calc tokens =
    let
        f op lst =
            case lst of
                (Number x) :: (Number y) :: ys ->
                    case op of
                        MulOp ->
                            Number (x * y) :: ys

                        DivOp ->
                            Number (y // x) :: ys

                        AddOp ->
                            Number (x + y) :: ys

                        SubOp ->
                            Number (y - x) :: ys

                        _ ->
                            op :: lst

                _ ->
                    op :: lst
    in
        case List.head <| List.foldl f [ Number 0 ] tokens of
            Just (Number n) ->
                n

            _ ->
                0



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change expression ->
            { model | expression = expression, parsed = tokenize expression |> parse |> transform }



-- VIEW


view : Model -> Html Msg
view model =
    let
        result =
            case model.parsed of
                Ok a ->
                    toString <| calc a

                Err s ->
                    s
    in
        div [ class "content" ]
            [ h1 []
                [ text "Elm Calculator" ]
            , input [ type' "text", onInput Change ] []
            , div [] [ text <| toString model.parsed ]
            , div [] [ text result ]
            ]
