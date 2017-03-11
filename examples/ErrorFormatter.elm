module ErrorFormatter exposing (print)

import Html exposing (..)
import Html.Attributes exposing (..)

import BinaryDecoder exposing (..)
import BinaryDecoder.Byte as Byte exposing (..)
import Hex


{-|-}
print : ArrayBuffer -> Error -> Html msg
print buf error =
  case Byte.decode (err error) buf of
    Ok html -> html
    Err e ->
      let
        _ = Debug.log "print" e
      in
        div [] [ text error.message, text " (print failed)" ]


err : Error -> Decoder (Html msg)
err error =
  let
    start =
      startPosition error.position

    startRowIndex =
      start // 16
  in
    succeed (toHtml startRowIndex error.position error.message)
      |. goTo start
      |= repeat (error.position - start) uint8
      |= uint8


startPosition : Int -> Int
startPosition pos =
  let
    start =
      Basics.max 0 (pos - 64)
  in
    start - start % 16


toHtml : Int -> Int -> String -> List Int -> Int -> Html msg
toHtml startRowIndex position message before value =
  let
    (_, last_, init_) =
      before
        |> List.foldl (\value (i, tmp, list) ->
          if i % 16 == 15 then
            (i + 1, [], (value :: tmp) :: list)
          else
            (i + 1, value :: tmp, list)
          ) (0, [], [])

    init =
      init_
        |> List.map List.reverse
        |> List.reverse

    last =
      last_
        |> List.reverse
  in
    pre [ style preStyle ]
      [ div [] [ text <| "decode failed at " ++ toString position ]
      , div
          [ style numbersStyle ]
          ( List.indexedMap (row startRowIndex) init ++ [ lastRow (startRowIndex + List.length init) last value ] )
      , div [] [ text message ]
      ]


preStyle : List (String, String)
preStyle =
  [ ("font-family", "monospace")
  , ("padding", "20px")
  , ("margin", "20px")
  , ("background-color", "#ddd")
  ]


numbersStyle : List (String, String)
numbersStyle =
  [ ("padding-left", "40px")
  , ("margin-top", "10px")
  , ("margin-bottom", "10px")
  ]


row : Int -> Int -> List Int -> Html msg
row startRowIndex index numbers =
  numbers
    |> List.map (cell False)
    |> (::) (span [] [ text <| String.padLeft 4 ' ' <| toString (startRowIndex + index), text " |    " ])
    |> div []


lastRow : Int -> List Int -> Int -> Html msg
lastRow rowIndex numbers value =
  numbers
    |> List.map (cell False)
    |> (::) (span [] [ text <| String.padLeft 4 ' ' <| toString rowIndex, text " |    " ])
    |> (\normalCells -> normalCells ++ [ cell True value ] )
    |> div []


cell : Bool -> Int -> Html msg
cell strong value =
  span
    [ style (if strong then [("color", "red")] else []) ]
    [ text (Hex.toString value |> String.padLeft 2 '0'), text " " ]
