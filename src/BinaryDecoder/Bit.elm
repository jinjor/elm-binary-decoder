module BinaryDecoder.Bit exposing
  ( BitDecoder
  , decode
  , int, bool
  , zeros, ones, choose
  )


{-|-}

import Bitwise
import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)
import BinaryDecoder exposing (..)
import Dict


type alias Context =
  GenericDecoder.Context Int


{-|-}
type alias BitDecoder a
  = GenericDecoder Int a


{-|-}
decode : BitDecoder a -> Int -> Result Error a
decode =
  GenericDecoder.decode



-- READ BITS


bitAt : Int -> BitDecoder Bool
bitAt n =
  if n < 0 || n >= 8 then
    fail ("out of index: " ++ toString n)
  else
    GenericDecoder (\context ->
      context.source
        |> Bitwise.shiftRightBy (7 - n)
        |> Bitwise.and 1
        |> (\a -> a > 0)
        |> ((,) context)
        |> Ok
      )



-- UTILITY


{-|-}
int : Int -> BitDecoder Int
int length =
  if length < 0 then
    fail ("invalid length " ++ toString length)
  else
    GenericDecoder (\context ->
      case intHelp context.position length context.source of
        Ok i ->
          Ok ({ context | position = context.position + length }, i)

        Err s ->
          Err (Error context.position s)
    )


intHelp : Int -> Int -> Int -> Result String Int
intHelp from length source =
  if from < 0 then
    Err ("index out of bounds: " ++ toString from)
  else if from < 8 then
    Ok <| intHelpHelp from length source
  else
    Ok <| intHelpHelp (from - 8) length (source |> Bitwise.shiftLeftBy 8)


intHelpHelp : Int -> Int -> Int -> Int
intHelpHelp from length source =
  if length == 0 then
    0
  else
    let
      to =
        min 8 (from + length)

      len =
        to - from

      rest =
        length - len

      this =
        source
          |> Bitwise.shiftRightBy (32 - to)
          |> Bitwise.and (2 ^ len - 1)
    in
      intHelpHelp 0 rest (source |> Bitwise.shiftLeftBy 8)
        + (this |> Bitwise.shiftLeftBy rest)


{-|-}
bool : BitDecoder Bool
bool =
  map (\i -> i > 0) (int 1)



matchInts : List Int -> BitDecoder ()
matchInts ints =
  match ints <|
    sequence ( List.map (always (int 1)) ints )


{-|-}
zeros : Int -> BitDecoder ()
zeros length =
  matchInts (List.repeat length 0)


{-|-}
ones : Int -> BitDecoder ()
ones length =
  matchInts (List.repeat length 1)


{-|-}
choose : Int -> List (Int, a) -> BitDecoder a
choose length list =
  given (int length) (\i ->
    list
      |> Dict.fromList
      |> Dict.get 1
      |> Maybe.map succeed
      |> Maybe.withDefault (fail ("no option is given for index: " ++ toString i))
  )
