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
  if from < 0 || from + length >= 32 then
    Err ("index out of bounds: " ++ toString length ++ " bits from " ++ toString from)
  else
    source
      |> Bitwise.shiftRightBy (32 - (from + length))
      |> Bitwise.and (2 ^ length - 1)
      |> Ok


{-|-}
bool : BitDecoder Bool
bool =
  map (\i -> i > 0) (int 1)



symbolInt : List Int -> BitDecoder ()
symbolInt ints =
  equal ints <|
    sequence ( List.map (always (int 1)) ints )


{-|-}
zeros : Int -> BitDecoder ()
zeros length =
  symbolInt (List.repeat length 0)


{-|-}
ones : Int -> BitDecoder ()
ones length =
  symbolInt (List.repeat length 1)


{-|-}
choose : Int -> List (Int, a) -> BitDecoder a
choose length list =
  given (int length) (\i ->
    list
      |> Dict.fromList
      |> Dict.get i
      |> Maybe.map succeed
      |> Maybe.withDefault (fail ("no option is given for index: " ++ toString i))
  )
