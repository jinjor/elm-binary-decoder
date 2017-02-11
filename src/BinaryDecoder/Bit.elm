module BinaryDecoder.Bit exposing
  ( BitDecoder
  , decode
  , int, bool
  , zeros, ones, choose
  )


import Bitwise
import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)
import BinaryDecoder exposing (..)
import Dict


type alias Context =
  GenericDecoder.Context Int


type alias BitDecoder a
  = GenericDecoder Int a


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


int : Int -> BitDecoder Int
int length =
  if length < 0 then
    fail ("invalid length " ++ toString length)
  else
    GenericDecoder (\context ->
      case intHelp 0 context.position length of
        Ok i ->
          Ok ({ context | position = context.position + length }, i)

        Err s ->
          Err (Error context.position s)
    )


intHelp : Int -> Int -> Int -> Result String Int
intHelp prev from length =
  Debug.crash "not implemented."


bool : BitDecoder Bool
bool =
  map (\i -> i > 0) (int 1)


zeros : Int -> BitDecoder ()
zeros length =
  match (List.repeat length 0)


ones : Int -> BitDecoder ()
ones length =
  match (List.repeat length 1)


choose : Int -> List (Int, a) -> BitDecoder a
choose length list =
  given (int length) (\i ->
    list
      |> Dict.fromList
      |> Dict.get 1
      |> Maybe.map succeed
      |> Maybe.withDefault (fail ("no option is given for index: " ++ toString i))
  )
