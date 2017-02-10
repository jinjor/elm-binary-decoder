module BitDecoder exposing (BitDecoder, decode, int, bool)


import Bitwise
import GenericDecoder exposing (GenericDecoder(..), Context, Error, succeed, fail, andThen, map, sequence)


type alias Context =
  GenericDecoder.Context Int


type alias Error =
  GenericDecoder.Error


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


match : List Int -> BitDecoder ()
match ints =
  Debug.crash "not implemented."


zeros : Int -> BitDecoder ()
zeros length =
  match (List.repeat length 0)


ones : Int -> BitDecoder ()
ones length =
  match (List.repeat length 1)


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
