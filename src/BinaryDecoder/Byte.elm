module BinaryDecoder.Byte exposing
  ( ArrayBuffer, Decoder, decode
  , uint8, uint16BE, uint16LE, uint32BE, uint32LE
  , int8, int16BE, int16LE, int32BE, int32LE
  , bool, char, string, stringUntilNull, symbol, symbolInt, choose, bits
  )


{-|-}


import Char
import Bitwise
import Dict
import Json.Encode
import BinaryDecoder exposing (..)
import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)
import BinaryDecoder.Bit as BitDecoder exposing (BitDecoder)
import Native.BinaryDecoder


{-|-}
type ArrayBuffer =
  ArrayBuffer


type DataView =
  DataView


type alias Context =
  GenericDecoder.Context DataView


{-|-}
type alias Decoder a
  = GenericDecoder DataView a


{-|-}
decode : Decoder a -> ArrayBuffer -> Result Error a
decode decoder arrayBuffer =
  GenericDecoder.decode decoder (toDataView arrayBuffer)



-- READ BINARY


toDataView : ArrayBuffer -> DataView
toDataView =
  Native.BinaryDecoder.toDataView


type DecodeIntOption =
  DecodeIntOption


nativeDecodeInt : DecodeIntOption -> (Context -> Result Error (Context, Int))
nativeDecodeInt option = \context ->
  case Native.BinaryDecoder.decodeInt option context of
    Err s -> Err (Error context.position s)
    Ok i -> Ok i


int : DecodeIntOption -> Decoder Int
int option =
  GenericDecoder <| nativeDecodeInt option


{-|-}
uint8 : Decoder Int
uint8 =
  int <| Native.BinaryDecoder.uint8


{-|-}
uint16BE : Decoder Int
uint16BE =
  int <| Native.BinaryDecoder.uint16 False


{-|-}
uint16LE : Decoder Int
uint16LE =
  int <| Native.BinaryDecoder.uint16 True


{-|-}
uint32BE : Decoder Int
uint32BE =
  int <| Native.BinaryDecoder.uint32 False


{-|-}
uint32LE : Decoder Int
uint32LE =
  int <| Native.BinaryDecoder.uint32 True


{-|-}
int8 : Decoder Int
int8 =
  int <| Native.BinaryDecoder.int8


{-|-}
int16BE : Decoder Int
int16BE =
  int <| Native.BinaryDecoder.int16 False


{-|-}
int16LE : Decoder Int
int16LE =
  int <| Native.BinaryDecoder.int16 True


{-|-}
int32BE : Decoder Int
int32BE =
  int <| Native.BinaryDecoder.int32 False


{-|-}
int32LE : Decoder Int
int32LE =
  int <| Native.BinaryDecoder.int32 True



-- UTILITY


{-|-}
bool : Decoder Bool
bool =
  uint8
    |> map (\i -> i > 0)



{-|-}
char : Decoder Char
char =
  uint8
    |> map Char.fromCode


{-|-}
string : Int -> Decoder String
string length =
  repeat length char
    |> map String.fromList


{-|-}
stringUntilNull : Decoder String
stringUntilNull =
  succeed String.fromList
    |= many nonNullChar
    |. uint8


nonNullInt : Decoder Int
nonNullInt =
  uint8
    |> andThen
      (\i ->
        if i > 0 then
          succeed i
        else
          fail "null char"
      )


nonNullChar : Decoder Char
nonNullChar =
  map Char.fromCode nonNullInt


{-|-}
symbol : String -> Decoder ()
symbol s =
  equal s (string (String.length s))


{-|-}
symbolInt : List Int -> Decoder ()
symbolInt list =
  equal list (repeat (List.length list) uint8)


{-|-}
choose : List (Int, a) -> Decoder a
choose list =
  given uint8 (\i ->
    list
      |> Dict.fromList
      |> Dict.get i
      |> Maybe.map succeed
      |> Maybe.withDefault (fail ("no option is given for index: " ++ toString i))
  )


{-|-}
bits : Int -> BitDecoder a -> Decoder a
bits length bitDecoder =
  let
    intDecoder =
      case length of
        1 -> map (Bitwise.shiftLeftBy 24) uint8
        2 -> map (Bitwise.shiftLeftBy 16) uint16BE
        4 -> uint32BE
        _ -> fail ("invalid byte length for reading int: " ++ toString length)
  in
    intDecoder
      |> andThen (\i ->
        case BitDecoder.decode bitDecoder i of
          Ok a -> succeed a
          Err e -> fail e.message
        )
