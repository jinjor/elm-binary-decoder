module BinaryDecoder.Byte exposing
  ( Binary, Decoder, decode
  , uint8, uint16BE, uint16LE, uint32BE, uint32LE
  , int8, int16BE, int16LE, int32BE, int32LE
  , char, symbol, bits
  )


{-|-}


import Char
import Json.Encode
import BinaryDecoder exposing (..)
import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)
import BinaryDecoder.Bit as BitDecoder exposing (BitDecoder)
import Native.BinaryDecoder


{-|-}
type alias Binary =
  Json.Encode.Value


type alias Context =
  GenericDecoder.Context Binary


{-|-}
type alias Decoder a
  = GenericDecoder Binary a


{-|-}
decode : Decoder a -> Binary -> Result Error a
decode =
  GenericDecoder.decode



-- READ BINARY


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
char : Decoder Char
char =
  uint8
    |> map Char.fromCode


{-|-}
symbol : String -> Decoder ()
symbol s =
  match s
    ( sequence (List.repeat (String.length s) char)
        |> map String.fromList
    )


{-|-}
bits : Int -> BitDecoder a -> Decoder a
bits length bitDecoder =
  let
    intDecoder =
      case length of
        1 -> uint8
        2 -> uint16BE
        4 -> uint32BE
        _ -> fail ("invalid byte length for reading int: " ++ toString length)
  in
    intDecoder
      |> andThen (\i ->
        case BitDecoder.decode bitDecoder i of
          Ok a -> succeed a
          Err e -> fail e.message
        )
