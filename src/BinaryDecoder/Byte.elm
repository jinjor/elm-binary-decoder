module BinaryDecoder.Byte exposing
  ( ArrayBuffer, Decoder, Error, decode
  , uint8, uint16BE, uint16LE, uint32BE, uint32LE
  , int8, int16BE, int16LE, int32BE, int32LE
  , bool, char, string, stringUntilNull, symbol, symbolInt, choose, bits
  )


{-| This module defines basic byte decoders.
You also need to import BinaryDecoder to use useful combinators.

```
import BinaryDecoder exposing (..)
import BinaryDecoder.Byte exposing (..)

wave : Decoder Wave
wave =
  succeed Wave
    |. symbol "RIFF"
    |= uint32BE
    |. symbol "WAVE"
    |= formatChunk
    |= dataChunk
```

@docs ArrayBuffer, Decoder, Error, decode
@docs uint8, uint16BE, uint16LE, uint32BE, uint32LE
@docs int8, int16BE, int16LE, int32BE, int32LE
@docs bool, char, string, stringUntilNull, symbol, symbolInt, choose, bits
-}


import Char
import Bitwise
import Dict
import BinaryDecoder exposing (..)
import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)
import BinaryDecoder.Bit as BitDecoder exposing (BitDecoder)
import Native.BinaryDecoder


{-| Binary data to be decoded. An ArrayBuffer value is available though `File.readFileAsArrayBuffer` or `File.fetchArrayBuffer`.
-}
type ArrayBuffer =
  ArrayBuffer


type DataView =
  DataView


type alias State =
  GenericDecoder.State DataView


{-| Decoder type. Type variable `a` is the result type.
-}
type alias Decoder a =
  GenericDecoder DataView a


{-| Error type. It contains error message and cursor position.
-}
type alias Error =
  GenericDecoder.Error


{-| Decode from ArrayBuffer.

```
decode (succeed 1) buffer -- Ok 1
```

-}
decode : Decoder a -> ArrayBuffer -> Result Error a
decode decoder arrayBuffer =
  GenericDecoder.decode decoder (toDataView arrayBuffer)



-- READ BINARY


toDataView : ArrayBuffer -> DataView
toDataView =
  Native.BinaryDecoder.toDataView


type DecodeIntOption =
  DecodeIntOption


nativeDecodeInt : DecodeIntOption -> (State -> Result Error (State, Int))
nativeDecodeInt option = \state ->
  case Native.BinaryDecoder.decodeInt option state of
    Err s -> Err (Error state.position state.context s)
    Ok i -> Ok i



-- PRIMITIVE


int : DecodeIntOption -> Decoder Int
int option =
  GenericDecoder <| nativeDecodeInt option


{-| Decode unsigned 8 bit integer.
-}
uint8 : Decoder Int
uint8 =
  int <| Native.BinaryDecoder.uint8


{-| Decode unsigned 16 bit integer of big endian.
-}
uint16BE : Decoder Int
uint16BE =
  int <| Native.BinaryDecoder.uint16 False


{-| Decode unsigned 16 bit integer of little endian.
-}
uint16LE : Decoder Int
uint16LE =
  int <| Native.BinaryDecoder.uint16 True


{-| Decode unsigned 32 bit integer of big endian.
-}
uint32BE : Decoder Int
uint32BE =
  int <| Native.BinaryDecoder.uint32 False


{-| Decode unsigned 32 bit integer of little endian.
-}
uint32LE : Decoder Int
uint32LE =
  int <| Native.BinaryDecoder.uint32 True


{-| Decode signed 8 bit integer.
-}
int8 : Decoder Int
int8 =
  int <| Native.BinaryDecoder.int8


{-| Decode signed 16 bit integer of big endian.
-}
int16BE : Decoder Int
int16BE =
  int <| Native.BinaryDecoder.int16 False


{-| Decode signed 16 bit integer of little endian.
-}
int16LE : Decoder Int
int16LE =
  int <| Native.BinaryDecoder.int16 True


{-| Decode signed 32 bit integer of big endian.
-}
int32BE : Decoder Int
int32BE =
  int <| Native.BinaryDecoder.int32 False


{-| Decode signed 32 bit integer of little endian.
-}
int32LE : Decoder Int
int32LE =
  int <| Native.BinaryDecoder.int32 True



-- UTILITY


{-| Decode bool consuming 1 byte. True if any bit is 1.
-}
bool : Decoder Bool
bool =
  uint8
    |> map (\i -> i > 0)


{-| Decode ASCII character consuming 1 byte.
-}
char : Decoder Char
char =
  uint8
    |> map Char.fromCode


{-| Decode ASCII character with given length of bytes and make string.
-}
string : Int -> Decoder String
string length =
  repeat length char
    |> map String.fromList


{-| Read until null character and make string.
-}
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


{-| Succeeds if a particular string comes next.
-}
symbol : String -> Decoder ()
symbol s =
  equal s (string (String.length s))


{-| Succeeds if particular integers comes next.
-}
symbolInt : List Int -> Decoder ()
symbolInt list =
  equal list (repeat (List.length list) uint8)


{-| Decode 8 bit integer and return related value.

```
-- Default if buffer is 00000000
-- Special if buffer is 00000001
-- Otherwise, this decoding fails.
decode (choose [(0, Default), (1, Special)]) buffer
```

-}
choose : List (Int, a) -> Decoder a
choose list =
  uint8 |> andThen (\i ->
    list
      |> Dict.fromList
      |> Dict.get i
      |> Maybe.map succeed
      |> Maybe.withDefault (fail ("no option is given for index: " ++ toString i))
  )


{-| Create Decoder from BitDecoder that decodes given length of bytes. The length must be one of 1, 2 or 4.
-}
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
