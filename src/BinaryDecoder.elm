module BinaryDecoder exposing (..)

import Native.BinaryDecoder
import Json.Encode
import Char
import BitDecoder exposing (BitDecoder)
import GenericDecoder exposing (GenericDecoder(..), Context, Error, succeed, fail, andThen, map, sequence)


type alias Context =
  GenericDecoder.Context Binary


type alias Binary =
  Json.Encode.Value


type alias Error =
  GenericDecoder.Error


printError : Error -> String
printError err =
  "decode failed at " ++ toString err.position ++ ":\n\n\t" ++ err.message ++ "\n"


type alias Decoder a
  = GenericDecoder Binary a


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


uint8 : Decoder Int
uint8 =
  int <| Native.BinaryDecoder.uint8


uint16BE : Decoder Int
uint16BE =
  int <| Native.BinaryDecoder.uint16 False


uint16LE : Decoder Int
uint16LE =
  int <| Native.BinaryDecoder.uint16 True


uint32BE : Decoder Int
uint32BE =
  int <| Native.BinaryDecoder.uint32 False


uint32LE : Decoder Int
uint32LE =
  int <| Native.BinaryDecoder.uint32 True


char : Decoder Char
char =
  uint8
    |> map Char.fromCode



-- UTILITY


symbol : String -> Decoder ()
symbol s =
  sequence (List.repeat (String.length s) char)
    |> map String.fromList
    |> andThen (\str ->
        if str == s then
          succeed ()
        else
          fail ("expected " ++ s ++ ", but got " ++ str)
      )



bits : Decoder Int -> BitDecoder a -> Decoder a
bits intDecoder bitDecoder =
  intDecoder
    |> andThen (\i ->
      case BitDecoder.decode bitDecoder i of
        Ok a -> succeed a
        Err e -> fail e.message
      )
