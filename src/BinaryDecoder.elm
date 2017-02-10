module BinaryDecoder exposing (..)

import Native.BinaryDecoder
import Json.Encode
import Char
import BitDecoder exposing (..)


type alias Context =
  { position : Int
  , source : Binary
  }


type alias Binary =
  Json.Encode.Value


type alias Error =
  { position : Int
  , message : String
  }


printError : Error -> String
printError err =
  "decode failed at " ++ toString err.position ++ ":\n\n\t" ++ err.message ++ "\n"


type alias Step a =
  Context -> Result Error (Context, a)


type Decoder a
  = Decoder (Step a)



decode : Decoder a -> Binary -> Result Error a
decode (Decoder f) source =
  case f (Context 0 source) of
    Ok (context, a) ->
      Ok a

    Err e ->
      Err e


-- READ BINARY


type DecodeIntOption =
  DecodeIntOption


nativeDecodeInt : DecodeIntOption -> Step Int
nativeDecodeInt option = \context ->
  case Native.BinaryDecoder.decodeInt option context of
    Err s -> Err (Error context.position s)
    Ok i -> Ok i


int : DecodeIntOption -> Decoder Int
int option =
  Decoder <| nativeDecodeInt option


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



-- PRIMITIVE


succeed : a -> Decoder a
succeed a =
  Decoder (\context -> Ok (context, a))


fail : String -> Decoder a
fail s =
  Decoder (\context -> Err <| Error context.position s)



-- COMBINATOR


(|=) : Decoder (a -> b) -> Decoder a -> Decoder b
(|=) transformer decoder =
  transformer
    |> andThen (\f ->
      map f decoder
    )


(|.) : Decoder a -> Decoder x -> Decoder a
(|.) decoder ignored =
  decoder
    |> andThen (\a ->
      map (\_ -> a) ignored
    )


(|+) : Decoder a -> (a -> Decoder b) -> Decoder b
(|+) =
  flip andThen


infixl 5 |=
infixl 5 |+
infixl 5 |.


andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder f_) =
  Decoder (\context ->
    f_ context
      |> Result.andThen (\(context, a) ->
        let
          (Decoder decode) =
            f a
        in
          decode context
      )
    )


given : Decoder a -> (a -> Decoder b) -> Decoder b
given =
  flip andThen


map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder f_) =
  Decoder (\context ->
    f_ context
      |> Result.map (\(context, a) ->
        (context, f a)
      )
    )



-- LAZY


lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
  Decoder (\context ->
    let
      (Decoder decode) =
        thunk ()
    in
      decode context
  )



-- JUMP


from : Int -> Decoder a -> Decoder a
from position (Decoder decode) =
  Decoder
    (\context ->
      decode { context | position = position }
        |> Result.map (\(c, a) -> ({ c | position = context.position }, a))
    )


goTo : Int -> Decoder ()
goTo position =
  Decoder
    (\context ->
      Ok ({ context | position = position }, ())
    )



-- UTILITY


sequence : List (Decoder a) -> Decoder (List a)
sequence decoders =
  case decoders of
    [] ->
      succeed []

    x :: xs ->
      x
        |> andThen (\head ->
          sequence xs
            |> map (\tail -> head :: tail)
        )


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
        Err s -> fail s
      )
