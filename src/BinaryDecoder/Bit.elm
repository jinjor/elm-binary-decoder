module BinaryDecoder.Bit exposing
  ( BitDecoder
  , decode
  , int, bool
  , zeros, ones, choose
  )


{-|
@docs BitDecoder
@docs decode
@docs int, bool
@docs zeros, ones, choose
-}

import Bitwise
import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)
import BinaryDecoder exposing (..)
import Dict


type alias Context =
  GenericDecoder.Context Int


{-| Sperial decoder type for bits.

-}
type alias BitDecoder a
  = GenericDecoder Int a


{-| Decode bits from 32 bit Integer.

```
source : Int
source =
  Bitwise.shiftLeftBy 31 1 -- 10000000 00000000 00000000 00000000

Bit.decode Bit.bool source -- True
```

-}
decode : BitDecoder a -> Int -> Result Error a
decode =
  GenericDecoder.decode



-- PRIMITIVE


{-| Decode integer with given length of bits.

```
source : Int
source =
  Bitwise.shiftLeftBy 31 1 -- 10000000 00000000 00000000 00000000

Bit.decode (Bit.int 1) source -- 1
Bit.decode (Bit.int 2) source -- 2
Bit.decode (Bit.int 3) source -- 4

Bit.decode (Bit.int 32) 42 -- 42
```

-}
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


{-| Decode bool from 1 bit.

```
source : Int
source =
  Bitwise.shiftLeftBy 28 9 -- 10010000 00000000 00000000 00000000

Bit.decode Bit.bool source -- True

Bit.decode (
  succeed (,,,,)
    |= bool
    |= bool
    |= bool
    |= bool
    |= bool
) source -- (True, False, False, True, False)
```

-}
bool : BitDecoder Bool
bool =
  map (\i -> i > 0) (int 1)



symbolInt : List Int -> BitDecoder ()
symbolInt ints =
  equal ints <|
    sequence ( List.map (always (int 1)) ints )



-- UTILITY


{-| Succeeds if all bits of given length are zero.

```
Bit.decode (zeros 32) 0 -- Ok
Bit.decode (zeros 32) 1 -- Err
Bit.decode (zeros 31) 1 -- Ok
```

-}
zeros : Int -> BitDecoder ()
zeros length =
  symbolInt (List.repeat length 0)


{-| Succeeds if all bits of given length are one.

```
Bit.decode (ones 8) (Bitwise.shiftLeftBy 24 255) -- Ok
Bit.decode (ones 8) (Bitwise.shiftLeftBy 24 254) -- Err
Bit.decode (ones 9) (Bitwise.shiftLeftBy 24 255) -- Err
```

-}
ones : Int -> BitDecoder ()
ones length =
  symbolInt (List.repeat length 1)


{-| Decode integer from given length of bits and return related value.

```
Bit.decode (choose 32 [(0, Default), (1, Special)]) 1 -- Special
```

-}
choose : Int -> List (Int, a) -> BitDecoder a
choose length list =
  int length |> andThen (\i ->
    list
      |> Dict.fromList
      |> Dict.get i
      |> Maybe.map succeed
      |> Maybe.withDefault (fail ("no option is given for index: " ++ toString i))
  )
