module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)

import BinaryDecoder.Byte as B exposing (ArrayBuffer)
import BinaryDecoder.Bit as Bit
import BinaryDecoder exposing (..)
import BinaryDecoder.GenericDecoder as G exposing (GenericDecoder)

import Native.TestData

import Char
import Bitwise

import SmfDecoder
import WaveDecoder
import Mp3Decoder


justTry : ArrayBuffer -> B.Decoder a -> (() -> Expectation)
justTry binary decoder = \() ->
  case B.decode decoder binary of
    Ok a ->
      let
        _ =
          Debug.log "result" a
      in
        Expect.pass

    Err e ->
      Expect.fail (printError e)


testSucceed1 : ArrayBuffer -> a -> B.Decoder a -> (() -> Expectation)
testSucceed1 source a decoder = \() ->
  Expect.equal (Ok a) (B.decode decoder source)


testSucceedBit1 : Int -> a -> Bit.BitDecoder a -> (() -> Expectation)
testSucceedBit1 source a decoder = \() ->
  Expect.equal (Ok a) (Bit.decode decoder source)


testSucceed : a -> B.Decoder a -> (() -> Expectation)
testSucceed a decoder =
  testSucceed1 (uints []) a decoder


testFail1 : ArrayBuffer -> B.Decoder a -> (() -> Expectation)
testFail1 source decoder = \() ->
  case B.decode decoder source of
    Ok a ->
      Expect.fail ("Unexpectedly succeed: " ++ toString a)

    Err e ->
      Expect.pass


testFailBit1 : Int -> Bit.BitDecoder a -> (() -> Expectation)
testFailBit1 source decoder = \() ->
  case Bit.decode decoder source of
    Ok a ->
      Expect.fail ("Unexpectedly succeed: " ++ toString a)

    Err e ->
      Expect.pass


testFail : String -> B.Decoder a -> (() -> Expectation)
testFail err decoder = \() ->
  case B.decode decoder (uints []) of
    Ok a ->
      Expect.fail ("Unexpectedly succeed: " ++ toString a)

    Err e ->
      Expect.equal e.message err


uints : List Int -> ArrayBuffer
uints list =
  Native.TestData.fromList list


primitivesAndCombinators : List Test
primitivesAndCombinators =
  [ test "suceed" <| testSucceed 1 <| succeed 1
  , test "fail" <| testFail "oops" <| fail "oops"
  , test "|=" <| testSucceed (1,2) <|
      succeed (,)
        |= succeed 1
        |= succeed 2
  , test "|=" <| testFail "oops" <|
      fail "oops"
        |= succeed 2
  , test "|=" <| testFail "oh" <|
      fail "oh"
        |= fail "my"
        |= fail "god"
  , test "|." <| testSucceed (2,4) <|
      succeed (,)
        |. succeed 1
        |= succeed 2
        |. succeed 3
        |= succeed 4
  , test "|." <| testFail "oh" <|
      succeed ()
        |. fail "oh"
        |. fail "my"
        |. fail "god"
  , test "map" <| testSucceed "3" <|
      map toString (succeed 3)
  , test "andThen" <| testSucceed 2 <|
      andThen (\a -> succeed (a - 1)) (succeed 3)
  , test "andThen" <| testFail "oops" <|
      andThen (\a -> fail "oops") (succeed 3)
  , test "sequence" <| testSucceed1 (uints []) [] <|
      sequence []
  , test "sequence" <| testSucceed1 (uints [0,1,2,3]) [0,1,2,3] <|
      sequence (List.repeat 4 B.uint8)
  , test "from" <| testSucceed1 (uints [0,1,2,3]) (2,3) <|
      from 2 <|
        succeed (,)
          |= B.uint8
          |= B.uint8
  , test "from" <| testSucceed1 (uints [0,1,2,3]) (0,0,1) <|
      succeed (,,)
        |= from 0 B.uint8
        |= B.uint8
        |= B.uint8
  , test "from" <| testSucceed1 Native.TestData.variousUint (1,2,3,4,5) <|
      succeed (,,,,)
        |= from 0 B.uint8
        |= from 1 B.uint16BE
        |= from 3 B.uint32BE
        |= from 7 B.uint16LE
        |= from 9 B.uint32LE
  , test "goTo" <| testSucceed1 (uints [0,1,2,3]) (2,3) <|
      succeed (,)
        |. goTo 2
        |= B.uint8
        |= B.uint8
  , test "repeat" <| testSucceed1 (uints [0,1,2,3]) [] <|
      repeat 0 B.uint8
  , test "repeat" <| testSucceed1 (uints [0,1,2,3]) [0,1,2,3] <|
      repeat 4 B.uint8
  , test "repeat" <| testFail1 (uints [0,1,2,3]) <|
      repeat 5 B.uint8
  , test "many" <| testSucceed1 (uints [0,1,2,3]) [0,1] <|
      many
        ( B.uint8
            |> andThen (\i -> if i == 2 then fail "two" else succeed i)
        )
  , test "many" <| testSucceed1 (uints [0,1,2,3]) ([0,1],2,3) <|
      ( succeed (,,)
          |= many
            ( B.uint8
                |> andThen (\i -> if i == 2 then fail "done" else succeed i)
            )
          |= B.uint8
          |= B.uint8
      )
  , test "many" <| testSucceed1 (uints [0,1,2,3,4,5]) ([(0,1),(2,3)],4,5) <|
      ( succeed (,,)
          |= many
            ( succeed (,)
                |= (B.uint8 |> andThen (\i -> if i == 4 then fail "done" else succeed i))
                |= B.uint8
            )
          |= B.uint8
          |= B.uint8
      )
  , test "many" <| testSucceed1 (uints [0,1,2,3,4,5]) ([(0,1),(2,3)],4,5) <|
      ( succeed (,,)
          |= many
            ( succeed (,)
                |= B.uint8
                |= (B.uint8 |> andThen (\i -> if i == 5 then fail "done" else succeed i))
            )
          |= B.uint8
          |= B.uint8
      )
  , test "many" <| testSucceed1 (uints [0,1,2,3]) [0,1,2,3] <|
      many B.uint8
  ]


decodeingBytes : List Test
decodeingBytes =
  [ test "uint8" <| testSucceed1 (uints [0]) 0 <| B.uint8
  , test "uint8" <| testFail1 (uints []) <| B.uint8
  , test "uint8" <| testSucceed1 (uints [0,1]) (0,1) <|
      succeed (,)
        |= B.uint8
        |= B.uint8
  , test "various uint" <| testSucceed1 Native.TestData.variousUint (1,2,3,4,5) <|
      succeed (,,,,)
        |= B.uint8
        |= B.uint16BE
        |= B.uint32BE
        |= B.uint16LE
        |= B.uint32LE
  , test "various int" <| testSucceed1 Native.TestData.variousUint (-1,-2,-3,-4,-5) <|
      succeed (,,,,)
        |. goTo 13
        |= B.int8
        |= B.int16BE
        |= B.int32BE
        |= B.int16LE
        |= B.int32LE
  , test "char" <| testSucceed1 (fromString "aA1 ") "aA1 " <|
      succeed String.fromList
        |= sequence (List.repeat 4 B.char)
  , test "symbol" <| testSucceed1 (fromString "aA1 ") () <|
      B.symbol "aA1 "
  ]


decodeingBits : List Test
decodeingBits =
  [ test "int" <| testSucceedBit1 0 [0,0,0,0,0,0,0,0] <| sequence (List.repeat 8 (Bit.int 1))
  , test "int" <| testSucceedBit1 (1 |> Bitwise.shiftLeftBy 24) [0,0,0,0,0,0,0,1] <| sequence (List.repeat 8 (Bit.int 1))
  , test "int" <| testSucceedBit1 (255 |> Bitwise.shiftLeftBy 24) [1,1,1,1,1,1,1,1] <| sequence (List.repeat 8 (Bit.int 1))
  , test "int" <| testSucceedBit1 (8 |> Bitwise.shiftLeftBy 24) [0,0,0,0,1,0,0,0] <| sequence (List.repeat 8 (Bit.int 1))
  , test "int" <| testSucceedBit1 (8 |> Bitwise.shiftLeftBy 24) 0 <| Bit.int 4
  , test "int" <| testSucceedBit1 (8 |> Bitwise.shiftLeftBy 24) 1 <| Bit.int 5
  , test "int" <| testSucceedBit1 (8 |> Bitwise.shiftLeftBy 24) 2 <| Bit.int 6
  , test "int" <| testSucceedBit1 (8 |> Bitwise.shiftLeftBy 24) 4 <| Bit.int 7
  , test "bool" <| testSucceedBit1 (1 |> Bitwise.shiftLeftBy 31) True <| Bit.bool
  , test "bool" <| testSucceedBit1 (1 |> Bitwise.shiftLeftBy 30) False <| Bit.bool
  , test "ones" <| testSucceedBit1 (255 |> Bitwise.shiftLeftBy 24) () <| Bit.ones 8
  , test "ones" <| testFailBit1 (254 |> Bitwise.shiftLeftBy 24) <| Bit.ones 8
  , test "ones" <| testFailBit1 (127 |> Bitwise.shiftLeftBy 24) <| Bit.ones 8
  , test "zeros" <| testSucceedBit1 (0 |> Bitwise.shiftLeftBy 24) () <| Bit.zeros 8
  , test "ones" <| testFailBit1 (1 |> Bitwise.shiftLeftBy 24) <| Bit.zeros 8
  , test "ones" <| testFailBit1 (127 |> Bitwise.shiftLeftBy 24) <| Bit.zeros 8
  , test "choose" <| testSucceedBit1 (0 |> Bitwise.shiftLeftBy 30) 0 <| Bit.choose 2 [(0,0),(1,1),(2,2),(3,3)]
  , test "choose" <| testSucceedBit1 (1 |> Bitwise.shiftLeftBy 30) 1 <| Bit.choose 2 [(1,1),(2,2),(3,3)]
  , test "choose" <| testSucceedBit1 (2 |> Bitwise.shiftLeftBy 30) 2 <| Bit.choose 2 [(2,2),(3,3)]
  , test "choose" <| testSucceedBit1 (3 |> Bitwise.shiftLeftBy 30) 3 <| Bit.choose 2 [(3,3)]
  , test "choose" <| testSucceedBit1 (3 |> Bitwise.shiftLeftBy 30) 1 <| Bit.choose 1 [(1,1)]
  , test "choose" <| testFailBit1 (3 |> Bitwise.shiftLeftBy 30) <| Bit.choose 1 [(3,3)]
  , test "choose" <| testFailBit1 (0 |> Bitwise.shiftLeftBy 30) <| Bit.choose 2 []
  , test "choose" <| testFailBit1 (0 |> Bitwise.shiftLeftBy 30) <| Bit.choose 2 []
  ]


decodeMidi : List Test
decodeMidi =
  [ test "deltaTime" <| testSucceed1 (Native.TestData.fromList [0x7F]) 0x7F SmfDecoder.deltaTime
  , test "deltaTime" <| testSucceed1 (Native.TestData.fromList [0x81, 0x00]) 0x80 SmfDecoder.deltaTime
  , test "deltaTime" <| testSucceed1 (Native.TestData.fromList [0x81, 0x01]) 0x81 SmfDecoder.deltaTime
  , test "midi" <| justTry midi SmfDecoder.smf
  ]


fromString : String -> ArrayBuffer
fromString s =
  Native.TestData.fromList (String.toList s |> List.map Char.toCode)


midi : ArrayBuffer
midi =
  Native.TestData.fromList <|
    [77,84,104,100 -- 0
    ,0,0,0,6  -- 4
    ,0,0  -- 8
    ,0,1  -- 10
    ,1,224 -- 12
    ,77,84,114,107 -- 14
    ,0,0,0,60 -- 18
    ,0,192,0 -- 22
    ,0,144,60,64 -- 25
    -- ,131,96,60,0
    ,0,255,47,0 -- 29
    ]


all : Test
all =
  describe "Decoder"
    [ describe "primitivs and combinators" primitivesAndCombinators
    , describe "decodeing bytes" decodeingBytes
    , describe "decodeing bits" decodeingBits
    , describe "decode midi" decodeMidi
    ]
