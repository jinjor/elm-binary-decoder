module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (list, int, tuple, string)

import Json.Encode as E
import BinaryDecoder as B exposing ((|=), (|.))

import Native.TestData

import Char
import Bitwise

import MidiDecoder
import WaveDecoder


justTry : B.Binary -> B.Decoder a -> (() -> Expectation)
justTry binary decoder = \() ->
  case B.decode decoder binary of
    Ok a ->
      let
        _ =
          Debug.log "result" a
      in
        Expect.pass

    Err e ->
      Expect.fail (B.printError e)


testSucceed1 : B.Binary -> a -> B.Decoder a -> (() -> Expectation)
testSucceed1 binary a decoder = \() ->
  Expect.equal (Ok a) (B.decode decoder binary)


testSucceed : a -> B.Decoder a -> (() -> Expectation)
testSucceed a decoder =
  testSucceed1 Native.TestData.empty a decoder


testFail1 : B.Binary -> B.Decoder a -> (() -> Expectation)
testFail1 binary decoder = \() ->
  case B.decode decoder binary of
    Ok a ->
      Expect.fail ("Unexpectedly succeed: " ++ toString a)

    Err e ->
      Expect.pass


testFail : String -> B.Decoder a -> (() -> Expectation)
testFail err decoder = \() ->
  case B.decode decoder (E.int 0) of
    Ok a ->
      Expect.fail ("Unexpectedly succeed: " ++ toString a)

    Err e ->
      Expect.equal e.message err


simpleUints : B.Binary
simpleUints =
  Native.TestData.fromList [0,1,2,3]


primitivesAndCombinators : List Test
primitivesAndCombinators =
  [ test "suceed" <| testSucceed 1 <| B.succeed 1
  , test "fail" <| testFail "oops" <| B.fail "oops"
  , test "|=" <| testSucceed 2 <|
      B.succeed (\a -> a - 1)
        |= B.succeed 3
  , test "|=" <| testFail "oops" <|
      B.fail "oops"
        |= B.succeed 2
  , test "|=" <| testFail "oh" <|
      B.fail "oh"
        |= B.fail "my"
        |= B.fail "god"
  , test "|=" <| testSucceed1 simpleUints (0,1,2,3) <|
      B.succeed (,,,)
        |= B.uint8
        |= B.uint8
        |= B.uint8
        |= B.uint8
  , test "|." <| testSucceed1 simpleUints (1,3) <|
      B.succeed (,)
        |. B.uint8
        |= B.uint8
        |. B.uint8
        |= B.uint8
  , test "map" <| testSucceed "3" <|
      B.map toString (B.succeed 3)
  , test "andThen" <| testSucceed 2 <|
      B.andThen (\a -> B.succeed (a - 1)) (B.succeed 3)
  , test "andThen" <| testFail "oops" <|
      B.andThen (\a -> B.fail "oops") (B.succeed 3)
  , test "sequence" <| testSucceed1 Native.TestData.empty [] <|
      B.sequence []
  , test "sequence" <| testSucceed1 simpleUints [0,1,2,3] <|
      B.sequence (List.repeat 4 B.uint8)
  , test "from" <| testSucceed1 simpleUints (2,3) <|
      B.from 2 <|
        B.succeed (,)
          |= B.uint8
          |= B.uint8
  , test "from" <| testSucceed1 simpleUints (0,0,1) <|
      B.succeed (,,)
        |= B.from 0 B.uint8
        |= B.uint8
        |= B.uint8
  , test "from" <| testSucceed1 Native.TestData.variousUint (1,2,3,4,5) <|
      B.succeed (,,,,)
        |= B.from 0 B.uint8
        |= B.from 1 B.uint16BE
        |= B.from 3 B.uint32BE
        |= B.from 7 B.uint16LE
        |= B.from 9 B.uint32LE
  , test "goTo" <| testSucceed1 simpleUints (2,3) <|
      B.succeed (,)
        |. B.goTo 2
        |= B.uint8
        |= B.uint8
  ]


decodeingBytes : List Test
decodeingBytes =
  [ test "uint8" <| testSucceed1 simpleUints 0 <| B.uint8
  , test "uint8" <| testFail1 Native.TestData.empty <| B.uint8
  , test "uint8" <| testSucceed1 simpleUints (0,1,2,3) <|
      B.succeed (,,,)
        |= B.uint8
        |= B.uint8
        |= B.uint8
        |= B.uint8
  , test "various uintFrom" <| testSucceed1 Native.TestData.variousUint (1,2,3,4,5) <|
      B.succeed (,,,,)
        |= B.uint8
        |= B.uint16BE
        |= B.uint32BE
        |= B.uint16LE
        |= B.uint32LE
  , test "char" <| testSucceed1 (fromString "MThd") "MThd" <|
      B.succeed String.fromList
        |= B.sequence (List.repeat 4 B.char)
  ]

decodeMidi : List Test
decodeMidi =
  [ test "deltaTime" <| testSucceed1 (Native.TestData.fromList [0x7F]) 0x7F MidiDecoder.deltaTime
  , test "deltaTime" <| testSucceed1 (Native.TestData.fromList [0x81, 0x00]) 0x80 MidiDecoder.deltaTime
  , test "deltaTime" <| testSucceed1 (Native.TestData.fromList [0x81, 0x01]) 0x81 MidiDecoder.deltaTime
  , test "midi" <| justTry midi MidiDecoder.midi
  ]


fromString : String -> B.Binary
fromString s =
  Native.TestData.fromList (String.toList s |> List.map Char.toCode)


midi : B.Binary
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
    , describe "decode midi" decodeMidi
    ]
