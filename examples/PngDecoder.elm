module PngDecoder exposing (..)

import Bitwise
import Char
import BinaryDecoder exposing (..)
import BinaryDecoder.Byte exposing (..)
import BinaryDecoder.Bit exposing (..)


type alias Png =
  { ihdrChunk : IhdrChunk
  , dataChunks : List IdatChunk
  }


type alias IhdrChunk =
  { length : Int
  , width : Int
  , height : Int
  , bitDepth : Int
  , colorType : Int
  , compress : Int
  , filter : Int
  , interlace : Int
  }


type alias IdatChunk =
  { length : Int
  }


png : Decoder Png
png =
  succeed Png
    |. symbolInt [ 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A ]
    |= ihdrChunk
    |. many subChunk
    |= many idatChunk
    |. iendChunk


ihdrChunk : Decoder IhdrChunk
ihdrChunk =
  succeed IhdrChunk
    |= uint32BE -- 13
    |. symbol "IHDR"
    |= uint32BE
    |= uint32BE
    |= uint8
    |= uint8
    |= uint8
    |= uint8
    |= uint8
    |. skip 4


subChunk : Decoder ()
subChunk =
  uint32BE
    |> andThen (\length ->
      succeed ()
        |. given (string 4) (\s -> if s == "IDAT" then fail "" else succeed ())
        |. skip length
        |. skip 4
    )


idatChunk : Decoder IdatChunk
idatChunk =
  uint32BE
    |> andThen (\length ->
      succeed (IdatChunk length)
        |. symbol "IDAT"
        |. skip length
        |. skip 4
    )
    

iendChunk : Decoder ()
iendChunk =
  succeed ()
    |. uint32BE
    |. symbol "IEND"
    |. skip 4
