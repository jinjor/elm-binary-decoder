module PngDecoder exposing (..)

import Bitwise
import Char
import Dict exposing (Dict)
import BinaryDecoder exposing (..)
import BinaryDecoder.Byte exposing (..)


type alias Png =
  { ihdr : Maybe IhdrChunk
  , plte : Maybe PlteChunk
  , data : List IdatChunk
  , gama : Maybe GamaChunk
  , chrm : Maybe ChrmChunk
  , srgb : Maybe SrgbChunk
  , iccp : Maybe IccpChunk
  , text : List TextChunk
  , ztxt : List ZtxtChunk
  , itxt : List ItxtChunk
  , bkgd : Maybe BkgdChunk
  , phys : Maybe PhysChunk
  , sbit : Maybe SbitChunk
  , splt : List SpltChunk
  , hist : Maybe HistChunk
  , time : Maybe TimeChunk
  , iend : Bool
  , others : List OtherChunk
  }


init : Png
init =
  { ihdr = Nothing
  , plte = Nothing
  , data = []
  , gama = Nothing
  , chrm = Nothing
  , srgb = Nothing
  , iccp = Nothing
  , text = []
  , ztxt = []
  , itxt = []
  , bkgd = Nothing
  , phys = Nothing
  , sbit = Nothing
  , splt = []
  , hist = Nothing
  , time = Nothing
  , iend = False
  , others = []
  }


type alias IhdrChunk =
  { width : Int
  , height : Int
  , bitDepth : Int
  , colorType : Int
  , compress : Int
  , filter : Int
  , interlace : Int
  }


type alias PlteChunk =
  { values : List (Int, Int, Int)
  }


type alias GamaChunk =
  { gamma : Int
  }


type alias ChrmChunk =
  { whitePointX : Int
  , whitePointY : Int
  , redX : Int
  , redY : Int
  , greenX : Int
  , greenY : Int
  , blueX : Int
  , blueY : Int
  }


type alias SrgbChunk =
  { chunkData : Int
  }


type alias IccpChunk =
  { profile : String
  , compressType : Int
  }


type alias TextChunk =
  { keyWord : String
  , text : String
  }

type alias ZtxtChunk =
  { keyWord : String
  , compressType : Int
  }


type alias ItxtChunk =
  { keyword : String
  , compressed : Bool
  , complessType : Int
  , langTag : String
  , translationKeyword : String
  , text : String
  }


type BkgdChunk
  = IndexedColor Int
  | GrayScale Int
  | TrueColor Int Int Int


type alias PhysChunk =
  { pixelXperUnit : Int
  , pixelYperUnit : Int
  , isMeter : Bool
  }


type SbitChunk
  = SbitChunk3 Int Int Int
  | SbitChunk0 Int
  | SbitChunk2 Int Int Int
  | SbitChunk4 Int Int
  | SbitChunk6 Int Int Int Int


type alias SpltChunk =
  { paletteName : String
  , sampleDepth : Int
  , values : List SpltValue
  }


type alias SpltValue =
  { red : Int
  , green : Int
  , blue : Int
  , alpha : Int
  , frequency : Int
  }


type alias HistChunk =
  { values : List Int
  }


type alias TimeChunk =
  { year : Int
  , month : Int
  , date : Int
  , hour : Int
  , minute : Int
  , second : Int
  }


type alias UnknownChunk =
  { id : String
  }


type alias IdatChunk =
  { length : Int
  , position: Int
  }


type alias OtherChunk =
  { id : String
  }


png : Decoder Png
png =
  succeed identity
    |. symbolInt [ 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A ]
    |= chainChunks (succeed init)


chainChunks : Decoder Png -> Decoder Png
chainChunks decoder =
  decoder
    |> andThen (\png ->
      if png.iend then
        succeed png
      else
        chainChunks (chunk png)
    )


chunk : Png -> Decoder Png
chunk png =
  uint32BE
    |> andThen (\length -> string 4
    |> andThen (\id ->
      Dict.get id chunkProfiles
        |> Maybe.map (\prof -> prof.bodyDecoder length png |. skip 4)
        |> Maybe.withDefault (fail ("unknown id: " ++ id))
    ))


chunkProfiles : Dict String ChunkProfile
chunkProfiles =
  [ ihdrChunk
  , plteChunk
  , gamaChunk
  , chrmChunk
  , srgbChunk
  , iccpChunk
  , textChunk
  , ztxtChunk
  , itxtChunk
  , bkgdChunk
  , physChunk
  , sbitChunk
  , spltChunk
  , histChunk
  , timeChunk
  , idatChunk
  , iendChunk
  , otherChunk "fRAc"
  , otherChunk "gIFg"
  , otherChunk "gIFt"
  , otherChunk "gIFx"
  , otherChunk "oFFs"
  , otherChunk "pCAL"
  , otherChunk "sCAL"
  ]
  |> List.map (\prof -> (prof.id, prof))
  |> Dict.fromList


type alias ChunkProfile =
  { id : String
  , bodyDecoder : Int -> Png -> Decoder Png
  }


ihdrChunk : ChunkProfile
ihdrChunk =
  { id = "IHDR"
  , bodyDecoder = \_ png ->
      succeed IhdrChunk
        |= uint32BE
        |= uint32BE
        |= uint8
        |= uint8
        |= uint8
        |= uint8
        |= uint8
        |> map (\a -> { png | ihdr = Just a })
  }


plteChunk : ChunkProfile
plteChunk =
  { id = "PLTE"
  , bodyDecoder = \length png ->
      succeed PlteChunk
        |= repeat (length // 3) rgb
        |> map (\a -> { png | plte = Just a })
  }


rgb : Decoder (Int, Int, Int)
rgb =
  succeed (,,)
    |= uint8
    |= uint8
    |= uint8


gamaChunk : ChunkProfile
gamaChunk =
  { id = "gAMA"
  , bodyDecoder = \_ png ->
      succeed GamaChunk
        |= uint32BE
        |> map (\a -> { png | gama = Just a })
  }


chrmChunk : ChunkProfile
chrmChunk =
  { id = "cHRM"
  , bodyDecoder = \_ png ->
      succeed ChrmChunk
        |= uint32BE
        |= uint32BE
        |= uint32BE
        |= uint32BE
        |= uint32BE
        |= uint32BE
        |= uint32BE
        |= uint32BE
        |> map (\a -> { png | chrm = Just a })
  }


srgbChunk : ChunkProfile
srgbChunk =
  { id = "sRGB"
  , bodyDecoder = \_ png ->
      succeed SrgbChunk
        |= uint8
        |> map (\a -> { png | srgb = Just a })
  }


iccpChunk : ChunkProfile
iccpChunk =
  { id = "iCCP"
  , bodyDecoder = \length png ->
      given stringUntilNull (\profile ->
        succeed (IccpChunk profile)
          |= uint8
          |. skip (length - String.length profile - 2)
          |> map (\a -> { png | iccp = Just a })
      )
  }


textChunk : ChunkProfile
textChunk =
  { id = "tEXT"
  , bodyDecoder = \length png ->
      given stringUntilNull (\keyword ->
        succeed (TextChunk keyword)
          |= string (length - String.length keyword - 1)
          |> map (\a -> { png | text = a :: png.text })
      )
  }


ztxtChunk : ChunkProfile
ztxtChunk =
  { id = "zTXt"
  , bodyDecoder = \length png ->
      given stringUntilNull (\keyword ->
        succeed (ZtxtChunk keyword)
          |= uint8
          |. string (length - String.length keyword - 2)
          |> map (\a -> { png | ztxt = a :: png.ztxt })
      )
  }


itxtChunk : ChunkProfile
itxtChunk =
  { id = "iTXt"
  , bodyDecoder = \length png ->
      given position (\pos ->
        succeed ItxtChunk
          |= stringUntilNull
          |= bool
          |= uint8
          |= stringUntilNull
          |= stringUntilNull
          |= given position (\start ->
            string (pos + length - start)
          )
      )
      |> map (\a -> { png | itxt = a :: png.itxt })
  }


bkgdChunk : ChunkProfile
bkgdChunk =
  { id = "bKGD"
  , bodyDecoder = \length png ->
    png.ihdr
      |> Maybe.map .colorType
      |> Maybe.map (\colorType ->
        ( if colorType == 3 then
            succeed IndexedColor
              |= uint8
          else if colorType == 0 || colorType == 4 then
            succeed GrayScale
              |= uint16BE
          else if colorType == 2 || colorType == 6 then
            succeed TrueColor
              |= uint16BE
              |= uint16BE
              |= uint16BE
          else
            fail ("unknown colorType: " ++ toString colorType)
        )
        |> map (\a -> { png | bkgd = Just a })
      )
      |> Maybe.withDefault (fail "bKGD came before IHDR")
  }


physChunk : ChunkProfile
physChunk =
  { id = "pHYs"
  , bodyDecoder = \length png ->
      succeed PhysChunk
        |= uint32BE
        |= uint32BE
        |= choose [(0, False), (1, True)]
      |> map (\a -> { png | phys = Just a })
  }


sbitChunk : ChunkProfile
sbitChunk =
  { id = "sBIT"
  , bodyDecoder = \length png ->
      png.ihdr
        |> Maybe.map .colorType
        |> Maybe.map (\colorType ->
          ( if colorType == 3 then
              succeed SbitChunk3
                |= uint8
                |= uint8
                |= uint8
            else if colorType == 0 then
              succeed SbitChunk0
                |= uint8
            else if colorType == 2 then
              succeed SbitChunk2
                |= uint8
                |= uint8
                |= uint8
            else if colorType == 4 then
              succeed SbitChunk4
                |= uint8
                |= uint8
            else if colorType == 6 then
              succeed SbitChunk6
                |= uint8
                |= uint8
                |= uint8
                |= uint8
            else
              fail ("unknown colorType: " ++ toString colorType)
          )
          |> map (\a -> { png | sbit = Just a })
        )
        |> Maybe.withDefault (fail "sBIT came before IHDR")
  }


spltChunk : ChunkProfile
spltChunk =
  { id = "sPLT"
  , bodyDecoder = \length png ->
      given stringUntilNull (\paletteName ->
      given uint8 (\depth ->
        let
          restLength =
            length - String.length paletteName - 2

          values =
            if depth == 8 then
              repeat (restLength // 6) (spltValue False)
            else if depth == 16 then
              repeat (restLength // 10) (spltValue True)
            else
              fail ""
        in
          succeed (SpltChunk paletteName depth)
            |= values
      ))
      |> map (\a -> { png | splt = a :: png.splt })
  }


spltValue : Bool -> Decoder SpltValue
spltValue doubled =
  succeed SpltValue
    |= (if doubled then uint16BE else uint8)
    |= (if doubled then uint16BE else uint8)
    |= (if doubled then uint16BE else uint8)
    |= (if doubled then uint16BE else uint8)
    |= uint16BE


histChunk : ChunkProfile
histChunk =
  { id = "hIST"
  , bodyDecoder = \length png ->
      succeed HistChunk
        |= repeat (length // 2) uint16BE
      |> map (\a -> { png | hist = Just a })
  }


timeChunk : ChunkProfile
timeChunk =
  { id = "tIME"
  , bodyDecoder = \length png ->
      succeed TimeChunk
        |= uint16BE
        |= uint8
        |= uint8
        |= uint8
        |= uint8
        |= uint8
      |> map (\a -> { png | time = Just a })
  }


idatChunk : ChunkProfile
idatChunk =
  { id = "IDAT"
  , bodyDecoder = \length png ->
      succeed (IdatChunk length)
        |= position
        |. skip length
      |> map (\a -> { png | data = a :: png.data })
  }


iendChunk : ChunkProfile
iendChunk =
  { id = "IEND"
  , bodyDecoder = \length png ->
      succeed { png | iend = True }
  }


otherChunk : String -> ChunkProfile
otherChunk id =
  { id = id
  , bodyDecoder = \length png ->
      succeed (OtherChunk id)
        |. skip length
        |> map (\a ->  { png | others = a :: png.others })
  }


{-

TODO: Valdation is not implemented.

Each chunk's right position is like below.

A: PLTE ~ IDAT
B: ~ PLTE IDAT
C: IHDR ~ IEND
D: ~ IDAT

tRNS: A
gAMA: B
cHRM: B
sRGB: B
iCCP: B
tEXT: C
bKGD: A
pHYs: D
sBIT: B
sPLT: D
hIST: A if colorType == 3
tIME: C
---
fRAc: C
gIFg: C
gIFt: C
gIFx: C
oFFs: D
pCAL: D
sCAL: D

-}
