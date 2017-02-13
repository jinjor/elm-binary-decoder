module Mp3Decoder exposing (..)


import Bitwise
import Char
import BinaryDecoder exposing (..)
import BinaryDecoder.Byte exposing (..)
import BinaryDecoder.Bit exposing (..)


type alias Mp3 =
  { tagId3v2 : TagId3v2
  }


type alias TagId3v2 =
  { header : TagId3v2Header
  , expansion : Maybe ExpansionHeader
  , frames : List TagId3v2Frame
  }


type alias ExpansionHeader =
  {}


type alias TagId3v2Header =
  { majorVersion : Int
  , minorVersion : Int
  , sync : Bool
  , expansion : Bool
  , experimental : Bool
  , footer : Bool
  , size : Int
  }


type alias TagId3v2Footer =
  {}



type alias FrameHeader =
  { version : Version
  , layer : Layer
  , protection : Bool
  , bitRate : Int
  , sampleRate : Int
  , padding : Bool
  , extension : Bool
  , channelMode : ChannelMode
  , copyright : Bool
  , original : Bool
  , emphasis : String
  }


type Version
  = MPEGv25
  | MPEGv2
  | MPEGv1


type Layer
  = Layer3
  | Layer2
  | Layer1


type ChannelMode
  = JointStereo Int
  | Stereo
  | DualChannel
  | SingleChannel


tagId3v2 : Decoder TagId3v2
tagId3v2 =
  given tagId3v2Header (\header ->
    succeed (TagId3v2 header)
      |= ( if header.expansion then
             expansionHeader |> map Just
           else
             succeed Nothing )
      |= repeatUntil "break-frame-header-id" tagId3v2Frame
      |. skip ( if header.footer then 10 else 0 )
    )


tagId3v2Header : Decoder TagId3v2Header
tagId3v2Header =
  succeed TagId3v2Header
    |. symbol "ID3"
    |= uint8
    |= uint8
    |+ (\f ->
        bits 1 <|
          succeed f
            |= bool
            |= bool
            |= bool
            |= bool
        )
    |= syncSafeInt


expansionHeader : Decoder ExpansionHeader
expansionHeader =
  given uint32BE (\size ->
    succeed ExpansionHeader
      |. skip size
  )


type alias TagId3v2Frame =
  { header : TagId3v2FrameHeader
  , body : TagId3v2FrameBody
  }


type alias TagId3v2FrameHeader =
  { id : String -- UFID, TIT2, TPE1, TRCK, MCDI,...
  , size : Int
  , flags : TagId3v2FrameHeaderFlags -- from 2.3
  }


type alias TagId3v2FrameHeaderFlags =
  { a1 : Bool
  , a2 : Bool
  , a3 : Bool
  , a4 : Bool
  , a5 : Bool
  , a6 : Bool
  , a7 : Bool
  , a8 : Bool
  }


type alias TagId3v2FrameBody =
  {}



tagId3v2Frame : Decoder TagId3v2Frame
tagId3v2Frame =
  given tagId3v2FrameHeader (\header ->
    succeed (TagId3v2Frame header)
      |. skip header.size
      |= succeed TagId3v2FrameBody
    )


tagId3v2FrameHeader : Decoder TagId3v2FrameHeader
tagId3v2FrameHeader =
  succeed TagId3v2FrameHeader
    |= tagId3v2FrameHeaderId
    |= uint32BE
    |= tagId3v2FrameHeaderFlags -- from v2.3


tagId3v2FrameHeaderId : Decoder String
tagId3v2FrameHeaderId =
  uint8
    |> andThen (\i ->
        if 48 <= i && i <= 57 || 65 <= i && i <= 90 then
          succeed (Char.fromCode i)
        else
          fail "break-frame-header-id"
      )
    |> repeat 4
    |> map String.fromList


tagId3v2FrameHeaderFlags : Decoder TagId3v2FrameHeaderFlags
tagId3v2FrameHeaderFlags =
  bits 2 <|
    succeed TagId3v2FrameHeaderFlags
      |. goTo 1
      |= bool
      |= bool
      |= bool
      |. goTo 9
      |= bool
      |. goTo 12
      |= bool
      |= bool
      |= bool
      |= bool


frameHeader : Decoder FrameHeader
frameHeader =
  bits 4 <|
    succeed FrameHeader
      |. ones 11
      |= choose 2 [ (0, MPEGv25), (2, MPEGv2), (3, MPEGv1) ]
      |= choose 2 [ (1, Layer3), (2, Layer2), (3, Layer1) ]
      |= bool
      |= int 4
      |= int 2
      |= bool
      |= bool
      |= channelMode
      |= bool
      |= bool
      |= choose 2 [ (1, "50/15"), (2, ""), (3, "CCIT J.17") ]


channelMode : BitDecoder ChannelMode
channelMode =
  given (int 2) (\i ->
    if i == 1 then
      succeed JointStereo
        |= int 2
    else
      succeed (
        if i == 0 then
          Stereo
        else if i == 2 then
          DualChannel
        else
          SingleChannel
      )
        |. zeros 2
  )


syncSafeInt : Decoder Int
syncSafeInt =
  succeed (\a b c d -> a + b + c + d)
    |= map (Bitwise.and 255 >> Bitwise.shiftLeftBy 21) uint8
    |= map (Bitwise.and 255 >> Bitwise.shiftLeftBy 14) uint8
    |= map (Bitwise.and 255 >> Bitwise.shiftLeftBy 7) uint8
    |= map (Bitwise.and 255) uint8
