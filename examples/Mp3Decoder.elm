module Mp3Decoder exposing (..)


import BinaryDecoder exposing (..)
import BinaryDecoder.Byte exposing (..)
import BinaryDecoder.Bit exposing (..)


type alias Mp3 =
  { tagId3v2 : TagId3v2
  }


type alias TagId3v2 =
  { header : TagId3v2Header
  }


type alias TagId3v2Header =
  { majorVersion : Int
  , minorVersion : Int
  , sync : Bool
  , expansion : Bool
  , experimental : Bool
  , footer : Bool
  , size : Int
  }


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
    |= uint32BE


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
