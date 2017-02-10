module Mp3Decoder exposing (..)

import BinaryDecoder exposing (..)
import BitDecoder exposing (..)

import Char
import Bitwise


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


type alias Data =
  { dataSize : Int
  }



tagId3v2Header : Decoder TagId3v2Header
tagId3v2Header =
  BinaryDecoder.succeed TagId3v2Header
    |. symbol "ID3"
    |= uint8
    |= uint8
    |+ (\f ->
        bits uint8 <|
          BitDecoder.succeed f
            ||= bool
            ||= bool
            ||= bool
            ||= bool
        )
    |= uint32BE



  -- bits uint32 (
  --   BitDecoder.succeed f
  --     ||. match [1,1,1,1,1,1,1,1,1,1,1] -- ones 11
  --     ||= given (int 2) (\i -> ...) -- choose2 MPEGv25 Rserved MPEGv2 MPEGv1
  --     ||= given (int 2) (\i -> ...) -- choose2 Rserved Layer3 Layer2 Layer1
  --     ||= bool
  --     ||= int 4
  --     ||= int 2
  --     ||= bool
  --     ||= bool
  --     ||= given (int 2) (\i ->
  --           if i == 1 then
  --             succeed JointStereo
  --               |= int 2
  --           else
  --             succeed (if i == 0 then Stereo else if i == 2 then DualChannel else SingleChannel)
  --               |. match [0,0] -- zeros 2
  --         )
  --     ||= bool
  --     ||= bool
  --     ||= given (int 2) (..) -- choose2 None 50/15 Rserved CCITT_J17
  --   )
