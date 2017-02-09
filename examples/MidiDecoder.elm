module MidiDecoder exposing (..)

import BinaryDecoder exposing (..)

import Char
import Bitwise


type alias Midi =
  { header : Header
  , body : Body
  }


type alias Header =
  { length : Int
  , format : Int
  , trackNumber : Int
  , timeBase : Int
  }


type alias Body =
  { length : Int
  , dataList : List (Int, Data)
  }


type Data
  = NoteOn Int Int Int
  | NoteOff Int Int
  | ProgramChange Int Int
  | End



midi : Decoder Midi
midi =
  given header (\header ->
    succeed (Midi header)
      |= from (header.length + 8) body
  )


header : Decoder Header
header =
  succeed Header
    |. symbol "MThd"
    |= uint32BE
    |= uint16BE
    |= uint16BE
    |= uint16BE


body : Decoder Body
body =
  succeed Body
    |. symbol "MTrk"
    |= uint32BE
    |= dataList


dataList : Decoder (List (Int, Data))
dataList =
  dataListHelp []
    |> map List.reverse


dataListHelp : List (Int, Data) -> Decoder (List (Int, Data))
dataListHelp prev =
  given data (\d ->
    case d of
      (_, End) ->
        succeed (d :: prev)

      _ ->
        dataListHelp (d :: prev)
    )


data : Decoder (Int, Data)
data =
  succeed (,)
    |= deltaTime
    |= given uint8 (\num ->
        if num // 16 == 8 then
          succeed (NoteOff (num % 16))
            |= uint8
        else if num // 16 == 9 then
          succeed (NoteOn (num % 16))
            |= uint8
            |= uint8
        else if num // 16 == 12 then
          succeed (ProgramChange (num % 16))
            |= uint8
        else if num == 0xFF then
          succeed End
            |. uint8
            |. uint8
        else
          fail ("unknow data type: " ++ toString num)
      )


deltaTime : Decoder Int
deltaTime =
  deltaTimeHelp 0


deltaTimeHelp : Int -> Decoder Int
deltaTimeHelp prev =
  given uint8 (\i ->
    if Bitwise.and 0x80 i == 0x80 then
      deltaTimeHelp (Bitwise.shiftLeftBy 7 prev + Bitwise.xor 0x80 i)
    else
      succeed (Bitwise.shiftLeftBy 7 prev + i)
  )
