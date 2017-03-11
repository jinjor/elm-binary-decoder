module SmfDecoder exposing (..)


import Bitwise
import BinaryDecoder.Byte exposing (..)
import BinaryDecoder exposing (..)
import Hex


type alias Smf =
  { header : Header
  , tracks : List Track
  }


type alias Header =
  { length : Int
  , format : Int
  , trackNumber : Int
  , timeBase : Int
  }


type alias Track =
  { length : Int
  , values : List (Int, Data)
  }


type alias Channel = Int
type alias Note = Int

type Data
  = NoteOn Channel Note Int
  | NoteOff Channel Note
  | KeyPressure Channel Int Int
  | ControlChange Channel Int Int
  | ProgramChange Channel Int
  | ChannelPressure Channel Int
  | PitchWheelChange Channel Int
  | Meta Int
  | End



smf : Decoder Smf
smf =
  given header (\header ->
    succeed (Smf header)
      |= from (header.length + 8) (repeat header.trackNumber track)
  )


header : Decoder Header
header =
  succeed Header
    |. symbol "MThd"
    |= uint32BE
    |= uint16BE
    |= uint16BE
    |= uint16BE


track : Decoder Track
track =
  succeed Track
    |. symbol "MTrk"
    |= map (Debug.log "length") uint32BE
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
        if num // 16 < 8 then
          fail "Running Status not implemented."
        else if num // 16 == 8 then
          succeed (NoteOff (num % 16))
            |= uint8
            |. uint8
        else if num // 16 == 9 then
          succeed (\note vel ->
              if vel == 0 then
                NoteOff (num % 16) note
              else
                NoteOn (num % 16) note vel
            )
            |= uint8
            |= uint8
        else if num // 16 == 0xA then
          succeed (KeyPressure (num % 16))
            |= uint8
            |= uint8
        else if num // 16 == 0xB then
          succeed (ControlChange (num % 16))
            |= uint8
            |= uint8
        else if num // 16 == 0xC then
          succeed (ProgramChange (num % 16))
            |= uint8
        else if num // 16 == 0xD then
          succeed (ChannelPressure (num % 16))
            |= uint8
        else if num // 16 == 0xE then
          succeed (\lsb msb -> PitchWheelChange (num % 16) 0) -- TODO
            |= uint8
            |= uint8
        else if num // 16 == 0xF then
          meta
        else
          fail ("unknown data type: 0x" ++ Hex.toString num)
      )


meta : Decoder Data
meta =
  given uint8 (\tipe ->
  given uint8 (\length ->
    if tipe == 0x2F then
      succeed End
    else
      succeed (Meta tipe)
        |. skip length
  ))


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
