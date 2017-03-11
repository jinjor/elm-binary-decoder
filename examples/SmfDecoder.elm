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
    |= uint32BE
    |= dataList


dataList : Decoder (List (Int, Data))
dataList =
  dataListHelp -1 []
    |> map List.reverse


dataListHelp : Int -> List (Int, Data) -> Decoder (List (Int, Data))
dataListHelp prevStatus prev =
  given (deltaTimeAndData prevStatus) (\(prevStatus, d) ->
    case d of
      (_, End) ->
        succeed (d :: prev)

      _ ->
        dataListHelp prevStatus (d :: prev)
    )


deltaTimeAndData : Int -> Decoder (Int, (Int, Data))
deltaTimeAndData prevStatus =
  given deltaTime (\dtime ->
  given uint8 (\status ->
    if status // 16 < 8 then
      if prevStatus >= 0 then
        data prevStatus status
          |> map (\data -> (prevStatus, (dtime, data)))
      else
        fail "Running Status needs previous data."
    else
      given uint8 (data status)
        |> map (\data -> (status, (dtime, data)))
  ))


data : Int -> Int -> Decoder Data
data status first =
  if status // 16 == 8 then
    succeed (NoteOff (status % 16) first)
      |. skip 1
  else if status // 16 == 9 then
    succeed (\vel ->
        if vel == 0 then
          NoteOff (status % 16) first
        else
          NoteOn (status % 16) first vel
      )
      |= uint8
  else if status // 16 == 0xA then
    succeed (KeyPressure (status % 16) first)
      |= uint8
  else if status // 16 == 0xB then
    succeed (ControlChange (status % 16) first)
      |= uint8
  else if status // 16 == 0xC then
    succeed (ProgramChange (status % 16) first)
  else if status // 16 == 0xD then
    succeed (ChannelPressure (status % 16) first)
  else if status // 16 == 0xE then
    succeed (\msb -> PitchWheelChange (status % 16) 0) -- TODO
      |= uint8
  else if status // 16 == 0xF then
    meta first
  else
    fail ("unknown data type: 0x" ++ Hex.toString status)


meta : Int -> Decoder Data
meta tipe =
  given uint8 (\length ->
    if tipe == 0x2F then
      succeed End
    else
      succeed (Meta tipe)
        |. skip length
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
