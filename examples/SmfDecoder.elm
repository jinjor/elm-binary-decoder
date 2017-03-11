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
  , events : List (Int, MidiEvent)
  }


type alias Channel = Int
type alias Note = Int

type MidiEvent
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
    |= eventList


eventList : Decoder (List (Int, MidiEvent))
eventList =
  eventListHelp -1 []
    |> map List.reverse


eventListHelp : Int -> List (Int, MidiEvent) -> Decoder (List (Int, MidiEvent))
eventListHelp prevStatus prev =
  given (deltaTimeAndData prevStatus) (\(prevStatus, e) ->
    case e of
      (_, End) ->
        succeed (e :: prev)

      _ ->
        eventListHelp prevStatus (e :: prev)
    )


deltaTimeAndData : Int -> Decoder (Int, (Int, MidiEvent))
deltaTimeAndData prevStatus =
  given deltaTime (\dtime ->
  given uint8 (\status ->
    if status // 16 < 8 then
      if prevStatus >= 0 then
        event prevStatus status
          |> map (\event -> (prevStatus, (dtime, event)))
      else
        fail "Running Status needs previous data."
    else
      given uint8 (event status)
        |> map (\event -> (status, (dtime, event)))
  ))


event : Int -> Int -> Decoder MidiEvent
event status first =
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


meta : Int -> Decoder MidiEvent
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
