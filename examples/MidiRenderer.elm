module MidiRenderer exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import SmfDecoder as Smf exposing (Smf, MidiEvent(..))


type alias Midi =
  { tracks : List Track
  }


type alias Track =
  { name : String
  , notes : List Note
  }


type alias Note =
  { position : Int
  , note : Int
  , length : Int
  }


emptyTrack : Track
emptyTrack =
  Track "" []


renderSmf : Smf -> Html msg
renderSmf smf =
  fromSmf smf
    |> renderMidi


renderMidi : Midi -> Html msg
renderMidi midi =
  midi.tracks
    |> List.map renderTrack
    |> div []


renderTrack : Track -> Html msg
renderTrack track =
  track.notes
    |> List.map renderNote
    |> div [ style trackStyle ]


trackStyle : List (String, String)
trackStyle =
  [ ("position", "relative")
  , ("background-color", "black")
  , ("height", "60px")
  , ("margin-bottom", "2px")
  ]


renderNote : Note -> Html msg
renderNote note =
  div [ style (noteStyle note) ] []


noteStyle : Note -> List (String, String)
noteStyle note =
  [ ("position", "absolute")
  , ("left", px <| note.position // 100)
  , ("top", px <| 60 - (note.note - 30) )
  , ("height", "1px")
  , ("width", px <| note.length // 100)
  , ("background-color", "pink")
  ]


px : a -> String
px num =
  toString num ++ "px"


fromSmf : Smf -> Midi
fromSmf smf =
  smf.tracks
    |> List.map fromSmfTrack
    |> Midi


fromSmfTrack : Smf.Track -> Track
fromSmfTrack track =
  track.events
    |> List.foldl updateTrack (0, initContext)
    |> Tuple.second
    |> .notes
    |> List.reverse
    |> (\notes -> Track "" notes)


updateTrack : (Int, MidiEvent) -> (Int, Context) -> (Int, Context)
updateTrack (dtime, e) (position, context) =
  ( position + dtime
  , case e of
      NoteOn ch note vel ->
        { context
          | temporaryNotes =
              Dict.insert note (position + dtime, vel) context.temporaryNotes
        }

      NoteOff ch note ->
        { context
          | temporaryNotes =
              Dict.remove note context.temporaryNotes
          , notes =
              Dict.get note context.temporaryNotes
                |> Maybe.map (\(startPos, vel) ->
                    Note startPos note (position + dtime - startPos)
                      :: context.notes
                  )
                |> Maybe.withDefault context.notes
        }

      _ ->
        context
  )


type alias Context =
  { temporaryNotes : Dict Int (Int, Int)
  , notes : List Note
  }


initContext : Context
initContext =
  Context Dict.empty []
