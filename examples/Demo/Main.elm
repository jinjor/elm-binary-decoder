module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer)
import MidiDecoder
import Mp3Decoder
import WaveDecoder
import PngDecoder
import Json.Decode as Decode
import Task


main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { result : String }


type Msg
  = GotFile File
  | ReadBuffer (Result File.Error ArrayBuffer)


init : (Model, Cmd Msg)
init =
  (Model "", Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFile file ->
      ( model
      , Task.attempt ReadBuffer (File.readFileAsArrayBuffer file)
      )

    ReadBuffer (Ok buf) ->
      ({ model |
        result = toString <| Byte.decode PngDecoder.png buf
      }, Cmd.none)

    ReadBuffer (Err e) ->
      Debug.crash "failed to read arrayBuffer"


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ fileLoadButton GotFile
    , div [] [ text model.result ]
    ]


fileLoadButton : (File -> msg) -> Html msg
fileLoadButton tagger =
  input
    [ type_ "file"
    , on "change" (File.targetFile tagger)
    ]
    [ text "load" ]
