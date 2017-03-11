module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder
import Mp3Decoder
import WaveDecoder
import PngDecoder
import ErrorFormatter
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
  { result : Result (Error, ArrayBuffer) String }


type FileType
  = Png
  | Smf


type Msg
  = GotFile FileType File
  | ReadBuffer FileType (Result File.Error ArrayBuffer)


init : (Model, Cmd Msg)
init =
  (Model (Ok ""), Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFile tipe file ->
      ( model
      , Task.attempt (ReadBuffer tipe) (File.readFileAsArrayBuffer file)
      )

    ReadBuffer tipe (Ok buf) ->
      let
        result =
          case tipe of
            Png -> Result.map toString <| Byte.decode PngDecoder.png buf
            Smf -> Result.map toString <| Byte.decode SmfDecoder.smf buf
      in
        ({ model |
          result =
            result |> Result.mapError (\e -> (e, buf))
        }, Cmd.none)

    ReadBuffer _ (Err e) ->
      Debug.crash "failed to read arrayBuffer"


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "PNG Decoder" ]
    , fileLoadButton "image/png" (GotFile Png)
    , h2 [] [ text "MIDI Decoder" ]
    , fileLoadButton "audio/mid" (GotFile Smf)
    , case model.result of
        Ok s -> div [] [ text s ]
        Err (e, buf) -> ErrorFormatter.print buf e
    ]


fileLoadButton : String -> (File -> msg) -> Html msg
fileLoadButton accept_ tagger =
  input
    [ type_ "file"
    , accept accept_
    , on "change" (File.targetFile tagger)
    ]
    [ text "load" ]
