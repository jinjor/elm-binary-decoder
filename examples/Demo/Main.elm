module Main exposing (..)


import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import BinaryDecoder.File as File exposing (File)
import BinaryDecoder.Byte as Byte exposing (ArrayBuffer, Error)
import SmfDecoder exposing (Smf)
import PngDecoder exposing (Png)
import Demo.ErrorFormatter as ErrorFormatter
import Demo.MidiRenderer as MidiRenderer


main : Program Never Model Msg
main =
  program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { png : Maybe (Result (Error, ArrayBuffer) Png)
  , smf : Maybe (Result (Error, ArrayBuffer) Smf)
  }


type FileType
  = Png
  | Smf


type Msg
  = GotFile FileType File
  | ReadBuffer FileType (Result String ArrayBuffer)
  | LoadMidi


init : (Model, Cmd Msg)
init =
  (Model Nothing Nothing, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFile tipe file ->
      ( model
      , Task.attempt (ReadBuffer tipe) (File.readFileAsArrayBuffer file |> Task.mapError toString)
      )

    ReadBuffer tipe (Ok buf) ->
      case tipe of
        Png ->
          ({ model |
              png =
                Byte.decode PngDecoder.png buf
                  |> Result.mapError (\e -> (e, buf))
                  |> Just
          }, Cmd.none)

        Smf ->
          ({ model |
              smf =
                Byte.decode SmfDecoder.smf buf
                  |> Result.mapError (\e -> (e, buf))
                  |> Just
          }, Cmd.none)

    LoadMidi ->
      ( model
      , Task.attempt (ReadBuffer Smf) (File.fetchArrayBuffer "sample.mid")
      )

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
    , case model.png of
        Just (Ok png) -> div [] [ text (toString png) ]
        Just (Err (e, buf)) -> ErrorFormatter.print buf e
        _ -> text ""

    , h2 [] [ text "MIDI Decoder" ]
    , fileLoadButton "audio/mid" (GotFile Smf)
    , button [ onClick LoadMidi ] [ text "Load Sample" ]
    , case model.smf of
        Just (Ok smf) -> MidiRenderer.renderSmf smf
        Just (Err (e, buf)) -> ErrorFormatter.print buf e
        _ -> text ""
    ]


fileLoadButton : String -> (File -> msg) -> Html msg
fileLoadButton accept_ tagger =
  input
    [ type_ "file"
    , accept accept_
    , on "change" (File.targetFile tagger)
    ]
    [ text "load" ]
