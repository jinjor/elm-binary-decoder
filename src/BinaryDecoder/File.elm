module BinaryDecoder.File exposing
  ( FileList, File
  , fileList, targetFile, targetFiles
  , getAt, readFileAsArrayBuffer, fetchArrayBuffer
  )


{-| Use this module to get ArrayBuffer.

```
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotFile file ->
      ( model
      , Task.attempt ReadBuffer (File.readFileAsArrayBuffer file)
      )

    ReadBuffer (Ok arrayBuffer) ->
      ...

    ReadBuffer (Err error) ->
      ...
```

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import BinaryDecoder.Byte exposing (ArrayBuffer)
import Native.BinaryDecoder
import Task exposing (Task)


{-| [FileList](https://developer.mozilla.org/en-US/docs/Web/API/FileList) type from Web API.
-}
type FileList = FileList


{-| [File](https://developer.mozilla.org/en-US/docs/Web/API/File) type from Web API.
-}
type File = File


{-| Decode JSON value as FileList.
-}
fileList : Decoder FileList
fileList =
  Decode.value
    |> Decode.andThen (\json ->
      case toFileList json of
        Ok fileList -> Decode.succeed fileList
        Err s -> Decode.fail s
    )


{-| Get `.target.files` and take the first file.

Typical usage:

```elm
fileLoadButton : String -> (File -> msg) -> Html msg
fileLoadButton accept_ tagger =
  input
    [ type_ "file"
    , accept accept_
    , on "change" (File.targetFile tagger)
    ]
    [ text "load" ]
```

-}
targetFile : (File -> msg) -> Decoder msg
targetFile tagger =
  Decode.at ["target", "files"] fileList
    |> Decode.andThen (\fileList ->
      getAt 0 fileList
        |> Maybe.map (tagger >> Decode.succeed)
        |> Maybe.withDefault (Decode.fail "file list is empty")
    )


{-| Decode `.target.files`.
-}
targetFiles : (FileList -> msg) -> Decoder msg
targetFiles tagger =
  Decode.map tagger <|
    Decode.at ["target", "files"] fileList



toFileList : Encode.Value -> Result String FileList
toFileList =
  Native.BinaryDecoder.toFileList


{-| Get file from given index of FileList.
-}
getAt : Int -> FileList -> Maybe File
getAt =
  Native.BinaryDecoder.getAt


{-| Read file as ArrayBuffer.
-}
readFileAsArrayBuffer : File -> Task String ArrayBuffer
readFileAsArrayBuffer =
  Native.BinaryDecoder.readFileAsArrayBuffer


{-| Send HTTP request (GET) and get ArrayBuffer.
-}
fetchArrayBuffer : String -> Task String ArrayBuffer
fetchArrayBuffer =
  Native.BinaryDecoder.fetchArrayBuffer
