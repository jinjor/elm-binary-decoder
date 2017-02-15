module BinaryDecoder.File exposing
  ( FileList, File, Error
  , fileList, targetFile, targetFiles
  , fileGet, readFileAsArrayBuffer
  )


{-|-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import BinaryDecoder.Byte exposing (ArrayBuffer)
import Native.BinaryDecoder
import Task exposing (Task)


{-|-}
type FileList = FileList


{-|-}
type File = File


{-|-}
fileList : Decoder FileList
fileList =
  Decode.value
    |> Decode.andThen (\json ->
      case toFileList json of
        Ok fileList -> Decode.succeed fileList
        Err s -> Decode.fail s
    )


{-|-}
targetFile : (File -> msg) -> Decoder msg
targetFile tagger =
  Decode.at ["target", "files"] fileList
    |> Decode.andThen (\fileList ->
      fileGet 0 fileList
        |> Maybe.map (tagger >> Decode.succeed)
        |> Maybe.withDefault (Decode.fail "file list is empty")
    )


{-|-}
targetFiles : (FileList -> msg) -> Decoder msg
targetFiles tagger =
  Decode.map tagger <|
    Decode.at ["target", "files"] fileList



toFileList : Encode.Value -> Result String FileList
toFileList =
  Native.BinaryDecoder.toFileList


{-|-}
fileGet : Int -> FileList -> Maybe File
fileGet =
  Native.BinaryDecoder.fileGet


{-|-}
type Error = Error


{-|-}
readFileAsArrayBuffer : File -> Task Error ArrayBuffer
readFileAsArrayBuffer =
  Native.BinaryDecoder.readFileAsArrayBuffer


-- {-|-}
-- type Progress
--   = None
--   | Some { bytes : Int, bytesExpected : Int }
--   | Fail Error
--   | Done ArrayBuffer
--
-- {-|-}
-- track : String -> (Progress -> msg) -> File -> Sub msg
