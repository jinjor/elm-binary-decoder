module BinaryDecoder.GenericDecoder exposing (..)


type alias State s =
  { position : Int
  , context : List (Int, String)
  , source : s
  }


type alias Error =
  { position : Int
  , labels : List (Int, String)
  , message : String
  }


type GenericDecoder s a
  = GenericDecoder (State s -> Result Error (State s, a))


decode : GenericDecoder s a -> s -> Result Error a
decode (GenericDecoder f) source =
  f (State 0 [] source)
    |> Result.map Tuple.second
