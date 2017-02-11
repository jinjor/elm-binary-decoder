module BinaryDecoder.GenericDecoder exposing (..)


type alias Context s =
  { position : Int
  , source : s
  }


type alias Error =
  { position : Int
  , message : String
  }


type GenericDecoder s a
  = GenericDecoder (Context s -> Result Error (Context s, a))


decode : GenericDecoder s a -> s -> Result Error a
decode (GenericDecoder f) source =
  f (Context 0 source)
    |> Result.map Tuple.second
