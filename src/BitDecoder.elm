module BitDecoder exposing (BitDecoder, decode, succeed, (||=), (||.), (||+), int, bool)


import Bitwise


type alias Context =
  { position : Int
  , source : Int
  }



type BitDecoder a
  = BitDecoder (Context -> Result String (Context, a))


decode : BitDecoder a -> Int -> Result String a
decode (BitDecoder f) int =
  f (Context 0 int)
    |> Result.map Tuple.second



-- READ BITS


bitAt : Int -> BitDecoder Bool
bitAt n =
  if n < 0 || n >= 8 then
    fail ("out of index: " ++ toString n)
  else
    BitDecoder (\context ->
      context.source
        |> Bitwise.shiftRightBy (7 - n)
        |> Bitwise.and 1
        |> (\a -> a > 0)
        |> ((,) context)
        |> Ok
      )



-- PRIMITIVE


succeed : a -> BitDecoder a
succeed a =
  BitDecoder (\context -> Ok (context, a))


fail : String -> BitDecoder a
fail s =
  BitDecoder (\context -> Err s)



-- COMBINATOR


(||=) : BitDecoder (a -> b) -> BitDecoder a -> BitDecoder b
(||=) transformer decoder =
  transformer
    |> andThen (\f ->
      map f decoder
    )


(||.) : BitDecoder a -> BitDecoder x -> BitDecoder a
(||.) decoder ignored =
  decoder
    |> andThen (\a ->
      map (\_ -> a) ignored
    )


(||+) : BitDecoder a -> (a -> BitDecoder b) -> BitDecoder b
(||+) =
  flip andThen


infixl 5 ||=
infixl 5 ||+
infixl 5 ||.


andThen : (a -> BitDecoder b) -> BitDecoder a -> BitDecoder b
andThen f (BitDecoder f_) =
  BitDecoder (\context ->
    f_ context
      |> Result.andThen (\(context_, a) ->
        let
          (BitDecoder decode) =
            f a
        in
          decode context_
      )
    )


given : BitDecoder a -> (a -> BitDecoder b) -> BitDecoder b
given =
  flip andThen


map : (a -> b) -> BitDecoder a -> BitDecoder b
map f (BitDecoder f_) =
  BitDecoder (\context ->
    f_ context
      |> Result.map (\(context, a) ->
        (context, f a)
      )
    )



-- UTILITY


match : List Int -> BitDecoder ()
match ints =
  Debug.crash "not implemented."


zeros : Int -> BitDecoder ()
zeros length =
  match (List.repeat length 0)


ones : Int -> BitDecoder ()
ones length =
  match (List.repeat length 1)


int : Int -> BitDecoder Int
int length =
  if length < 0 then
    fail ("invalid length " ++ toString length)
  else
    BitDecoder (\context ->
      intHelp 0 context.position length
        |> Result.map ((,) { context | position = context.position + length})
    )


intHelp : Int -> Int -> Int -> Result String Int
intHelp prev from length =
  Debug.crash "not implemented."


bool : BitDecoder Bool
bool =
  map (\i -> i > 0) (int 1)
