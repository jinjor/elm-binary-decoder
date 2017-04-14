module BinaryDecoder exposing
  ( succeed, fail
  , (|=), (|.)
  , andThen, map, sequence, repeat, many
  , position, from, goTo, skip
  , lazy
  , equal, match, printError
  )


{-|

@docs succeed,fail
@docs (|=), (|.)
@docs andThen, map, sequence, repeat, many
@docs position, from, goTo, skip
@docs lazy
@docs equal, match, printError

-}


import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)



-- PRIMITIVE


{-| Create a decoder that always succeeds.
-}
succeed : a -> GenericDecoder s a
succeed a =
  GenericDecoder (\context -> Ok (context, a))


{-| Create a decoder that always failes with given error message.
-}
fail : String -> GenericDecoder s a
fail s =
  GenericDecoder (\context -> Err (Error context.position s))



-- COMBINATOR


{-|-}
(|=) : GenericDecoder s (a -> b) -> GenericDecoder s a -> GenericDecoder s b
(|=) transformer decoder =
  transformer
    |> andThen (\f ->
      map f decoder
    )


{-|-}
(|.) : GenericDecoder s a -> GenericDecoder s x -> GenericDecoder s a
(|.) decoder ignored =
  decoder
    |> andThen (\a ->
      map (\_ -> a) ignored
    )


infixl 5 |=
infixl 5 |.


{-|-}
andThen : (a -> GenericDecoder s b) -> GenericDecoder s a -> GenericDecoder s b
andThen f (GenericDecoder f_) =
  GenericDecoder (\context ->
    f_ context
      |> Result.andThen (\(context_, a) ->
        let
          (GenericDecoder decode) =
            f a
        in
          decode context_
      )
    )


{-|-}
map : (a -> b) -> GenericDecoder s a -> GenericDecoder s b
map f (GenericDecoder f_) =
  GenericDecoder (\context ->
    f_ context
      |> Result.map (\(context, a) ->
        (context, f a)
      )
    )


{-|-}
sequence : List (GenericDecoder s a) -> GenericDecoder s (List a)
sequence decoders =
  case decoders of
    [] ->
      succeed []

    x :: xs ->
      x
        |> andThen (\head ->
          sequence xs
            |> map (\tail -> head :: tail)
        )

{-|-}
repeat : Int -> GenericDecoder s a -> GenericDecoder s (List a)
repeat n decoder =
  sequence (List.repeat n decoder)



{-|-}
many : GenericDecoder s a -> GenericDecoder s (List a)
many (GenericDecoder decode) =
  GenericDecoder
    (\context ->
      manyHelp decode context
    )


manyHelp : (Context s -> Result Error (Context s, a)) -> Context s -> Result Error (Context s, List a)
manyHelp decode context =
  case decode context of
    Ok (newContext, head) ->
      manyHelp decode newContext
        |> Result.map (Tuple.mapSecond ((::) head))

    Err e ->
      Ok (context, [])


-- oneOf : List (GenericDecoder s a) -> GenericDecoder s a
-- oneOf decoders =
--   GenericDecoder (\context ->
--     oneOfHelp context decoders
--   )
--
--
-- oneOfHelp : Context s -> List (GenericDecoder s a) -> Result Error (Context s, a)
-- oneOfHelp context decoders =
--   case decoders of
--     [] ->
--       Err (Error context.position "none of decoders succeeds")
--
--     (GenericDecoder decode) :: tail ->
--       case decode context of
--         Err _ ->
--           oneOfHelp context tail
--
--         ok ->
--           ok


-- POSITION


{-| Get current cursor position.
-}
position : GenericDecoder s Int
position =
  GenericDecoder (\context ->
    Ok (context, context.position)
  )


{-| Decode from given position. The cursor returns to the original position after decoding finishes.
-}
from : Int -> GenericDecoder s a -> GenericDecoder s a
from position (GenericDecoder decode) =
  GenericDecoder
    (\context ->
      decode { context | position = position }
        |> Result.map (\(c, a) -> ({ c | position = context.position }, a))
    )


{-| Decode from given position. Contrust to `from`, the cursor does not return to the original position.
-}
goTo : Int -> GenericDecoder s ()
goTo position =
  GenericDecoder
    (\context ->
      Ok ({ context | position = position }, ())
    )


{-| Skip given length of bytes.
-}
skip : Int -> GenericDecoder s ()
skip size =
  GenericDecoder
    (\context ->
      Ok ({ context | position = context.position + size }, ())
    )



-- LAZY


{-|-}
lazy : (() -> GenericDecoder s a) -> GenericDecoder s a
lazy thunk =
  GenericDecoder (\context ->
    let
      (GenericDecoder decode) =
        thunk ()
    in
      decode context
  )



-- UTILITY


{-| Succeeds if the decoded value is exactly the same as given value.
-}
equal : a -> GenericDecoder s a -> GenericDecoder s ()
equal expected decoder =
  decoder
    |> andThen (\a ->
        if a == expected then
          succeed ()
        else
          fail ("expected " ++ toString expected ++ ", but got " ++ toString a)
      )

{-|-}
match : (a -> Bool) -> GenericDecoder s a -> GenericDecoder s ()
match isOk decoder =
  decoder
    |> andThen (\a ->
        if isOk a then
          succeed ()
        else
          fail (toString a ++ " is not expected here")
      )

{-|-}
printError : Error -> String
printError err =
  "decode failed at " ++ toString err.position ++ ":\n\n\t" ++ err.message ++ "\n"
