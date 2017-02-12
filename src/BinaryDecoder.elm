module BinaryDecoder exposing
  ( succeed, fail
  , (|=), (|.), (|+)
  , andThen, given, map, sequence, repeat
  , from, goTo, skip
  , lazy
  , match, printError
  )


{-|-}


import BinaryDecoder.GenericDecoder as GenericDecoder exposing (..)



-- PRIMITIVE


{-|-}
succeed : a -> GenericDecoder s a
succeed a =
  GenericDecoder (\context -> Ok (context, a))


{-|-}
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


{-|-}
(|+) : GenericDecoder s a -> (a -> GenericDecoder s b) -> GenericDecoder s b
(|+) =
  flip andThen


infixl 5 |=
infixl 5 |+
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
given : GenericDecoder s a -> (a -> GenericDecoder s b) -> GenericDecoder s b
given =
  flip andThen


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


repeat : Int -> GenericDecoder s a -> GenericDecoder s (List a)
repeat n decoder =
  sequence (List.repeat n decoder)



-- JUMP


{-|-}
from : Int -> GenericDecoder s a -> GenericDecoder s a
from position (GenericDecoder decode) =
  GenericDecoder
    (\context ->
      decode { context | position = position }
        |> Result.map (\(c, a) -> ({ c | position = context.position }, a))
    )


{-|-}
goTo : Int -> GenericDecoder s ()
goTo position =
  GenericDecoder
    (\context ->
      Ok ({ context | position = position }, ())
    )


{-|-}
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


{-|-}
match : a -> GenericDecoder s a -> GenericDecoder s ()
match expected decoder =
  decoder
    |> andThen (\a ->
        if a == expected then
          succeed ()
        else
          fail ("expected " ++ toString expected ++ ", but got " ++ toString a)
      )

{-|-}
printError : Error -> String
printError err =
  "decode failed at " ++ toString err.position ++ ":\n\n\t" ++ err.message ++ "\n"
