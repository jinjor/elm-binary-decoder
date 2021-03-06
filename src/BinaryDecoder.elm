module BinaryDecoder exposing
  ( succeed, fail
  , (|=), (|.)
  , andThen, map, sequence, repeat, many
  , position, from, goTo, skip
  , lazy
  , equal, match, printError
  )


{-| This module provides useful combinators that works just like [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/latest).

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
  GenericDecoder (\state -> Ok (state, a))


{-| Create a decoder that always failes with given error message.
-}
fail : String -> GenericDecoder s a
fail s =
  GenericDecoder (\state -> Err (Error state.position state.context s))



-- COMBINATOR


{-| Keep a value in a decoder pipeline.
-}
(|=) : GenericDecoder s (a -> b) -> GenericDecoder s a -> GenericDecoder s b
(|=) transformer decoder =
  transformer
    |> andThen (\f ->
      map f decoder
    )


{-| Ignore a value in a decoder pipeline.
-}
(|.) : GenericDecoder s a -> GenericDecoder s x -> GenericDecoder s a
(|.) decoder ignored =
  decoder
    |> andThen (\a ->
      map (\_ -> a) ignored
    )


infixl 5 |=
infixl 5 |.


{-| Run a decoder and then run another decoder.
-}
andThen : (a -> GenericDecoder s b) -> GenericDecoder s a -> GenericDecoder s b
andThen f (GenericDecoder f_) =
  GenericDecoder (\state ->
    f_ state
      |> Result.andThen (\(state_, a) ->
        let
          (GenericDecoder decode) =
            f a
        in
          decode state_
      )
    )


{-| Transform the result of a decoder.
-}
map : (a -> b) -> GenericDecoder s a -> GenericDecoder s b
map f (GenericDecoder f_) =
  GenericDecoder (\state ->
    f_ state
      |> Result.map (\(state, a) ->
        (state, f a)
      )
    )


{-| Apply list of decoders that all returns the same type.
-}
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


{-| Apply decoder just n times and return list.
-}
repeat : Int -> GenericDecoder s a -> GenericDecoder s (List a)
repeat n decoder =
  sequence (List.repeat n decoder)



{-| Apply decoder many times until it fails.
-}
many : GenericDecoder s a -> GenericDecoder s (List a)
many (GenericDecoder decode) =
  GenericDecoder
    (\state ->
      manyHelp decode state
    )


manyHelp : (State s -> Result Error (State s, a)) -> State s -> Result Error (State s, List a)
manyHelp decode state =
  case decode state of
    Ok (newState, head) ->
      manyHelp decode newState
        |> Result.map (Tuple.mapSecond ((::) head))

    Err e ->
      Ok (state, [])



-- POSITION


{-| Get current cursor position.
-}
position : GenericDecoder s Int
position =
  GenericDecoder (\state ->
    Ok (state, state.position)
  )


{-| Decode from given position. The cursor returns to the original position after decoding finishes.
-}
from : Int -> GenericDecoder s a -> GenericDecoder s a
from position (GenericDecoder decode) =
  GenericDecoder
    (\state ->
      decode { state | position = position }
        |> Result.map (\(c, a) -> ({ c | position = state.position }, a))
    )


{-| Decode from given position. Contrust to `from`, the cursor does not return to the original position.
-}
goTo : Int -> GenericDecoder s ()
goTo position =
  GenericDecoder
    (\state ->
      Ok ({ state | position = position }, ())
    )


{-| Skip given length of bytes.
-}
skip : Int -> GenericDecoder s ()
skip size =
  GenericDecoder
    (\state ->
      Ok ({ state | position = state.position + size }, ())
    )


{-| Give context for making nicer error message.
-}
inContext : String -> GenericDecoder s () -> GenericDecoder s ()
inContext label (GenericDecoder decode) =
  GenericDecoder
    (\state ->
      decode { state | context = (state.position, label) :: state.context }
        |> Result.map (\(c, a) -> ({ c | context = state.context }, a))
    )



-- LAZY


{-| This is needed to write recursive decoder.
-}
lazy : (() -> GenericDecoder s a) -> GenericDecoder s a
lazy thunk =
  GenericDecoder (\state ->
    let
      (GenericDecoder decode) =
        thunk ()
    in
      decode state
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


{-| Succeeds if the decoded value martches given condition.
-}
match : (a -> Bool) -> GenericDecoder s a -> GenericDecoder s ()
match isOk decoder =
  decoder
    |> andThen (\a ->
        if isOk a then
          succeed ()
        else
          fail (toString a ++ " is not expected here")
      )


{-| Make simple error message. (More helpful message should be made in the future.)
-}
printError : Error -> String
printError err =
  "decode failed at " ++ toString err.position ++ ":\n\n\t" ++ err.message ++ "\n"
