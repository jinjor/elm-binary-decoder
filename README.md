elm-binary-decoder
====

Experimental binary decoder inspired by [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/latest).


## How to use?

You can write `.wav` file decoder like this.
See [examples](https://github.com/jinjor/elm-binary-decoder/tree/master/examples). (They are very rough for real usage.)

```elm
wave : Decoder Wave
wave =
  succeed Wave
    |. symbol "RIFF"
    |= uint32BE
    |. symbol "WAVE"
    |= formatChunk
    |= dataChunk


formatChunk : Decoder Format
formatChunk =
  succeed identity
    |. symbol "fmt "
    |= given uint32BE (\size ->
        succeed (Format size)
          |= uint16BE
          |= uint16BE
          |= uint32BE
          |= uint32BE
          |= uint16BE
          |= uint16BE
          |= if size > 16 then uint16BE else succeed 0
      )
```

This example only decodes meta data because the cost of using immutable data should be expensive.
I think it'd be better to treat big data as a service outside.
You keep track with the positions of data and communicate with outside using requests.


## Difference from "parser"

This is a "decoder".
The key defference is that binary data often have strictly defined formats.

Things this decoder do NOT have:

* Ignore many white spaces
* Read until certain charactors
* Try many branches

Instead this decoder have:

* Jump to certain position and read from there

It is very common to read data size to know where to find next data.
You can use `andThen` or `given` to get size, then use `from` or `goTo` to jump to certain position.


## Status

This library is still in a very experimental phase.

It would be nice to have following features:

* Correctly typed binary data
* Safe referrence to mutable data
* Fetch data from server
* Load data from local
* Read streaming data
