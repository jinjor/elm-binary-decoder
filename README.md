elm-binary-decoder
====

[![Build Status](https://travis-ci.org/jinjor/elm-binary-decoder.svg)](https://travis-ci.org/jinjor/elm-binary-decoder)

Experimental binary decoder inspired by [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/latest).


## How to use?

You can write `.wav` file decoder like this.
See [examples](https://github.com/jinjor/elm-binary-decoder/tree/master/examples). (They are too rough for real usage.)

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


## Bytes and Bits

You can use two decoders together.

This is an example of Mp3Decoder.

```elm
tagId3v2FrameHeaderFlags : Decoder TagId3v2FrameHeaderFlags
tagId3v2FrameHeaderFlags =
  -- This is (Btye) Decoder
  bits 2 <|
    -- and, this is BitDecoder.
    succeed TagId3v2FrameHeaderFlags
      |. goTo 1
      |= Bit.bool
      |= Bit.bool
      |= Bit.bool
      |. goTo 9
      |= Bit.bool
      |. goTo 12
      |= Bit.bool
      |= Bit.bool
      |= Bit.bool
      |= Bit.bool
```

While "Btye" decoder decodes the whole binary data, "Bit" decoder only decodes an Int value.


## What's special about this library?

Binary format offen give you the size of data before you read it.
So, there are many useful functions for using these meta information.

* Jump to certain position and read from there
* Read bytes until certain position


## Status

This library is still in a very experimental phase.

It would be nice to have following features:

* Fetch data from server
* Load data from local
* Read streaming data
