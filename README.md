elm-binary-decoder
====

[![Build Status](https://travis-ci.org/jinjor/elm-binary-decoder.svg)](https://travis-ci.org/jinjor/elm-binary-decoder)

Experimental binary decoder inspired by [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/latest).


## How to use?

You can write `.wav` file decoder like this.
See [examples](https://github.com/jinjor/elm-binary-decoder/tree/master/examples). (**Caution**: Don't trust any of these implementations! I wrote them only to sho how to use this library but did not confirm if they are correct really.)

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
    |= (uint32BE |> andThen (\size ->
        succeed (Format size)
          |= uint16BE
          |= uint16BE
          |= uint32BE
          |= uint32BE
          |= uint16BE
          |= uint16BE
          |= if size > 16 then uint16BE else succeed 0
      ))
```

This example only decodes meta data because the cost of using immutable data should be expensive.


## Bytes and Bits

You can use two decoders together.

This is an example from Mp3Decoder.

```elm
tagId3v2Header : Decoder TagId3v2Header
tagId3v2Header =
  succeed TagId3v2Header
    |. symbol "ID3"
    |= uint8
    |= uint8
    |= bits 1 tagId3v2HeaderFlags -- convert BitDecoder to Decoder
    |= syncSafeInt


tagId3v2HeaderFlags : BitDecoder TagId3v2HeaderFlags
tagId3v2HeaderFlags =
  succeed TagId3v2HeaderFlags
    |= Bit.bool
    |= Bit.bool
    |= Bit.bool
    |= Bit.bool
```

While "Byte" decoder decodes the whole binary data, "Bit" decoder only decodes an Int value.


## What's special about this library?

Binary format often give you the size of data before you read it.
So, there are many useful functions for using these meta information.

* Jump to certain position and read from there
* Read bytes until certain position


## Status

This library is still in a very experimental phase.

It would be nice to have following features:

* Fetch data from server
* Load data from local
* Read streaming data
