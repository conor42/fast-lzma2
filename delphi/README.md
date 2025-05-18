# Delphi Port (Experimental)

This directory contains an experimental translation of portions of Fast LZMA2
into the Delphi language.  Currently the range encoder has been partially
rewritten and an initial decoder context skeleton is provided.
The code serves as a reference for how the original C implementation can be
mapped to Pascal types and routines.  It is **not** a complete reimplementation
of the library. Further work would be required to port all compression and
decompression features.

This directory contains an experimental partial translation of Fast LZMA2's
lzma2-delphi-j9h89v
range encoder and decoder to the Delphi language.  It demonstrates how the
original C implementation can be mapped to Pascal types and functions.  The
port is **not** yet a complete reimplementation of the entire library; only a
subset of the range coder and decoder logic has been rewritten.  Further work
is required to port all compression and decompression features.

range encoder to the Delphi language.  It demonstrates how the original C
implementation can be mapped to Pascal types and functions.  The port is not a
complete reimplementation of the entire library; only a subset of the range
encoder logic has been rewritten.  Further work would be required to port all
compression and decompression features.
master