# Delphi Port (Experimental)

This directory contains an experimental translation of portions of Fast LZMA2
into the Delphi language.  Currently the range encoder has been partially
rewritten and an initial decoder context skeleton is provided.
The code serves as a reference for how the original C implementation can be
mapped to Pascal types and routines.  It is **not** a complete reimplementation
of the library. Further work would be required to port all compression and
decompression features.
