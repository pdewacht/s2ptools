# S2PPATCH

S2PPATCH can patch games to directly support the S2P. After
patching, configure the game for FIXME.

Caveats:

- Patching can break support for real sound cards (including unrelated
  cards).
- Patched games might only work on the computer where the patch was
  done. This is because the patch hardcodes the parallel port I/O
  address, which might vary from computer to computer.

Pros compared to the SOFTMPU:

- No need for 386 CPU
- Can work with protected mode games

Cons:

- Patches need to be written for each game by hand

## Compressed EXEs

DOS software often shipped with compressed EXEs. Such EXEs need to be
unpacked before they can be patched. S2PPATCH will detect the more
common compression schemes and warn about them ("This file might be
compressed"). If you see such a warning, you should try decompressing
it and running S2PPATCH again. I recommend [Ben Castricum's
UNP](http://unp.bencastricum.nl/) decompressor, it will handle pretty
much all formats.

## Working games


## Compilation

To compile a DOS executable using OpenWatcom, use the `build.sh`
script. To compile with GCC, use the `buildunix.sh` script.

You'll also need Python 3, the NASM assembler and [the Ragel state
machine compiler][Ragel].

[Ragel]: https://www.colm.net/open-source/ragel/
