# S2PPATCH

S2PPATCH can patch games to directly support the S2P. After patching,
configure the game for MIDI output.

This is an alternative to using the SoftMPU TSR.

Pros compared to the SoftMPU:

- No need for a 386 CPU.
- Can work with protected mode games.

Cons:

- Patches need to be written for each game by hand. This can be rather
  hard, so few games are supported.


## Working games

- Doom and other games using the same sound engine: Heretic, Hexen,
  Raptor: Call of the Shadows, etc.

- Games that use the Miles Audio Interface Library (both 16-bit and
  32-bit).

- Most Sierra games, except for the oldest ones.

For Miles & Sierra games, the file to be patched is the MIDI
driver. This file is usually named something like 'MT32' or 'MIDI'.


## Caveats

Patching will break support for regular MIDI output.

Patched games might only work on the computer where the patch was
done. This is because the patch hardcodes the parallel port I/O
address, which might vary from computer to computer.


## Compilation

To compile a DOS executable using OpenWatcom, use the `build.sh`
script. To compile with GCC, use the `buildunix.sh` script.

You'll also need Python 3, the NASM assembler and [the Ragel state
machine compiler][Ragel].

[Ragel]: https://www.colm.net/open-source/ragel/
