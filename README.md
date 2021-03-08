This is a solver for Snakebird puzzle game made by Noumenon Games.

[![asciicast](https://asciinema.org/a/j3JPqQcGQQx42nt3C1tNx092K.svg)](https://asciinema.org/a/j3JPqQcGQQx42nt3C1tNx092K)

To use, create a level file and run `cargo run --release --
<path_to_level.txt>`. See `levels/1.txt` (and others) for syntax and examples.
Note that these are not the real levels in the game - you will have to enter
the real levels yourself.

This will easily consume all your RAM and probably make your computer unhappy
on the harder levels. It currently fails to solve `*1` level, running out of
RAM after consuming ~10GB of it. Level `19` gets solved with ~8 GB memory
usage, taking ~6 minutes on author's machine.

The author of this code is not affiliated Noumenon Games. The code in this repo
contains no game assets.
