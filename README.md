
# ASMBan
A pure 16-bit x86 assembly implementation of Sokoban, complete with animations and audio.
This functions like a bootloader and works without an operating system. It can be booted in an emulator from within an OS.
It uses [Mode 13h](https://en.wikipedia.org/wiki/Mode_13h) to draw in 320x200 pixels and uses a 256-colour palette.

## Why does this exist?
This project is the culmination of many years of on and off development to create something in pure ASM that is bootable without an operating system, while not being too complex.
It exists purely because I wanted to build it out of curiousity and learn a bunch of things along the way. I hope others can learn something too!

## Tools
### Building
To assemble a bootable binary, use [NASM](https://www.nasm.us/): `nasm -fbin mbr.asm -o mbr.bin`. This will create a bootable `mbr.bin`.

### Running and debugging
#### Bochs
For debugging, I used [Bochs](https://bochs.sourceforge.io/). 2 configuration files for Bochs are supplied; `bochsrc.bxrc` for just running the project, and `bochsrc-debug.bxrc` for running it in the debugger.
To run it in Bochs, use `bochs -f bochsrc.bxrc`. For debugging, use `bochsdbg -f bochsrc-debug.bxrc`.
Note that the audio does not work properly in Bochs!

#### QEMU
Running with [QEMU](https://www.qemu.org/) is also possible; `qemu-system-i386 -drive "format=raw,file=mbr.bin,if=floppy"` - audio does not work properly in QEMU.

#### DOSBox-X
[DOSBox-X](https://dosbox-x.com/) is a bit of a weird one, but this is the only emulator I found that properly handles the audio with 2 different timers to generate something that sounds vaguely decent.
After opening DOSBox-X, first go to main > configuration tool, select PC speaker and make sure the `pcspeaker` checkbox is checked. Set the `pcrate` to `8000` for best results.
`CD` into your disk/directory where `mbr.bin` lives and run `boot mbr.bin`. After this it is not possible to exit back to the prompt; you will have to close DOSBox-X entirely to get out.


## Project layout
Aside from some binary resoures in `res/` (more on that below), the game itself consists of just `.asm` files.

#### mbr.asm
The entrypoint of the whole bootloader. It handles the bootloader setup, the padding at the end and the partition table. It loads the rest of the game from disk into memory and handles any loading errors with error messages.

#### game.asm
This file includes everything else and contains the main game loop, including the mode 13h switch, drawing logic and game logic.

#### constants.asm
A bunch of constants to make the code more readable. The sprite size can be tweaked here to use larger or smaller images. Note that non-player sprites are square and a player sprite can be higher than a non-player sprite, but not wider.

#### sprites.asm
This file includes the binary sprites and contains some macro functions to easily access the memory locations where they are loaded.

#### levels.asm
Loads the binary levels and contains some macro functions to easily access them. Also contains functions to load a level into the "active level" block, to keep track of the current state of the active level.

#### animation.asm
Functions to start and stop a character animation, some macros and structs to keep track of the animation state and a function that is triggered on a timer to move to the next frame.

#### audio.asm
Includes the raw binary audio and a function to start playing audio, as well as macros and structs to keep track of the currently playing audio and a timed function to move onto the next byte in the audio stream.
This code is adapted from the code found [here](https://bumbershootsoft.wordpress.com/2016/12/10/beyond-beep-boop-mastering-the-pc-speaker/). That article also contains an explanation of how it works using 2 timers.

#### timer.asm
Since we need 2 timers for the audio, we do not really have an animation timer available.
This file sets up the audio timer and the interrupt function that is called on each trigger. It then calls the respective audio timer and the animation timer every X cycles.
This does mean that the animation timer can only ever trigger at a subdivided interval of the audio timer, but since the audio timer triggers very often, this is not really a problem.


## Resources
This game uses a bunch of free resources from around the web.

`res/tiles_and_char_8bit.png` contains all the sprites for the game. The first 5 (level tiles and chests) come from `hyptosis_tile-art-batch-5.png` found [here](https://opengameart.org/content/lots-of-free-2d-tiles-and-sprites-by-hyptosis).
The character comes from [this tileset](https://opengameart.org/content/zelda-like-tilesets-and-sprites), in the `character.png` file.

`res/Ejimas1.ogg` is the original audio file of the walk sound, which comes from [here](https://opengameart.org/content/step-sound-walking).
`res/Win sound.wav` is the original victory sound, which comes from [here](https://opengameart.org/content/win-sound-effect).

`res/original.txt` is a set of the original 50 Sokoban levels (with one added at the start for easy testing). This file is in [Sokoban level format](http://www.sokobano.de/wiki/index.php?title=Level_format).
These levels, and many more, can be found [here](https://www.sourcecode.se/sokoban/levels). This file contains the first 50 levels from the "Original & Extra" set by "Thinking Rabbit".

### Resource pre-processing
#### Sprites
None of the resources are included into the final binary "as-is".

The sprites are combined from the 2 sources mentioned above into a single file containing both the level sprites and the character.
1. Using [GIMP](https://www.gimp.org/) I pasted the sprites from the sheets into one file.
2. Enlarge the chests a bit so they cover the entire 16x16 tile (the game code only handles transparency for the character).
3. Remove the semi-transparency on the character (the game code does not handle semi-transparency, only full) by using Layer > Transparency > Threshold Alpha, value 0.02, opacity 100. This leaves the transparency around the character while changing semi-transparent pixels into fully opaque ones.
4. Export the image to an 8bpc RGBA PNG.
5. Run the Python script `res/converters/sprites.py` to read the combined file and output it into a game-specific binary format with a palette. The script itself has comments describing what is happening and why.

#### Sound files
For the audio to work properly, we want it mono and as small as possible. The quality will suffer (greatly), but it will work! I processed the audio using [Audacity](https://www.audacityteam.org/).

1. Cut off the bits of the file at the start and end that have no (or barely any) actual audio, making the whole track a lot smaller.
2. Select Tracks > Mix > Mix Stereo down to Mono (if there is a stereo track).
3. Set the Project Rate (Hz) (bottom) to 8000.
4. Select File > Export > Export Audio.
5. Set Save as type to "Other uncompressed file".
6. Set Header to "RAW (header-less)" and Encoding to "Signed 8-bit PCM".
7. Save as either `walk.raw` or `victory.raw`.

#### Levels
The levels are pre-processed to make them easier to load in ASM. The default Sokoban format is readable text that can be parsed, but it has some issues that make it tricky to process:

1. The levels are spread across multiple lines using `\n` or `\r\n` as a line delimiter, or even `\r` can be valid, making it hard to predictably parse.
2. Level lines are not the same length, meaning I would have to parse all lines just to find the width of the level for drawing purposes.
3. There is no difference between empty space outside of the level wall, or inside of the level wall - both are just a blank space. Figuring out which squares are inside the playing field and which are not is doable, but tricky in ASM.

During the pre-processing I make the levels "rectangular" and solve the issue with inside and outside spaces. Some metadata is added as a header and all levels that are too large to actually draw in mode 13h are skipped.

The Python script `res/converters/levels.py` will turn `res/original.txt` into `res/levels.bin` for inclusion in the source.
Note that if you change the sprite size in `constants.asm`, you also need to change it here.
