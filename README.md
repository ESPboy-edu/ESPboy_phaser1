
# ESPboy "phaser1" -- 1-bit music player

by Shiru

shiru@mail.ru

https://www.patreon.com/shiru8bit

## To edit phaser1 music use:

- [beepola tracker](http://freestuff.grok.co.uk/beepola/)
- [phaser1 ZX Spectrum tracker](https://worldofspectrum.org/software?id=0024603).  Try ZX Spectrum emulator [FUSE or UNREALSPECCY or any other](https://worldofspectrum.org/tools/emulators)

## Check demo video

- [one](https://www.youtube.com/watch?v=BGOUavIc9Fk)
- [two](https://www.youtube.com/watch?v=pFxaQLD2BQ8)




# READ.ME

from original ZX Spectrum Phaser1 1-bit music engine

http://shiru.untergrund.net/files/zx/phaser1.zip


Phaser1 by Shiru (shiru@mail.ru)


About

Phaser1 is a 1-bit music editor with tracker interface for ZX Spectrum 48K. Unlike my previous editors, this one is not based on any existing beeper music engine. I've made my own, completely original engine instead, with inspiration from Topo Soft music (the same type of the synthesis) and Chase H.Q. drums. Also, unlike the previous editors, this one is designed not to simply make music per se, but to use it in programs, either from BASIC or assembler.


Features

Separate speed column. Speeds has step of 1/23.4 second, so choice of possible speeds is rather limited.

One channel of drums, either synthesized (two types, 9 pitches), or digital (8 samples). Drums aren't customizable. Digital drums adds ~1K of data to the player.

Two channels of tone. First channel is phasing synth, up to 99 instruments could be used. Second is pure square tone. Both channels has range of 5 octaves.

Single-pattern structure, 2048 rows. Use block copying instead of order-list.


Row format:

     +--Speed 1..9
     |+-Drum 1..9
     ||
0000 12 C-401 C-2
|  |    | ||| | |
+--+    +++++ +++
|        |  | +-Ch2 note
|        |  +---Ch1 instrument
|        +------Ch1 note
+-Row number


Phasing synth

Phasing synth consist two square tone generators, both change output state, producing more complex waveform. The synth controlled by three parameters, all the parameters are for the second generator only. Multiple defines frequency of the second generator, relative to the first (0=the same, 1=twice of the first, etc). Detune introduces difference between the generators, producing phasing effect. Phase allows to set phase on start of the generator, which only occurs if phase reset used (at start of the song, or using CS+2 on note in the first channel). Don't forget to reset the phase using CS+2 with every change of the instrument, otherwise you'll get combination of settings of the previous and new instruments, which will produce unexpected sound.

Although the engine does not support the volume, you can imitate PWM volumes on the first tone channel, using Multiple=0 Detune=0 Phase<128. Lower value for Phase makes the volume lower, and also affects to the timbre.


Controls

All the control keys are listed in built-in help (Space, I).


Using the music in the programs

The editor has Compile option. However, it only saves music data, without player. The player could be customized and often need to be relocated, so instead of including tons of related code into editor, I've decided to release the player as source code. You can easily compile the player, even if you know nothing about assemblers.

You'll need SjAsmPlus (http://sjasmplus.sourceforge.net/) to compile the code. Before compilation, edit lines 10-14 of player.asm to choose your settings:

LOCATION is the address in the RAM where the player should be located. You could provide it in hexadecimal (like #c000) or decimal (49152) form. Do not locate the player under #8000 (32768) or in slow RAM page (for 128K).

MUSICDATA is the address in the RAM where the compiled music data should be located. You can locate in under #8000, however it is not recommended.

DRUMSET is the type of drums. The player could be compiled either with synthesized drums (smaller) or with digital (larger). Your music should be made using the same set of the drums.

WAITKEY is the condition when the player should stop playing. 0 for waiting any key, 1 for waiting release of any key.

LOOPMODE is the condition what happens when the song is over. 0 for play it from loop point (provided in the editor using L key), 1 for stop and exit from player.

After you've edit the file, compile it with this command (in command line or .bat file):

sjasmplus player.asm

You'll get player.bin, which is the compiled player. To play the music, simply use RANDOMIZE USR (address you provided as LOCATION), or CALL (address). You only have to compile the player if you need to relocate it or use another drum set. All the settings, except the DRUMSET, could be changed using POKEs:

MUSICDATA - LOCATION+1 (two bytes)
WAITKEY   - LOCATION+4 (one byte)
LOOPMODE  - LOCATION+6 (one byte)

This way you can use the same player code with few songs.


History

v1.0 24.03.10 - Initial version