// vim:set ft=kickass:fdm=marker:

// {{{ Global constants

// https://www.c64-wiki.com/wiki/Sprite
// https://emudev.de/q00-c64/sprites-more-cpu-timings-and-irq-quirks/
// 3.14.1. Hyperscreen

// $d011 |RST8| ECM| BMM| DEN|RSEL|    YSCROLL   | Control register 1
//RSEL|  Display window height   | First line  | Last line
//  ----+--------------------------+-------------+----------
//    0 | 24 text lines/192 pixels |   55 ($37)  | 246 ($f6)
//    1 | 25 text lines/200 pixels |   51 ($33)  | 250 ($fa)

//  CSEL|   Display window width   | First X coo. | Last X coo.
//  ----+--------------------------+--------------+------------
//    0 | 38 characters/304 pixels |   31 ($1f)   |  334 ($14e)
//    1 | 40 characters/320 pixels |   24 ($18)   |  343 ($157)

// TODO:
// - avoid having to 'skip over' badlines
//
// some magic numbers that are hardcoded because i want them
// etched into my memory:
// 21: height of a sprite in lines (pixels)
// 24: - width of a sprite in pixels
//     - x pos when sprite touches left border
//  3: width of a sprite in bytes (characters) (24/8)
//  8: number of sprites available
// 63: number of bytes of data for one sprite (21*24/8)
// 64: size of block of sprite data, with the last byte not being used

.const debug = false
.const cols = 8 // nr of sprites horizontally
.const rows = 11 // nr of sprites vertically
.const lowerCols = 14 // nr of sprites in the lower border
.const lowerSpritesY = 10 // y position of sprites in the lower border
.const sineLength = 216 // number of bytes in one period of the sinetables, timed to the music (24*note duration)
.const initialSpriteMovementSpeed = 0 // speed of sprite movement to start with

// visible part of screen is at rasterlines $33-$f8
.const spriteRasterStart = $33-19 // the rasterline where the main sprites start
.const borderline = $f8 // the rasterline where the lower border starts

// }}}

// {{{ External resources

.var music = LoadSid("terminator-vfx.sid")
.var spritepadFile = LoadBinary("arnie3.spd", "BackgroundColor=0,MultiColor1=1,MultiColor2=2,Sprites=3")

// }}}

// {{{ Global macros

// TODO: make more efficient
.macro sleep(cycles) {
  .for (var i = 0; i < cycles; i++) {
    nop
  }
}

// copy the character data that is hidden in the ROM underneath $d000 to a location in RAM,
// so we can use it and also use the VIC and SID registers
.macro copyRomChar(toAddress) {

        lda $01
        pha
        // make rom characters visible
        lda #%00110011
        sta $01
        ldx #0
!loop:
        .for(var i = 0; i < 8; i++) {
          lda $d000 + (i * $100),x
          sta toAddress + (i * $100),x
        }
        inx
        bne !loop-

        pla
        sta $01
}

// we are done, it's the next interrupt's turn
.macro nextInterrupt(rasterline, irq) {
  setInterrupt(rasterline, irq)
  asl $d019
  rti
}

// we are done, set the next interrupt to occur just before a sprite row
.macro nextInterruptBeforeRow(row, irq) {
  nextInterrupt(spriteRasterStart - 2 + row * 21, irq)
}

.macro setInterrupt(rasterline, address) {
    lda #<address
    sta $fffe
    lda #>address
    sta $ffff
    lda #(rasterline & $ff)
    sta $d012
    //.if (rasterline > $ff) {
      // lda $d011
      // ora %10000000
      // sta $d011
//    }
}

.macro multicolorSprites(on) {
    lda #(on ? $ff : 0)
    sta $d01c
}

// }}}

// {{{ Init, setup first IRQ
* = $0801

BasicUpstart2(start)

* = * "Main code"

nmi:
        rti
start:
        sei             // Turn off interrupts
        jsr music.init
        copyRomChar(characterData)
        // jsr scroller
        jsr $ff81       // ROM Kernal function to clear the screen
        lda #%00110101
        sta $01         // Turn off Kernal ROM

        lda #$7f
        sta $dc0d      // no timer IRQs
        lda $dc0d      // acknowledge CIA interrupts

        lda #<nmi
        sta $fffa
        lda #>nmi
        sta $fffb      // dummy NMI (Non Maskable Interupt) to avoid crashing due to RESTORE

        ldx #$0f
        stx $d020
        stx $d021
        ldx #$0c
        stx $d025
        dex
        stx $d026

        lda #$00
        .for (var col=0; col < cols; col++) {
          sta $d027 + col;
        }
        // clear the black byte that lives under the lower and upper borders
        sta $03fff

        // all sprites enabled and multicolor
        lda #$ff
        sta $d015
        sta $d01c

        // set x-coordinates
        .for (var i=0; i < cols; i++) {
           lda #24 + 64 + i * 24
           sta $d000 + i * 2
        }

        lda #%00011000
        sta $d011

        setInterrupt(borderline, irq_open_border)

        lda #$01
        sta $d01a   // Enable raster interrupts and turn interrupts back on
        cli

        jmp *       // Do nothing and let the interrupts do all the work.


// }}}

// {{{ IRQs: Rows of sprites, each row has an irq to display it

// rows are numbered starting from zero

// set the y-position of a row of sprites
// this can be done way in advance; once a sprite has started drawing,
// a change in y position is only effective the next time it starts drawing
.macro setYpos(row) {
  nop
  // y-position is one less than the rasterline where the sprite starts
  ldy #(spriteRasterStart - 1 + row * 21)
  .for (var col = 0; col < cols; col++) {
    sty $d001 + col * 2
  }
}
// set the sprite pointers for a row of sprites
.macro setPointers(row) {
  .for (var col = 0; col < cols; col++) {
    .if (row == 0) {
      lda #(spriteData/64 + col * rows);
      sta $07f8 + col;
    }
    else {
      // sprite data is layed out so we only need to increase the pointer by
      // one to get the sprite of the next row
      inc $07f8 + col
    }
  }
}

irq0:
        setYpos(1)
        nextInterruptBeforeRow(1,irq1)
irq1:
        // timing so changing pointers starts at the right position of
        // the rasterbeam
        sleep(16)
        // setting the pointers for a row of sprites
        // starts one rasterline before last line of the current row,
        // at the start of the right sideborder
        // because the VIC reads the spritepointer while in the sideborder,
        // it will just miss reading the new pointer values by a hair
        // so the last line will still be drawn with the data from the
        // original pointers, but the new pointers are already in place for
        // the first line of the next row
        setPointers(1)
        // the y-position for the next row can be
        // set as soon as this row starts drawing
        // because changing y-position does not
        // have an effect before the sprite has finished
        // drawing
        setYpos(2)
        // shift $d011 y-pos so a bad line does not occur during the next interrupt
        // TODO: handle bad lines by more careful timing? So we don't screw up the underlying
        // screen by messing with $d011
        dec $d011
        // some time left to do other stuff
        rotateVirtualSpriteTableRight(lowerXLow)

        nextInterruptBeforeRow(2, irq2)
irq2: // bad line
        sleep(17)
        setPointers(2)
        setYpos(3)
        // shift $d011 y-pos back where it was
        inc $d011
        // rotate the x pos for the sprites in the lower scroll
        // because we have some time before the next interrupt occurs
        rotateVirtualSpriteTableRight(lowerXHigh)
        nextInterruptBeforeRow(3, irq3)
irq3:
        sleep(16)
        setPointers(3)
        setYpos(4)
// by setting this to a value other than zero,
// the rotation of the pointers is skipped for one frame
// this has the effect of rotating the pointers one place to
// the left from the 'view' of the scroller logic
.label skipSpritePointerRotation = * + 1
        lda #0
        bne !skip+
        // rotate the sprite pointers for the lower scroll
        rotateVirtualSpriteTableRight(lowerpointers)
!skip:  lda #0
        sta skipSpritePointerRotation

        nextInterruptBeforeRow(4, irq4)
irq4:
        sleep(16)
        setPointers(4)
        setYpos(5)
        inc $d011
        // rotate the high bits of the x pos for the sprites in the lower scroll
        rotateVirtualSpriteTableRight(lowerXHigh2)
        nextInterruptBeforeRow(5, irq5)
irq5: // badline
        sleep(16)
        setPointers(5)
        setYpos(6)
        dec $d011
        nextInterruptBeforeRow(6, irq6)
irq6:
        sleep(17)
        setPointers(6)
        setYpos(7)
        nextInterruptBeforeRow(7, irq7)
irq7:
        sleep(17)
        setPointers(7)
        setYpos(8)
        nextInterruptBeforeRow(8, irq8)
irq8:
        sleep(16)
        setPointers(8)
        setYpos(9)
        nextInterruptBeforeRow(9, irq9)
irq9:
        sleep(17)
        setPointers(9)
        setYpos(10)
        dec $d011
        nextInterruptBeforeRow(10, irq10)

irq10: // badline
        sleep(17)
        setPointers(10)
        inc $d011

        nextInterrupt(borderline, irq_open_border)

// }}}

// {{{ IRQ: Open up the lower and upper border and set y pos of lower border sprites

irq_open_border:

        // turn border off by switching to 24 rows
        // also do subsequent interrupts on line > $ff
        lda #%10010000
        sta $d011
        // set sprite y beforehand
        lda #14
        .for (var col = 0; col < 8; col++) {
          sta $d001 + col * 2
        }

        nextInterrupt($106, irq_lower_sprites)

// }}}

// {{{ IRQ: Draw and animate sprites in lower border


// the sprites will shift from x pos 24 to 0 before resetting to 24
// at that moment, the pointers will be shifted to the left
// just like a 'normal' character scroller, but with sprites (3 chars) instead of characters
.const shiftWidth = 24

// the amount of pixels the sprites are moved to the right
// counts down from 24 to 0 repeatedly
scrollShift:
        .byte shiftWidth

irq_lower_sprites:

        .for (var i = 0; i < 8; i++) {
          lda lowerpointers + i
          sta $07f8 + i
        }

// set the high bit of the x position for each sprite
// which are packed into the byte at $d010
// because of the 24 pixel shifting, one of the sprites' x pos
// crosses the 255 mark, and two tables are used, one with
// the bit set and one with the bit cleared
        lda scrollShift
        cmp #16
        bcs !useSecondTable+
        lda lowerXHigh
        jmp !skipSecondTable+
!useSecondTable:
        lda lowerXHigh2
!skipSecondTable:
        sta $d010

        .for (var i = 0; i < 8; i++) {
          lda lowerXLow + i
          clc
          adc scrollShift
          sta $d000 + i * 2
        }

// set the sprite colors
        ldx #$0c
        stx $d027
        stx $d027+7
        dex
        stx $d027+1
        stx $d027+6
        ldx #00
        stx $d027+2
        stx $d027+3
        stx $d027+4
        stx $d027+5
        stx $d01c
        jsr moveScroller
        nextInterrupt($24, irq_move_sprites)

// move the scroller to the left
// every 24 pixels, reset, move sprite pointers to the left
// every 8 pixels, draw a new character on the far right, out of sight
moveScroller:
        dec scrollShift
        bpl !done+
        // reset the 24 pixel shift
        lda #shiftWidth
        sta scrollShift

        // like in a 'normal' scroller, because we just reset the scoller 24 pixels to the right,
        // we need to rotate the sprite pointers to the left
        // but because we are also rotating to the right elsewhere because
        // of the sprite 'multiplexing' effect, skipping one of those
        // rotations will do the job
        inc skipSpritePointerRotation
        rts

!done:
        // the next character is drawn every 8 pixels
        lda scrollShift
        and #%00000111
        beq advanceScrollText
!skip:  rts

// -----------------------------------------------------

advanceScrollText:
.label newCharLocation = * + 1
        ldx #41 // there are 42 characters
        jsr drawNextChar
        inc scrollTextIndex
        bne !skip+
        inc scrollTextIndex + 1
  !skip:
        inc newCharLocation
        lda newCharLocation
        cmp #42
        bne !skip+
        lda #0
        sta newCharLocation
!skip:
        rts

// There are 14 "virtual" sprite x positions and sprite pointers
// That are rotated to the left to make the illusion they are all displayed
// when in fact there are only 8 sprites visible at a time

// three tables need to be rotated:
// lowerXLow, lowerXHigh and lowerpointers
// this can be done anywhere where there is rastertime left, the following macro is used for that

.macro rotateVirtualSpriteTableRight(table) {

        ldx #lowerCols - 1
        lda table + lowerCols - 1
        pha
!loop:
        lda table -1,x
        sta table,x
        dex
        bne !loop-
        pla
        sta table
}

// }}}

// {{{ Subroutine: Draw text in lower border sprites

drawNextChar:
// x = character row to draw at
        lda spriteAddressLow,x
        sta copyTo
        lda spriteAddressHigh,x
        sta copyTo+1
loadNextChar:
.label scrollTextIndex = * + 1
        lda scrollText
        bne !notTheEnd+ // 0 marks the end of the text
        ldx #<scrollText
        stx scrollTextIndex
        ldx #>scrollText
        stx scrollTextIndex + 1
        jmp loadNextChar
!notTheEnd:
        tax
        // start of character data
        lda characterAddressLow,x
        sta copyFrom
        lda characterAddressHigh,x
        sta copyFrom + 1
        ldx #7
        ldy #8 * 3
!onechar:
.label copyFrom = * + 1
        lda characterData,x
.label copyTo = * + 1
        sta lowerspritedata,y
        dey
        dey
        dey
        dex
        bpl !onechar-
        rts

scrollText:
.text "                                                   "
.text "come with me if you want to live                                        "
.text @"this big angry sprite was coded for raistlin's \"only sprites compo\", but didn't quite make the deadline. "
.text "i am very glad anyway because it's been 30 years since i coded anything on the c64, and i "
.text "almost forgot how much fun and how addictive it is to be coding so close to the metal! "
.text @"this is also a reference to one of the last things i coded on c64: the \"split personality\" part in "
.text @"the demo \"before you go\", made in 1990. "
.text "greetings go to genius, laxity, jch, reyn, sander, moren, mace, stratford, burglar, scoosie and all my fellow heatwavers. "
.text "special thanks to laxity and jch for the excellent sidfactory 2, and for (p)reviewing my tunes. "
.text "if you want to swap, send a floppy with the latest warez to the address on the disk cover (no lamerz!). "
.text "and don't forget to hairspray the stamp so i can send it back to you! "
.text "                                 hugs and kisses - youth      "
.text "                                       "
.byte 0

// for each character position, the address where the data should go in the sprite
spriteAddressLow:
  .for (var sprite = 0; sprite < lowerCols; sprite++) {
    .for (var colInSprite = 0; colInSprite < 3; colInSprite++) {
      .byte <(lowerspritedata + sprite * 64 + colInSprite);
    }
  }
spriteAddressHigh:
  .for (var sprite = 0; sprite < lowerCols; sprite++) {
    .for (var colInSprite = 0; colInSprite < 3; colInSprite++) {
      .byte >(lowerspritedata + sprite * 64 + colInSprite);
    }
  }

// for each character, the address where the character data starts, for copying into a sprite
// TODO: limit to only the chars that are used, to save space
.const nrChars = 128
characterAddressLow:
  .for (var i = 0; i < nrChars; i++) {
     .byte <(characterData + i * 8)
  }
characterAddressHigh:
  .for (var i = 0; i < nrChars; i++) {
     .byte >(characterData + i * 8)
  }

// }}}

// {{{ IRQ: Move the main sprites

.macro setXPos(col) {
  lda sineTablesLo+2*floor(col/4),x
  sta $fa
  lda sineTablesLo+1+2*floor(col/4),x
  sta $fb
  lda sineTablesHi+2*floor(col/4),x
  sta $fc
  lda sineTablesHi+1+2*floor(col/4),x
  sta $fd
  clc
  lda ($fa),y
  .if (col > 0) {
    adc #(col * 24)
  }
  sta $d000 + col * 2;
  lda ($fc),y
  adc #0
  beq !done+

// if the addition overflowed (carry bit is set),
// set the corresponding high bit for the sprite x position
  lda $d010
  ora #(1 << col)
  sta $d010
!done:
}

irq_move_sprites:
        // do subsequents interrupts on line < $ff
        // and switch back to 25 rows
        lda #%00011000
        sta $d011

        lda #0
        sta $d010
        setPointers(0)
        setYpos(0)

.label sineindex = * + 1
        ldy #(sineLength/4 + sineLength/2)
        bne !skip+
        // when a sine has completed, set the
        // pointer to the next table.
        // by changing this pointer, the movement will change
.label nextSineTablePointer = * + 1
        lda #0
        sta sineTablePointer
.label nextSpriteMovementSpeed = * + 1
        lda #initialSpriteMovementSpeed
        sta spriteMovementSpeed

.label sineTablePointer = * + 1
  !skip:
        ldx #8 // sine table pointer
        setXPos(0)
        setXPos(1)
        setXPos(2)
        setXPos(3)
        setXPos(4)
        setXPos(5)
        setXPos(6)
        setXPos(7)
        multicolorSprites(true)

        lda sineindex
        clc
.label spriteMovementSpeed = * + 1
        adc #initialSpriteMovementSpeed
        sta sineindex
        cmp #sineLength
        bcc !skip+
        lda #0
        sta sineindex
!skip:
        jsr colorFade
        jsr music.play
        jsr handleMusicEvent

        nextInterrupt(spriteRasterStart+10, irq0)

// }}}

// {{{ Subroutine: Color fading
colorFade:
.label colorIndex = * + 1
        lda #0
        and #%00000111
        beq !skip+
        dec colorIndex
!skip:
        tax
        lda spriteColor1,x
        sta $d025
        lda spriteColor2,x
        sta $d026
        lda spriteColor3,x
        .for (var i = 0; i < 8; i++) {
          sta $d027 + i
        }
        rts

.align 8
spriteColor1:
        .byte $0c,$05,$03,$0f,$0f,$0f,$0f,$0f
spriteColor2:
        .byte $0b,$08,$0c,$05,$03,$0f,$0f,$0f
spriteColor3:
        .byte $00,$06,$0b,$08,$0c,$0f,$0f,$0f

// }}}

// {{{ Subroutine: Handle events from the music
// commands, used in the music with command 07 ?? xx
.const commandFade = 1
.const commandSetSineTable0 = 2
.const commandSetSineTable1 = 3
.const commandSetSineTable2 = 4
.const commandSetSpriteSpeed2 = 5
.const commandSetNextSpriteSpeed0 = 7

handleMusicEvent:

        // get the latest events from laxity's specialized player
        jsr music.play - 3
        // a = last byte of instrument table
        //     in case of a note-on event
        //     or-ed together from the 3 voices
        // x = xx on player command 07 ?? xx

!skip:  cpx #commandFade
        bne !skip+
        ldy #(spriteColor2 - spriteColor1 - 1)
        sty colorIndex
!skip:  cpx #commandSetSineTable0
        bne !skip+
        ldy #0
        sty nextSineTablePointer
!skip:  cpx #commandSetSineTable1
        bne !skip+
        ldy #4
        sty nextSineTablePointer
!skip:  cpx #commandSetSineTable2
        bne !skip+
        ldy #8
        sty nextSineTablePointer
!skip:  cpx #commandSetSpriteSpeed2
        bne !skip+
        ldy #2
        sty spriteMovementSpeed
        sty nextSpriteMovementSpeed
!skip:  cpx #commandSetNextSpriteSpeed0
        bne !skip+
        ldy #0
        sty nextSpriteMovementSpeed
!skip:  rts

// }}}

// {{{ Data

*=$1800 "Character data"
characterData:
  .fill 8 * $100, 0

*=$2000 "Terminator sprite data"

spriteData:
// layout 0  8 16 24 ..
//        1
//        :
//        10

.if (debug) {
  .for (var col = 0; col < 8; col++) {
    .for (var row = 0; row < rows; row++) {
      .for (var ii =0 ; ii < 21; ii++) {
        .if (mod(row, 2) == 0) {
        .byte %10001000
        .byte %10001000
        .byte %10001000
        }
        else {
        .byte %00100010
        .byte %00100010
        .byte %00100010
        }
      }
      .byte 0
    }
  }
}
else {
  .for (var col = 0; col < cols; col++) {
    .for (var row = 0; row < rows; row++) {
      .fill 64, spritepadFile.getSprites((row * 8 + col) * 64 + i)
    }
  }
}

* = * "Lower scroll sprite data"

lowerspritedata:
  .fill lowerCols * 64, 0

lowerpointers:
  .for (var i = 0; i< lowerCols; i++) {
    .byte lowerspritedata/64 + i
  }

lowerXLow:
  .for (var i = 0; i< lowerCols; i++) {
    .byte i * 24
  }
// the high bits of the x positions
// because the sprites are shifting 0-23 pixels,
// there are two tables
lowerXHigh:
  .for (var i = 0; i< lowerCols; i++) {
    .byte %11100000000000 >> i
  }
lowerXHigh2:
  .for (var i = 0; i< lowerCols; i++) {
    .byte %11110000000000 >> i
  }

* = music.location "Music"

.fill music.size, music.getData(i)
// }}}

* = * "Sine tables"

// {{{ Sine data
.const amplitude = 180
.const middle = 88
.const together = 0

.function sineCalc01(t) {
  .return middle + min(together, amplitude * sin(toRadians(t * 360/sineLength)))
}
.function sineCalc02(t) {
  .return middle + max(-together, amplitude * sin(toRadians(t * 360/sineLength)))
}
.function sineCalc03(t) {
  .return middle + 255 * sin(toRadians(t * 360/sineLength))
}
.function sineCalc04(t) {
  .return middle - abs(amplitude * sin(toRadians(t * 360/sineLength)))
}
.function sineCalc05(t) {
  .return middle + abs(amplitude * sin(toRadians(t * 360/sineLength)))
}

// one sine per 4 sprites, low byte
sineTablesLo:
// 0: take turns
      .byte <sine1,>sine1
      .byte <sine2,>sine2
// 4: big sweep
      .byte <sine3,>sine3
      .byte <sine3,>sine3
// 8: headbutt
      .byte <sine4,>sine4
      .byte <sine5,>sine5
// one sine per 4 sprites, high byte
sineTablesHi:
      .byte <sine1hi,>sine1hi
      .byte <sine2hi,>sine2hi

      .byte <sine3hi,>sine3hi
      .byte <sine3hi,>sine3hi

      .byte <sine4hi,>sine4hi
      .byte <sine5hi,>sine5hi

sine1:
 .fill sineLength, <sineCalc01(i)
sine1hi:
 .fill sineLength, >sineCalc01(i)

sine2:
  .fill sineLength, <sineCalc02(i)
sine2hi:
  .fill sineLength, >sineCalc02(i)

sine3:
  .fill sineLength, <sineCalc03(i)
sine3hi:
  .fill sineLength, >sineCalc03(i)

sine4:
  .fill sineLength, <sineCalc04(i)
sine4hi:
  .fill sineLength, >sineCalc04(i)

sine5:
  .fill sineLength, <sineCalc05(i)
sine5hi:
  .fill sineLength, >sineCalc05(i)

// }}}
