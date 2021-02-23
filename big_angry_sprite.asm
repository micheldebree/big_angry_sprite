; vim:set ft=c64jasm:fdm=marker:

; Wishes/issues:
; - !align not documented
; - else statement needs preceding } on the same line
; - segment names and var names clash
; - cannot forward reference "!let sineIndex = * + 1"

!use "spd" as spd
!use "sid" as sid
!use "sines" as sines
!use "bytes" as bytes
!include "macros.asm"

!let music = sid("terminator-vfx.sid")
!let getMusicEvents = music.play - 3
!let spritepadFile = spd("arniev2.spd")

!segment code(start=$0801, end=$17ff)
!segment charset(start=$1800, end=$1fff)
!segment sprites(start=$2000, end=$3fff)
!segment musicsegment(start=$4000, end=$9fff)

!segment charset 
characterData:

!segment musicsegment
!byte music.data

!let cols = 8 ; nr of sprites horizontally
!let rows = 11 ; nr of sprites vertically
!let lowerCols = 14 ; nr of sprites in the lower border
!let lowerSpritesY = 10 ; y position of sprites in the lower border
!let sineLength = 216 ; number of bytes in one period of the sinetables, timed to the music (24*note duration)
!let initialSpriteMovementSpeed = 0 ; speed of sprite movement to start with

!let spriteRasterStart = $33-19 ; the rasterline where the main sprites start
!let borderline = $f8 ; the rasterline where the lower border starts

; we are done, set the next interrupt to occur just before a sprite row
!macro nextInterruptBeforeRow(row, irq) {
  +nextInterrupt(spriteRasterStart - 2 + row * 21, irq)
}

!macro rotateVirtualSpriteTableRight(table) {
        ldx #lowerCols - 1
        lda table + lowerCols - 1
        pha
loop:
        lda table - 1,x
        sta table,x
        dex
        bne loop
        pla
        sta table
}

!segment code

+basic_start(start)

nmi:
        rti

start: {
        sei             ; Turn off interrupts
        jsr music.init
        +copyRomChar(characterData)
        jsr $ff81       ; ROM Kernal function to clear the screen
        lda #%00110101
        sta $01         ; Turn off Kernal ROM

        lda #$7f
        sta $dc0d      ; no timer IRQs
        lda $dc0d      ; acknowledge CIA interrupts

        lda #<nmi
        sta $fffa
        lda #>nmi
        sta $fffb      ; dummy NMI (Non Maskable Interupt) to avoid crashing due to RESTORE

        ldx #$0f
        stx $d020
        stx $d021
        ldx #$0c
        stx $d025
        dex
        stx $d026

        lda #0
        !for i in range(cols) { sta $d027 + i }
        sta $3fff ; clear the black byte that lives under the lower and upper borders

        ; all sprites enabled and multicolor
        lda #$ff
        sta $d015
        sta $d01c

        ;set x-coordinates
        !for i in range(cols) {
           lda #24 + 64 + i * 24
           sta $d000 + i * 2
        }

        lda #%00011000
        sta $d011

        +setInterrupt(borderline, irq_open_border)

        lda #$01
        sta $d01a   ; Enable raster interrupts and turn interrupts back on

        cli
        jmp *       ; Do nothing and let the interrupts do all the work.
}

; rows are numbered starting from zero

; set the y-position of a row of sprites
; this can be done way in advance; once a sprite has started drawing,
; a change in y position is only effective the next time it starts drawing
!macro setYpos(row) {
  nop
  ; y-position is one less than the rasterline where the sprite starts
  ldy #(spriteRasterStart - 1 + row * 21)
  !for col in range(cols) {
    sty $d001 + col * 2
  }
}
; set the sprite pointers for a row of sprites
!macro setPointers(row) {
  !for col in range(cols) {
    !if (row == 0) {
      lda #(spriteData/64 + col * rows);
      sta $07f8 + col;
    } else {
      ; sprite data is layed out so we only need to increase the pointer by
      ; one to get the sprite of the next row
      inc $07f8 + col
    }
  }
}

irq0:
        +setYpos(1)
        +nextInterruptBeforeRow(1,irq1)
irq1:
        +sleep(16)
        +setPointers(1)
        +setYpos(2)
        dec $d011
        +rotateVirtualSpriteTableRight(lowerXLow)

        +nextInterruptBeforeRow(2, irq2)
irq2: ; bad line
        +sleep(17)
        +setPointers(2)
        +setYpos(3)
        inc $d011
        +rotateVirtualSpriteTableRight(lowerXHigh)
        +nextInterruptBeforeRow(3, irq3)
irq3:
        +sleep(16)
        +setPointers(3)
        +setYpos(4)
!let skipSpritePointerRotation = * + 1
        lda #0
        bne skip
        +rotateVirtualSpriteTableRight(lowerpointers)
skip:   lda #0
        sta skipSpritePointerRotation

        +nextInterruptBeforeRow(4, irq4)
irq4:
        +sleep(16)
        +setPointers(4)
        +setYpos(5)
        inc $d011
        +rotateVirtualSpriteTableRight(lowerXHigh2)
        +nextInterruptBeforeRow(5, irq5)
irq5: ; badline
        +sleep(16)
        +setPointers(5)
        +setYpos(6)
        dec $d011
        +nextInterruptBeforeRow(6, irq6)
irq6:
        +sleep(17)
        +setPointers(6)
        +setYpos(7)
        +nextInterruptBeforeRow(7, irq7)
irq7:
        +sleep(17)
        +setPointers(7)
        +setYpos(8)
        +nextInterruptBeforeRow(8, irq8)
irq8:
        +sleep(16)
        +setPointers(8)
        +setYpos(9)
        +nextInterruptBeforeRow(9, irq9)
irq9:
        +sleep(17)
        +setPointers(9)
        +setYpos(10)
        dec $d011
        +nextInterruptBeforeRow(10, irq10)

irq10: ; badline
        +sleep(17)
        +setPointers(10)
        inc $d011

        +nextInterrupt(borderline, irq_open_border)

irq_open_border: {
        lda #%10010000
        sta $d011
        ; set sprite y beforehand
        lda #14
        !for col in range(8) {
          sta $d001 + col * 2
        }
        +nextInterrupt($106, irq_lower_sprites)
}

; the sprites will shift from x pos 24 to 0 before resetting to 24
; at that moment, the pointers will be shifted to the left
; just like a 'normal' character scroller, but with sprites (3 chars) instead of characters
!let shiftWidth = 24

; the amount of pixels the sprites are moved to the right
; counts down from 24 to 0 repeatedly
scrollShift:
        !byte shiftWidth

irq_lower_sprites:
        !for i in range(8) {
          lda lowerpointers + i
          sta $07f8 + i
        }

; set the high bit of the x position for each sprite
; which are packed into the byte at $d010
; because of the 24 pixel shifting, one of the sprites' x pos
; crosses the 255 mark, and two tables are used, one with
; the bit set and one with the bit cleared
        lda scrollShift
        cmp #16
        bcs useSecondTable
        lda lowerXHigh
        jmp skipSecondTable
useSecondTable:
        lda lowerXHigh2
skipSecondTable:
        sta $d010

        !for i in range(8) {
          lda lowerXLow + i
          clc
          adc scrollShift
          sta $d000 + i * 2
        }

; set the sprite colors
        +multicolorSprites(0)
        ldx #$0c
        stx $d027
        stx $d027+7
        dex
        stx $d027+1
        stx $d027+6
        ldx #0
        stx $d027+2
        stx $d027+3
        stx $d027+4
        stx $d027+5
        jsr scroller::moveScroller
        +nextInterrupt($24, irq_move_sprites)

scroller: {

  moveScroller: {
            dec scrollShift
            bpl done
            lda #shiftWidth
            sta scrollShift
            inc skipSpritePointerRotation
            rts
    done:   lda scrollShift
            and #%00000111
            beq advanceScrollText
    skip:  rts
  }

  drawNextChar: {

    !let copyToAddress = storeInSprite + 1
    !let copyFromAddress = loadCharacterData + 1

    ; x = character row to draw at
            lda spriteAddressLow,x
            sta copyToAddress
            lda spriteAddressHigh,x
            sta copyToAddress + 1
    loadNextChar:
            lda scrollText
            bne notTheEnd ; 0 marks the end of the text
            ldx #<scrollText
            stx loadNextChar + 1
            ldx #>scrollText
            stx loadNextChar + 2
            jmp loadNextChar
    notTheEnd:
            tax
            ; start of character data
            lda characterAddressLow,x
            sta copyFromAddress
            lda characterAddressHigh,x
            sta copyFromAddress + 1
            ldx #7
            ldy #8 * 3

    loadCharacterData:
            lda characterData,x

    storeInSprite:
            sta lowerspritedata,y
            dey
            dey
            dey
            dex
            bpl loadCharacterData
            rts
  }

  advanceScrollText: {
    !let newCharLocation = * + 1
            ldx #41 ; there are 42 characters
            jsr drawNextChar
            inc drawNextChar::loadNextChar + 1
            bne skipHibyte
            inc drawNextChar::loadNextChar + 2
    skipHibyte:
            inc newCharLocation
            lda newCharLocation
            cmp #42
            bne skipReset
            lda #0
            sta newCharLocation
    skipReset:
            rts
  }

  scrollText:
    !byte bytes.toScreencode("scroll.txt")
    !byte 0
}

; for each character position, the address where the data should go in the sprite
spriteAddressLow:
  !for sprite in range(lowerCols) {
    !for colInSprite in range(3) { !byte (lowerspritedata + sprite * 64 + colInSprite) }
  }

spriteAddressHigh:
  !for sprite in range(lowerCols) {
    !for colInSprite in range(3) { !byte (lowerspritedata + sprite * 64 + colInSprite) >> 8 }
  }

; for each character, the address where the character data starts, for copying into a sprite
; TODO: limit to only the chars that are used, to save space
!let nrChars = 128
characterAddressLow:
  !for char in range(nrChars) { !byte bytes.lo(characterData + char * 8) }

characterAddressHigh:
  !for char in range(nrChars) { !byte bytes.hi(characterData + char * 8) }

irq_move_sprites: {

  !macro setXPos(col) {
    lda sineTablesLo+2*Math.floor(col/4),x
    sta $fa
    lda sineTablesLo+1+2*Math.floor(col/4),x
    sta $fb
    lda sineTablesHi+2*Math.floor(col/4),x
    sta $fc
    lda sineTablesHi+1+2*Math.floor(col/4),x
    sta $fd
    clc
    lda ($fa),y
    !if (col > 0) { adc #(col * 24) }
    sta $d000 + col * 2;
    lda ($fc),y
    adc #0
    beq done

  ; if the addition overflowed (carry bit is set),
  ; set the corresponding high bit for the sprite x position
    lda $d010
    ora #(1 << col)
    sta $d010
  done:
  }

  ; do subsequents interrupts on line < $ff
  ; and switch back to 25 rows
  lda #%00011000
  sta $d011

  lda #0
  sta $d010
  +setPointers(0)
  +setYpos(0)

  ; TODO: allow forward references that are relative to *?
  !let sineindex = getSineSample + 1
  !let spriteMovementSpeed = advanceSineIndex + 1
  !let sineTablePointer = nowrap + 1

  getSineSample:

          ldy #(sineLength/4 + sineLength/2)
          bne nowrap
          ; when a sine has completed, set the
          ; pointer to the next table.
          ; by changing this pointer, the movement will change

  !let nextSineTablePointer = * + 1

          lda #0
          sta sineTablePointer

  !let nextSpriteMovementSpeed = * + 1

          lda #initialSpriteMovementSpeed
          sta spriteMovementSpeed

  nowrap:
          ldx #8 ; sine table pointer
          +setXPos(0)
          +setXPos(1)
          +setXPos(2)
          +setXPos(3)
          +setXPos(4)
          +setXPos(5)
          +setXPos(6)
          +setXPos(7)
          +multicolorSprites(1)

          lda sineindex
          clc

  advanceSineIndex:
          adc #initialSpriteMovementSpeed
          sta sineindex
          cmp #sineLength
          bcc nowrap2
          lda #0
          sta sineindex
  nowrap2:
          jsr colorFade
          jsr music.play
          jsr handleMusicEvent

          +nextInterrupt(spriteRasterStart + 10, irq0)
}

colorFade: {
!let colorIndex = * + 1
        lda #0
        and #%00000111
        beq stop
        dec colorIndex
stop:
        tax
        lda spriteColor1,x
        sta $d025
        lda spriteColor2,x
        sta $d026
        lda spriteColor3,x
        !for i in range(8) { sta $d027 + i }
        rts

spriteColor1:
        !byte $0c,$05,$03,$0f,$0f,$0f,$0f,$0f
spriteColor2:
        !byte $0b,$08,$0c,$05,$03,$0f,$0f,$0f
spriteColor3:
        !byte $00,$06,$0b,$08,$0c,$0f,$0f,$0f
}

handleMusicEvent: {

        ; commands, used in the music with command 07 ?? xx
        !let commandFade = 1
        !let commandSetSineTable0 = 2
        !let commandSetSineTable1 = 3
        !let commandSetSineTable2 = 4
        !let commandSetSpriteSpeed2 = 5
        !let commandSetNextSpriteSpeed0 = 7

        ; get the latest events from laxity's specialized player
        jsr getMusicEvents
        ; a = last byte of instrument table
        ;     in case of a note-on event
        ;     or-ed together from the 3 voices
        ; x = xx on player command 07 ?? xx

        cpx #commandFade
        bne skip1
        ldy #(colorFade::spriteColor2 - colorFade::spriteColor1 - 1)
        sty colorFade::colorIndex
skip1:  cpx #commandSetSineTable0
        bne skip2
        ldy #0
        sty irq_move_sprites::nextSineTablePointer
skip2:  cpx #commandSetSineTable1
        bne skip3
        ldy #4
        sty irq_move_sprites::nextSineTablePointer
skip3:  cpx #commandSetSineTable2
        bne skip4
        ldy #8
        sty irq_move_sprites::nextSineTablePointer
skip4:  cpx #commandSetSpriteSpeed2
        bne skip5
        ldy #2
        sty irq_move_sprites::spriteMovementSpeed 
        sty irq_move_sprites::nextSpriteMovementSpeed
skip5:  cpx #commandSetNextSpriteSpeed0
        bne skip6
        ldy #0
        sty irq_move_sprites::nextSpriteMovementSpeed
skip6:  rts

}

!segment sprites

spriteData:

!for col in range(cols) {
  !for row in range(rows) { !byte spritepadFile.data[row * 8 + col] }
}

lowerspritedata:
  !fill lowerCols * 64, 0

lowerpointers:
  !for sprite in range(lowerCols) { !byte lowerspritedata/64 + sprite }

lowerXLow:
  !for sprite in range(lowerCols) { !byte 24 * sprite }

; the high bits of the x positions
; because the sprites are shifting 0-23 pixels,
; there are two tables
lowerXHigh:
  !for sprite in range(lowerCols) { !byte %11100000000000 >> sprite }

lowerXHigh2:
  !for sprite in range(lowerCols) { !byte %11110000000000 >> sprite }

!segment musicsegment

!align $100

!let amplitude = 180
!let middle = 88
!let together = 0

; one sine per 4 sprites, low byte
sineTablesLo:
; 0: take turns
      !byte bytes.lohi(sine1)
      !byte bytes.lohi(sine1)

; 4: big sweep
      !byte bytes.lohi(sine3)
      !byte bytes.lohi(sine3)
; 8: headbutt
      !byte bytes.lohi(sine4)
      !byte bytes.lohi(sine5)
; one sine per 4 sprites, high byte
sineTablesHi:
      !byte bytes.lohi(sine1hi)
      !byte bytes.lohi(sine2hi)

      !byte bytes.lohi(sine3hi)
      !byte bytes.lohi(sine3hi)

      !byte bytes.lohi(sine4hi)
      !byte bytes.lohi(sine5hi)

sine1:
  !byte bytes.loBytes(sines.sine01(middle, amplitude, sineLength))
sine1hi:
  !byte bytes.hiBytes(sines.sine01(middle, amplitude, sineLength))

sine2:
  !byte bytes.loBytes(sines.sine02(middle, amplitude, sineLength))
sine2hi:
  !byte bytes.hiBytes(sines.sine02(middle, amplitude, sineLength))

sine3:
  !byte bytes.loBytes(sines.sine03(middle, 255, sineLength))
sine3hi:
  !byte bytes.hiBytes(sines.sine03(middle, 255, sineLength))

sine4:
  !byte bytes.loBytes(sines.sine04(middle, amplitude, sineLength))
sine4hi:
  !byte bytes.hiBytes(sines.sine04(middle, amplitude, sineLength))

sine5:
  !byte bytes.loBytes(sines.sine05(middle, amplitude, sineLength))
sine5hi:
  !byte bytes.hiBytes(sines.sine05(middle, amplitude, sineLength))

