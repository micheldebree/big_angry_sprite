!macro basic_start(addr) {
* = $801
    !byte $0c
    !byte $08
    !byte $00
    !byte $00
    !byte $9e

!if (addr >= 10000) {
    !byte $30 + (addr/10000)%10
}
!if (addr >= 1000) {
    !byte $30 + (addr/1000)%10
}
!if (addr >= 100) {
    !byte $30 + (addr/100)%10
}
!if (addr >= 10) {
    !byte $30 + (addr/10)%10
}
    !byte $30 + addr % 10
    !byte 0, 0, 0
}

; copy the character data that is hidden in the ROM underneath $d000 to a location in RAM,
; so we can use it and also use the VIC and SID registers
!macro copyRomChar(toAddress) {

        lda $01
        pha
        ; make rom characters visible
        lda #%00110011
        sta $01
        ldx #0
loop:
        !for i in range(8) {
          lda $d000 + (i * $100),x
          sta toAddress + (i * $100),x
        }
        inx
        bne loop

        pla
        sta $01
}

!macro sleep(cycles) {
  !for i in range(cycles) {
    nop
  }
}

!macro multicolorSprites(on) {
    !if(on) {
      lda #$ff
    } else {
      lda #$0
    }
    sta $d01c
}

!macro setInterrupt(rasterline, address) {
    lda #<address
    sta $fffe
    lda #>address
    sta $ffff
    lda #(rasterline & $ff)
    sta $d012
}

!macro nextInterrupt(rasterline, irq) {
  +setInterrupt(rasterline, irq)
  asl $d019
  rti
}
