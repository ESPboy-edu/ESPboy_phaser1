;Phaser1 music player by Shiru (shiru@mail.ru), v1.0 22.03.10

	device zxspectrum48

    module player


;settings

LOCATION	equ #c000	;#8000 and above! should be in fast memory
MUSICDATA	equ #8000	;#8000 and above! preferably in fast memory
DRUMSET		equ 1		;0=synth (803 bytes),1=digital (1745 bytes)
WAITKEY		equ 0		;0=wait for key press,1=for release
LOOPMODE	equ 0		;0=play in loop,1=play once

;musicdata is also in location+1 and location+2
;waitkey is also in location+4
;loopmode is also in location+6


	org LOCATION

OP_JR_NZ	equ #20
OP_JR_Z		equ #28

    macro gen2ch
    exx             ;4
    exa             ;4
    add hl,bc       ;10
    out (#fe),a     ;11
    jr c,$+4        ;7/12
    jr $+4          ;7/12
    xor 16          ;7
    add ix,de       ;15
    jr c,$+4        ;7/12
    jr $+4          ;7/12
    xor 16          ;7

    exa             ;4
    out (#fe),a     ;11
    exx             ;4
    add hl,bc       ;10
    jr c,$+4        ;7/12
    jr $+4          ;7/12
    xor 16          ;7
    endm            ;+4+10=146t

;проигрыватель скомпилированных данных

begin

playSong
    ld hl,MUSICDATA
    ld a,WAITKEY
    ld b,LOOPMODE
    or a
    ld c,OP_JR_Z
    jr nz,$+4
    ld c,OP_JR_NZ
    ld a,c
    ld (playerWaitKey),a
    ld a,b
    or a
    ld bc,restart
    jr z,$+5
    ld bc,exitPlayer
    ld (loopMode),bc

player
    di
    push iy

    ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ld (insTable),hl
    ld (curInsOff),hl
    add hl,de
    push hl
 
	ld a,(#5c48)
	rra
	rra
	rra
	and 7
	ld (borderCol),a

    ld h,0
    ld l,a
    ld (cnt1a),hl
    ld (cnt1b),hl
    ld (div1a),hl
    ld (div1b),hl
    ld (cnt2),hl
    ld (div2),hl
borderCol=$+1
    ld a,0
    ld (out1),a
    ld (out2),a
 
    pop hl
    ld (seqPtr),hl

mainLoop
    ld iyl,0
readLoop
seqPtr=$+1
    ld hl,0
    ld a,(hl)
    inc hl
    ld (seqPtr),hl
    or a
loopMode=$+1
    jp z,restart	;end of song, loop
    ;jp z,exitPlayer ;end of song, stop
    bit 7,a
    jp z,render     ;wait
    ld iyh,a
    and 63
    cp 60
    jp nc,other     ;other parameters
    add a,a         ;note
    ld b,0
    ld c,a
    ld hl,noteTable
    add hl,bc
    ld e,(hl)
    inc hl
    ld d,(hl)
    ld a,iyl
    or a
    jr nz,setNote2  ;second channel
setNote1
    ld (div1a),de
    ex de,hl
curInsOff=$+2
    ld ix,0
    ld a,(ix)
    or a
    jr z,$+5
    ld b,a
    add hl,hl
    djnz $-1
    ld e,(ix+1)
    ld d,(ix+2)
    add hl,de
    ld (div1b),hl
    ld iyl,1
    ld a,iyh
    and 64
    jr z,readLoop   ;no phase reset
    ld hl,out1
    res 4,(hl)
    ld hl,0
    ld (cnt1a),hl
    ld h,(ix+3)
    ld (cnt1b),hl
    jr readLoop
setNote2
    ld (div2),de
    ld a,iyh
    ld hl,out2
    res 4,(hl)
    ld hl,0
    ld (cnt2),hl
    jr readLoop

setStop
    ld hl,0
    ld a,iyl
    or a
    jr nz,setStop2
setStop1
    ld (div1a),hl
    ld (div1b),hl
    ld hl,out1
    res 4,(hl)
    ld iyl,1
    jp readLoop
setStop2
    ld (div2),hl
    ld hl,out2
    res 4,(hl)
    jp readLoop

other
    cp 60
    jr z,setStop    ;stop note
    cp 62
    jr z,skipChn1   ;no changes for ch1
    cp 63
    jr z,setLoop    ;loop start
    ld hl,(seqPtr)  ;instrument change
    ld a,(hl)
    inc hl
    ld (seqPtr),hl
    ld h,0
    ld l,a
    add hl,hl
insTable=$+1
    ld bc,0
    add hl,bc
    ld (curInsOff),hl
    jp readLoop

skipChn1
    ld iyl,1
    jp readLoop

setLoop
    ld hl,(seqPtr)
    ld (seqStart),hl
    jp readLoop

restart
seqStart=$+1
    ld hl,0
    ld (seqPtr),hl
    jp readLoop

exitPlayer
	ld hl,10072
	exx
    pop iy
    ei
    ret

render
    and 127
    cp 118
	if DRUMSET==0
    jp nc,drumType0
	else
	jp nc,drumType1
	endif
    ld d,a
    exx

cnt1a=$+1
    ld hl,0
cnt1b=$+2
    ld ix,0
div1a=$+1
    ld bc,0
div1b=$+1
    ld de,0
out1=$+1
    ld a,0
    exx
    exa
cnt2=$+1
    ld hl,0
div2=$+1
    ld bc,0
out2=$+1
    ld a,0

playNote
    ld e,a          ;4
    xor a           ;4
    in a,(#fe)      ;11
    or #e0          ;7
    inc a           ;4
playerWaitKey=$
    jr z,exitPlayer	;7/12 z=hold key, nz=wait key
    ld a,e          ;4
    ld e,0          ;7=48t
.l0
    dup 4
    gen2ch
    nop
    jp $+3
    edup
    org $-4

    dec e           ;4
    jp nz,.l0       ;10

    gen2ch

    dec d           ;4
    jp nz,playNote  ;10
 
    ld (cnt2),hl
    ld (out2),a
    exx
    exa
    ld (cnt1a),hl
    ld (cnt1b),ix
    ld (out1),a

    jp mainLoop

	if DRUMSET==0
drumType0
    add a,a
    ld b,0
    ld c,a
    ld hl,drumTable-118*2
    add hl,bc
    ld e,(hl)
    inc hl
    ld d,(hl)
    ex de,hl
    jp (hl)
    endif

	if DRUMSET==1
drumType1
	sub 116
	ld b,a
	ld a,128
	rla
	djnz $-1
	jp drumDigital
	endif

	if DRUMSET==0
drumTone1
    ld l,16
    jr drumTone
drumTone2
    ld l,12
    jr drumTone
drumTone3
    ld l,8
    jr drumTone
drumTone4
    ld l,6
    jr drumTone
drumTone5
    ld l,4
    jr drumTone
drumTone6
    ld l,2

drumTone
    ld de,3700
    ld bc,#0101
    ld a,(borderCol)
.l0
    out (#fe),a
    dec b
    jr nz,.l1
    xor 16
    ld b,c
    exa
    ld a,c
    add a,l
    ld c,a
    exa
.l1
    dec e
    jr nz,.l0
    dec d
    jr nz,.l0
    jp mainLoop

drumNoise1
    ld de,2480
    ld ixl,1
    jr drumNoise
drumNoise2
    ld de,1070
    ld ixl,10
    jr drumNoise
drumNoise3
    ld de,365
    ld ixl,101

drumNoise
    ld h,d
    ld l,e
    ld a,(borderCol)
    ld c,a
.l0
    ld a,(hl)
    and 16
    or c
    out (#fe),a
    ld b,ixl
    djnz $
    inc hl
    dec e
    jr nz,.l0
    dec d
    jr nz,.l0
    jp mainLoop
    endif

	if DRUMSET==1
drumDigital
	ld (.smpn),a
	ld a,(borderCol)
	ld d,a
	ld hl,smpData
	ld bc,1024
.l0
	ld a,(hl)		;7
.smpn=$+1
	and 0			;7
	ld a,d			;4
	jr nz,$+4		;7/12
	jr z,$+4		;7/12
	or 16			;7
	out (#fe),a		;11
	ld e,4			;7
	dec e			;4
	jr nz,$-1		;7/12=56
	inc hl			;6
	dec bc			;6
	ld a,b			;4
	or c			;4
	jr nz,.l0		;7/12=83t
	jp mainLoop
	endif

noteTable
    dw 178,189,200,212,225,238,252,267,283,300,318,337
    dw 357,378,401,425,450,477,505,535,567,601,637,675
    dw 715,757,802,850,901,954,1011,1071,1135,1202,1274,1350
    dw 1430,1515,1605,1701,1802,1909,2023,2143,2270,2405,2548,2700
    dw 2860,3030,3211,3402,3604,3818,4046,4286,4541,4811,5097,5400

	if DRUMSET==0
drumTable
    dw drumTone1,drumTone2,drumTone3,drumTone4,drumTone5,drumTone6,drumNoise1,drumNoise2,drumNoise3
	else
smpData
	incbin "smpset.bin"
	endif

end

    display "Load player to ",/d,begin," (",/d,end-begin," bytes)"
    display "Load music data to ",/d,MUSICDATA
    if DRUMSET=0
    display "Drumset: SNT"
    else
    display "Drumset: DGT"
    endif
    if WAITKEY=0
    display "Key wait mode: 'wait for press'"
    else
    display "Key wait mode: 'wait for release'"
    endif
    if LOOPMODE=0
    display "Loop mode: 'play in loop'"
    else
    display "Loop mode: 'play once'"
    endif

    savebin "player.bin",begin,end-begin

    endmodule