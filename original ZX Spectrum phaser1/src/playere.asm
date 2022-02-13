    module playere

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

;получение номера текущего инструмента
;выход: A=номер

getCurInstr
    ld a,3
    call main.getColAdr
    ld bc,(main.curRow)
    inc bc
    ld e,1
.l0
    ld a,b
    or c
    jr z,.l1
    ld a,(hl)
    or a
    jr z,$+5
    ld e,a
    jr .l1
    dec hl
    dec bc
    jp .l0
.l1
    ld a,e
	ret

;компиляция текста в формат проигрывателя
;выход: HL=стартовый адрес для текущей строки

compileSong
    di
    push iy

	ld hl,main.tuneCh1Ins
	ld bc,(main.tuneLength)
	ld e,1
.la0
	ld a,(hl)
	cp e
	jp c,.la1
	ld e,a
.la1
	inc hl
	dec bc
	ld a,b
	or c
	jp nz,.la0

	ld d,b
	ex de,hl
	add hl,hl
	add hl,hl
	ld b,h
	ld c,l
	ld hl,main.tuneInsTable
	ld de,main.compData+2
	ldir

	ex de,hl
	push hl
	ld bc,main.compData+2
	or a
	sbc hl,bc
	ld (main.compData),hl
	pop hl

    push hl
    pop iy
    ld ix,main.tuneTempoDrum

    ld hl,main.tuneCh1Note
    ld de,(main.curRow)
    add hl,de
    ld a,(hl)
    ld (.phase),a
    or 64
    ld (hl),a
    ld hl,main.tuneCh1Ins
    add hl,de
    ld a,(hl)
    ld (.instr),a
    push hl
    call getCurInstr
    pop hl
    ld (hl),a

    ld hl,0
    ld a,2
    ld (compileBlock.wait),hl
    ld (compileBlock.speed),a
    
    ld hl,(main.curRow)
    ld de,(main.tuneLoop)
    push hl
    or a
    sbc hl,de
    pop hl
    jr c,.l0
    ld (.len1),de
    or a
    sbc hl,de
    ld (.len2),hl
    xor a
    jr .l1
.l0
    ld (.len1),hl
    ex de,hl
    or a
    sbc hl,de
    ld (.len2),hl
    ld a,1
.l1
    ld (.rl),a
    ld hl,(main.tuneLength)
    ld bc,(.len1)
    or a
    sbc hl,bc
    ld bc,(.len2)
    or a
    sbc hl,bc
    ld (.len3),hl

    call .setStart

.len1=$+1
    ld bc,0
    call compileBlock
    jr nz,.l2
.rl=$+1
    ld a,0
    or a
    call nz,.setStart
    call  z,.setLoop
.len2=$+1
    ld bc,0
    call compileBlock
    jr nz,.l2
    ld a,(.rl)
    or a
    call  z,.setStart
    call nz,.setLoop
.len3=$+1
    ld bc,0
    call compileBlock
    jr nz,.l2
 
    ld (iy),0
    inc iy

	push hl
	ld hl,main.tuneCh1Note
	ld bc,(main.curRow)
	add hl,bc
.phase=$+1
	ld a,0
	ld (hl),a
	ld hl,main.tuneCh1Ins
	add hl,bc
.instr=$+1
	ld a,0
	ld (hl),a
	pop hl

	push iy
	pop bc
    pop iy
    ei
    ret

.l2
	ld hl,main.compData
	ld (hl),128+63
	inc hl
	ld (hl),1
	inc hl
	ld (hl),0
	dec hl
	dec hl
	pop iy
	ei
	ld a,2
	out (#fe),a
	ld b,10
	halt
	djnz $-1
	xor a
	out (#fe),a
	ld c,a
	ret

.setStart
    push iy
    pop hl
    ret

.setLoop
	ld (seqStart),iy
    ld (iy),128+63
    inc iy
    ret

compileBlock
    ld a,b
    or c
    ret z
    push hl
.l0
    push bc
    push ix
    ld bc,main.MAXROWS
    ld a,(ix)       ;tempo and drum
    add ix,bc
    ld d,(ix)       ;note ch1
    add ix,bc
    ld e,(ix)       ;instrument ch1
    add ix,bc
    ld b,(ix)       ;note ch2
    ld c,a
    and #0f
    jr nz,.l3
    ld a,d
    cp 62
    jr nz,.l3
    ld a,e
    or a
    jr nz,.l3
    ld a,b
    cp 62
    jr nz,.l3
.l1
    ld a,c
    rra
    rra
    rra
    rra
    and #0f
    jr z,.l2
    ld (.speed),a
.l2
.wait=$+1
    ld hl,0
.speed=$+1
    ld de,0
    add hl,de
    ld a,c
    and #0f
    jr z,$+3
    dec hl
    ld (.wait),hl
    pop ix
    pop bc
    inc ix
    dec bc
    ld a,iyh
    cp (main.compData+main.compSize)/256-1
    jr nc,$+10
    ld a,b
    or c
    jp nz,.l0

	call .pwait

	ld a,b
	or c
	pop hl
    ret

.l3
    call .pwait
    ld a,e
    or a
    jr z,.l4
    push bc
    call main.nibblesToDec
    pop bc
    dec a
    add a,a
    ld (iy),128+61
    inc iy
    ld (iy),a
    inc iy
.l4
    ld a,d
    cp 62
    jr z,.l5
    add a,128
    ld (iy),a
    inc iy
.l5
    ld a,b
    cp 62
    jr z,.l7
    ld a,d
    cp 62
    jr nz,.l6
    ld (iy),128+62
    inc iy
.l6
    ld a,b
    add a,128
    ld (iy),a
    inc iy
.l7
    ld a,c
    and #0f
    jr z,.l1
    add a,117
    ld (iy),a
    inc iy
	jp .l1

.pwait
	ld hl,(.wait)
    push bc
    ld bc,-117
.p0
    ld a,l
    add hl,bc
    jr nc,.p1
    ld (iy),117
    inc iy
    jp .p0
.p1
    or a
    jr z,.p2
    ld (iy),a
    inc iy
.p2
    ld hl,0
    ld (.wait),hl
    pop bc
    ret

;компиляция и проигрывание одной строки

playRow
	call getCurInstr
    push af
    ld ix,main.compData
    ld a,1
    call main.getColAdr
    ld a,(hl)
    and #0f
    or a
    jr z,.l0
    add a,117
    ld (ix),a       ;drum
    inc ix
.l0
    ld (ix),128+61  ;set instrument
    inc ix
    pop af
    dec a
    add a,a
    ld (ix),a
    inc ix
    ld a,2          ;note ch1
    call main.getColAdr
    ld a,(hl)
    add a,128+64
    ld (ix),a
    inc ix
    ld a,5          ;note ch2
    call main.getColAdr
    ld a,(hl)
    add a,128
    ld (ix),a
    inc ix
    ld (ix),128+63  ;loop
    inc ix
    ld (ix),117     ;wait
    inc ix
    ld (ix),0
	ld a,main.OP_JR_Z
	ld (playerWaitKey),a
    ld hl,main.compData
	jr player

;проигрыватель скомпилированных данных

playSong
    ld hl,main.compData
    ld a,main.OP_JR_NZ
    ld (playerWaitKey),a
    jr player

playSongRow
	ld a,main.OP_JR_Z
	ld (playerWaitKey),a

player
    di
    push iy
    push hl

	ld a,(main.tuneDrumSet)
	ld hl,drumType0
	or a
	jr z,$+5
	ld hl,drumType1
	ld (drumType),hl

    ld hl,main.tuneInsTable
    ld (insTable),hl
    ld (curInsOff),hl
 
    xor a
    ld (borderCol),a

    ld h,a
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
    jp z,restart ;end of song, loop
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
    pop iy
    ei
    ret

render
    and 127
    cp 118
drumType=$+1
    jp nc,drumType0
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

drumType1
	sub 116
	ld b,a
	ld a,128
	rla
	djnz $-1
	jp drumDigital

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

noteTable
    dw 178,189,200,212,225,238,252,267,283,300,318,337
    dw 357,378,401,425,450,477,505,535,567,601,637,675
    dw 715,757,802,850,901,954,1011,1071,1135,1202,1274,1350
    dw 1430,1515,1605,1701,1802,1909,2023,2143,2270,2405,2548,2700
    dw 2860,3030,3211,3402,3604,3818,4046,4286,4541,4811,5097,5400

drumTable
    dw drumTone1,drumTone2,drumTone3,drumTone4,drumTone5,drumTone6,drumNoise1,drumNoise2,drumNoise3

smpData
	incbin "smpset.bin"

    endmodule