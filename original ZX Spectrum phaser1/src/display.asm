;отображение паттерна

viewPattern
    di
    ld hl,(curRow)
    ld bc,12
    scf
    ccf
    sbc hl,bc
    jr nc,$+5
    ld hl,0
    push hl
    ld hl,(tuneLength)
    ld bc,24
    sbc hl,bc
    ld b,h
    ld c,l
    pop hl
    ld a,h
    cp b
    jr c,.l0
    ld a,l
    cp c
    jr c,.l0
    ld h,b
    ld l,c
.l0
    ld (curRowOff),hl

    ld h,1
    ld l,0
    ld de,#5800
    ld a,24
.l1
    push af
    push hl
    push de

    ld de,(curRowOff)
    call dec4NumOut
    inc l

    push hl
    ld hl,(curRowOff)
    ld bc,tuneTempoDrum
    add hl,bc
    ld a,(hl)
    pop hl
    push af
    rra
    rra
    rra
    rra
    and #0f
    jr nz,$+4
    ld a,'.'-'0'
    add a,'0'
    call chrOutA
    pop af
    and #0f
    jr nz,$+4
    ld a,'.'-'0'
    add a,'0'
    call chrOutA
    inc l

    ex de,hl
    ld hl,(curRowOff)
    ld bc,tuneCh1Note
    add hl,bc
    ld b,tagName/256
    ld c,(hl)
    ex de,hl
    ld a,(bc)
    inc b
    call chrOutA
    ld a,(bc)
    inc b
    call chrOutA
    ld a,(bc)
    call chrOutA

    push hl
    ld hl,(curRowOff)
    ld bc,tuneCh1Ins
    add hl,bc
    ld a,(hl)
    pop hl
    or a
    jr nz,.l2
    ld e,'.'
    call chrOut
    ld e,'.'
    call chrOut
    jr .l3
.l2
    push af
    rra
    rra
    rra
    rra
    and #0f
    add a,'0'
    call chrOutA
    pop af
    and #0f
    add a,'0'
    call chrOutA
.l3
    inc l

    ex de,hl
    ld hl,(curRowOff)
    ld bc,tuneCh2Note
    add hl,bc
    ld b,tagName/256
    ld c,(hl)
    ex de,hl
    ld a,(bc)
    inc b
    call chrOutA
    ld a,(bc)
    inc b
    call chrOutA
    ld a,(bc)
    call chrOutA
 
    pop de
    push de

    ld hl,(curRowOff)
    ld bc,(curRow)
    ld a,h
    cp b
    jr nz,.la0
    ld a,l
    cp c
    jr nz,.la0
    ld c,64+7+8
    ld a,c
    jr .la1
.la0
    ld c,64+7
    ld a,(curRowOff)
    and 3
    jr z,$+4
    ld c,7
    xor a
.la1
    ld h,d
    ld l,e
    ld b,19
    ld (hl),c
    inc l
    djnz $-2
    pop de
 
    or a
    jr z,.l4
    push de
    ld hl,colCurAttr
    ld a,(curCol)
    ld c,a
    add hl,bc
    add hl,bc
    ld a,(hl)
    inc hl
    ld c,(hl)
    ex de,hl
    add hl,bc
    ld b,a
    ld c,64+48+1
    ld (hl),c
    inc l
    djnz $-2
    pop de
.l4
 
    ld hl,32
    add hl,de
    ex de,hl

    ld hl,(curRowOff)
    inc hl
    ld (curRowOff),hl
    pop hl
    pop af
    inc l
    dec a
    jp nz,.l1

    ei
    ret

;отображение настроек

viewOctave
    ld hl,#1f04
    ld a,(curOctave)
    jp dec1NumOut

viewAutoStep
    ld hl,#1e05
    ld de,(autoStep)
    jp dec2NumOut

viewLength
    ld hl,#1c06
    ld de,(tuneLength)
    jp dec4NumOut

viewLoop
    ld hl,#1c07
    ld de,(tuneLoop)
    jp dec4NumOut

viewInstr
    ld a,(curInstr)
    push af
    ld hl,#1e0b
    ld d,0
    ld e,a
    call dec2NumOut
    pop af
    call getInstrAddr
    ld a,(ix)
    ld hl,#1e0c
    ld d,0
    ld e,a
    call dec2NumOut
    ld e,(ix+1)
    ld d,(ix+2)
    ld hl,#1c0d
    call dec4NumOut
    ld d,0
    ld e,(ix+3)
    ld hl,#1d0e
    jp dec3NumOut

getMemSize
    push bc
    pop hl
    ld bc,compData
    or a
    sbc hl,bc

    push hl
    ld bc,100
    call STACK_BC
    pop bc
    call STACK_BC
    ld bc,compSize
    call STACK_BC
    rst #28
    db #05 ;/
    db #04 ;*
    db #38 ;end
    call FP_TO_BC
    ld (tuneMemSize),bc
    ret

viewMemSize
    ld hl,#1c08
    ld de,(tuneMemSize)
    ld a,d
    or e
    jp nz,dec3NumOut
    ld b,3
.l0
    call scrPos
    ld e,'-'
    call chrOut
    djnz $-5
    ret

viewCopy
    ld a,(copyBufCol)
    inc a
    ld d,0
    ld e,a
    ld hl,copyName
    add hl,de
    add hl,de
    ex de,hl
    ld hl,#1909
    call scrPos
    ld a,(de)
    inc de
    push de
    call chrOutA
    pop de
    ld a,(de)
    call chrOutA

    ld hl,#1c09
    ld de,(copyBufLen)
    ld a,d
    or e
    jp nz,dec4NumOut
    ld b,4
    jr viewMemSize.l0

viewDrumSet
    ld hl,#1d10
    ld a,(tuneDrumSet)
    or a
    ld bc,drumSetName
    jr z,$+5
    ld bc,drumSetName+4
    jp strOut

;очистка экрана и атрибутов

cls
    push bc
    xor a
    out (#fe),a
    ld a,64+7
    ld (BORDER),a
    ld (ATTR_P),a
    call CLS
    pop bc
    ret

;генерация таблицы адресов строк

makeScrTable
    ld hl,#4000
    ld de,scrTable
    ld b,24
.loop0
    ld a,l
    ld (de),a
    set 7,e
    ld a,h
    ld (de),a
    res 7,e
    inc e
    call downChr
    djnz .loop0

    ret

;получение адреса в экранной области
;вход: H=x,L=y
;выход: HL=адрес

scrPos
    push de
    ld e,h
    ld h,scrTable/256
    ld d,(hl)
    set 7,l
    ld h,(hl)
    ld l,d
    ld d,0
    add hl,de
    pop de
    ret

;переход на знакоместо вниз
;на основе DOWN_HL+ by Spencer Winsent

downChr
    ld a,l
    sub #e0
    ld l,a
    sbc a,a
    and #f8
    add a,h
    add a,8
    ld h,a
    ret

;закраска строки атрибутами
;вход: H=x,L=y,C=цвет,B=длина

setAttr
    push bc
    push de
    push hl
    ld e,h
    ld d,#58
    ld h,#00
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,hl
    add hl,de
.loop0
    ld (hl),c
    inc l
    djnz .loop0
    pop hl
    pop de
    pop bc
    ret

;вывод одного рязряда hex-числа
;вход: HL=адрес в экранной области, A=число

chrOutHex
    ld e,'.'
    or a
    jr z,chrOut
    ld e,'0'
    cp 10
    jr c,$+4
    ld e,'A'-10
    add a,e

;вывод одного символа
;вход: HL=адрес в экранной области, E=код символа

chrOutA
    ld e,a
chrOut
    ex de,hl
    ld h,0
    add hl,hl
    add hl,hl
    add hl,hl
    ld a,h
    add a,(15616/256)-1
    ld h,a
    ex de,hl

    dup 8
    ld a,(de)
    ld (hl),a
    inc e
    inc h
    edup
    org $-2
    ld a,h
    sub 7
    ld h,a
    inc l

    ret

;вывод строки текста
;вход: H=x,L=y, BC=адрес строки

strOut
    call scrPos
.loop0
    ld a,(bc)
    inc bc
    or a
    ret z
    cp #20
    jr nc,.loop1
    ld h,a
    ld a,(bc)
    ld l,a
    inc bc
    jr strOut
.loop1
    call chrOutA
    jr .loop0

;вывод десятичного числа, от одного до четырёх разрядов с нулями
;вход: H=x,L=y, DE=число

dec1NumOut
    call scrPos
    jp dec4NumOut.dec1
dec2NumOut
    call scrPos
    jp dec4NumOut.dec2
dec3NumOut
    call scrPos
    jp dec4NumOut.dec3
dec4NumOut
    call scrPos
    ex de,hl
    ld a,'0'
    ld bc,-1000
.loop0
    add hl,bc
    jr nc,.loop1
    inc a
    jp .loop0
.loop1
    sbc hl,bc
    ex de,hl
    push de
    call chrOutA
    pop de
.dec3
    ex de,hl
    ld a,'0'
    ld bc,-100
.loop2
    add hl,bc
    jr nc,.loop3
    inc a
    jp .loop2
.loop3
    sbc hl,bc
    ex de,hl
    push de
    call chrOutA
    pop de
.dec2
    ex de,hl
    ld a,'0'
    ld bc,-10
.loop4
    add hl,bc
    jr nc,.loop5
    inc a
    jp .loop4
.loop5
    sbc hl,bc
    ex de,hl
    push de
    call chrOutA
    pop de
    ld a,e
.dec1
    add a,'0'
    jp chrOutA

;отображение хелпа с раскраской
;вход: BC=адрес текста,A=y

showHelp
    ld (.row),a
    call cls
.loop0
    ld a,(bc)
    ld h,0
.row=$+1
    ld l,0
    ld (.pos),hl
    call scrPos
    ld e,0
.loop1
    ld a,(bc)
    inc bc
    push de
    push af
    call chrOutA
    pop af
    pop de
    inc e
    cp ':'
    jr nz,.loop1
    push hl
    push bc
.pos=$+1
    ld hl,0
    ld b,e
    ld c,64+6
    call setAttr
    pop bc
    pop hl
.loop2
    ld a,(bc)
    inc bc
    cp 32
    jr c,.loop3
    call chrOutA
    jr .loop2
.loop3
    or a
    jr z,.loop4
    ld hl,.row
    inc (hl)
    dec a
    jr nz,.loop3
    jr .loop0
.loop4
    ret