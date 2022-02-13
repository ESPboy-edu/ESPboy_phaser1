    device zxspectrum128;48
    module main

    org #e000


MAXROWS equ 2048    ;строк в треке
PAGEROW equ 16      ;строк в странице

STACK_A     equ #2d28
FP_TO_A     equ #2dd5
STACK_BC    equ #2d2b
FP_TO_BC    equ #2da2

CLS         equ #0d6b
BORDER      equ #5c48
ATTR_P      equ #5c8d

OP_NOP      equ #00
OP_XOR_A	equ #af
OP_JR_NZ	equ #20
OP_JR_Z		equ #28



begin
    jp $+6
    jp editorStart

    call makeScrTable
    call makeNoteNames
    call tuneClearAll

    call cls
    ld hl,#050a
    ld bc,#1670
    call setAttr
    inc h
    inc h
    ld bc,strTitle
    call strOut

    jp waitKey

editorStart
    call cls
    ld bc,strEditor
    call strOut
    call viewOctave
    call viewAutoStep
    call viewLength
    call viewLoop
    call viewInstr
    call viewMemSize
    call viewCopy
    call viewDrumSet

editorLoop
    call viewPattern

pollKbd
    call waitKey

    cp 11           ;UP
    jr nz,.key0
    ld bc,1
    call moveCurUp
    jp .keyE
.key0
    cp 10           ;DOWN
    jr nz,.key1
    ld bc,1
    call moveCurDown
    jp .keyE
.key1
    cp 4            ;PAGEUP
    jr nz,.key2
    ld bc,PAGEROW
    call moveCurUp
    jp .keyE
.key2
    cp 5            ;PAGEDOWN
    jr nz,.key3
    ld bc,PAGEROW
    call moveCurDown
    jp .keyE
.key2.0
    ld hl,MAXROWS-1
    ld (curRow),hl
    jp .keyE
.key3
    cp 8            ;LEFT
    jr nz,.key4
    ld hl,curCol
    ld a,(hl)
    dec a
    cp 5
    jr c,$+4
    ld a,5
    ld (hl),a
    jp .keyE
.key4
    cp 9            ;RIGHT
    jr nz,.key5
    ld hl,curCol
    ld a,(hl)
    inc a
    cp 6
    jr c,$+3
    xor a
    ld (hl),a
    jp .keyE
.key5
    cp 32           ;MENU
    jr nz,.key6
    jp menuStart
.key6
    cp 13           ;проигрывание с текущей позиции
    jr nz,.key7
    call playere.compileSong
    push hl
    call getMemSize
    call viewMemSize
    pop hl
    call playere.playSongRow
    jp .keyE
.key7
    cp 38           ;изменение autostep
    jr nz,.key8
    ld hl,#1e05
    ld de,(autoStep)
    ld b,2
    call numInput
    ld bc,0
    ld de,100
    call minMax
    ld (autoStep),hl
    call viewAutoStep
    jp .keyE
.key8
    cp 12           ;стирание ноты со сдвигом
    jr nz,.key9
    call rowDelete
    jp .keyE
.key9
    cp 201          ;INSERT
    jr nz,.key10
    call rowInsert
    jp .keyE
.key10
    cp 199          ;HOME
    jr nz,.key12
    ld hl,0
    ld (curRow),hl
    jp .keyE
.key12
    cp 200          ;END
    jr nz,.key13
    ld hl,(tuneLength)
    dec hl
    ld (curRow),hl
    jp .keyE
.key13
.key14
    cp 15           ;DELETE
    jr nz,.key15
    call getNoteAdr
    exa
    ld a,(curCol)
    cp 2
    jr c,.key14.0
    exa
    ld a,0
    jr nz,$+4
    ld a,62
    ld (hl),a
    jr .key14.1
.key14.0
    or a
    ld c,#f0
    jr nz,$+4
    ld c,#0f
    ld a,(hl)
    and c
    ld (hl),a
.key14.1
    call autoCurDown
    jp .keyE
.key15
    cp 6            ;сброс фазы дл€ ноты
    jr nz,.key16
    ld a,(curCol)
    cp 2
    jp nz,.keyE
    call getNoteAdr
    jp nz,.keyE
    ld a,(hl)
    xor 64
    ld (hl),a
    jp .keyE
.key16
    cp 108          ;установка начала цикла
    jr nz,.key17
    ld bc,(curRow)
    ld (tuneLoop),bc
    call viewLoop
    jp .keyE
.key17
    cp 7            ;редактор инструмента
    jr nz,.key20
    ld a,(curCol)
    sub 3
    cp 2
    jp nc,.key17.0
    call getNoteAdr
    ld a,(hl)
    or a
    jr z,.key17.0
    call nibblesToDec
    ld (curInstr),a
.key17.0
    jp instrEditor
.key20
    cp 41           ;изменение длины трека
    jr nz,.key21
    ld hl,#1c06
    ld de,(tuneLength)
    ld b,4
    call numInput
    ld bc,24
    ld de,MAXROWS
    call minMax
    ld (tuneLength),hl
    call viewLength
    ld hl,(tuneLoop)
    ld bc,0
    ld de,(tuneLength)
    dec de
    call minMax
    ld (tuneLoop),hl
    call viewLoop
    ld hl,(curRow)
    ld bc,0
    ld de,(tuneLength)
    dec de
    call minMax
    ld (curRow),hl
    jp .keyE
.key21
.key23
    cp 'k'          ;проиграть текущий р€д
    jr nz,.key24
    call playere.playRow
    jp .keyE
.key24
	cp 'i'			;переключение наборов барабанов
	jr nz,.key100
	ld hl,tuneDrumSet
	ld a,(hl)
	xor 1
	ld (hl),a
	call viewDrumSet
	jp .keyE
.key100
    ld c,a          ;ввод ноты
    call getNoteAdr
    ld a,c
    jr nz,.key101
    push hl
    call noteKeys
    push af
    push bc
    call viewOctave
    pop bc
    pop af
    pop hl
    jr nz,.keyE
    ld a,c
    ld (hl),a
.key100.1
    ld bc,(curRow)
    push bc
    call autoCurDown
    pop bc
    ld de,(curRow)
    push de
    push bc
    call viewPattern
    pop bc
    ld (curRow),bc
    call playere.playRow
    pop de
    ld (curRow),de
    jr .keyE
.key101
    cp '0'
    jr c,.keyE
    cp '9'+1
    jr nc,.keyE
    sub '0'
    ld c,a
    ld a,(curCol)
    or a
    jr z,.key101.0
    cp 3
    jr z,.key101.0
    ld a,(hl)
    and #f0
    jr .key101.1
.key101.0
    ld a,(hl)
    and #0f
    sla c
    sla c
    sla c
    sla c
.key101.1
    or c
    ld (hl),a
    ld a,(curCol)
    cp 1
    jr z,.key100.1
    call autoCurDown
.keyE
    jp editorLoop

;редактор инструмента

instrEditor
    ld a,(curInstr)
    call getInstrAddr
    ld a,(curInsParam)
    cp 1
    jr z,.l1
    cp 2
    jr z,.l2
    cp 3
    jp z,.l3

.l0
    xor a				;instrument number
    ld (curInsParam),a
    call viewInstr
    ld hl,#1e0b
    ld a,(curInstr)
    ld d,0
    ld e,a
    ld b,2
    call numInput
    push af
    ld bc,1
    ld de,99
    call minMax
    ld a,l
    ld (curInstr),a
    pop af
    cp 11
    jr z,.l3
    cp 7
    jp z,.l4
.l1
    ld a,1				;multiple
    ld (curInsParam),a
    call viewInstr
    ld hl,#1e0c
    ld d,0
    ld e,(ix)
    ld b,2
    call numInput
    push af
    ld bc,0
    ld de,16
    call minMax
    ld (ix),l
    pop af
    cp 11
    jr z,.l0
    cp 7
    jr z,.l4
.l2
    ld a,2				;detune
    ld (curInsParam),a
    call viewInstr
    ld hl,#1c0d
    ld e,(ix+1)
    ld d,(ix+2)
    ld b,4
    call numInput
    push af
    ld bc,0
    ld de,9999
    call minMax
    ld (ix+1),l
    ld (ix+2),h
    pop af
    cp 11
    jr z,.l1
    cp 7
    jr z,.l4
.l3
    ld a,3				;phase
    ld (curInsParam),a
    call viewInstr
    ld hl,#1d0e
    ld d,0
    ld e,(ix+3)
    ld b,3
    call numInput
    push af
    ld bc,0
    ld de,255
    call minMax
    ld (ix+3),l
    pop af
    cp 11
    jr z,.l2
    cp 7
    jp nz,.l0
.l4
	call viewInstr
	jp editorLoop

;получение адреса инструмента
;вход: A=номер инструмента
;выход: IX=адрес

getInstrAddr
    dec a
    ld h,0
    ld l,a
    add hl,hl
    add hl,hl
    ld bc,tuneInsTable
    add hl,bc
    push hl
    pop ix
    ret

;конверси€ двух полубайт в двухзначное дес€тичное число
;вход: A=число
;выход: A=число

nibblesToDec
    ld c,a
    and #f0
    ld a,c
    ret z
    rra
    rra
    rra
    rra
    and #0f
    ld b,a
    ld a,c
    and #0f
    ld c,10
    add a,c
    djnz $-1
    ret

;перемещение курсора вверх
;вход: BC=на сколько строк

moveCurUp
    ld hl,(curRow)
    sbc hl,bc
    jr nc,$+5
    ld hl,0
    ld (curRow),hl
    ret

;перемещение курсора вниз
;вход: BC=на сколько строк

autoCurDown
    ld bc,(autoStep)
moveCurDown
    ld hl,(curRow)
    add hl,bc
    ld bc,(tuneLength)
    sbc hl,bc
    jr c,$+5
    ld hl,-1
    add hl,bc
    ld (curRow),hl
    ret

;получение адреса текущей ноты или огибающей
;выход: A=0 или 1 (нота или огибающа€), HL=адрес

getNoteAdr
    ld a,(curCol)
getColAdr
    push bc
    push de
    ld de,(curRow)
    ld c,0
    cp c
    ld hl,tuneTempoDrum
    jr z,.env
    inc c
    cp c
    ld hl,tuneTempoDrum
    jr z,.env
    inc c
    cp c
    ld hl,tuneCh1Note
    jr z,.note
    inc c
    cp c
    ld hl,tuneCh1Ins
    jr z,.env
    inc c
    cp c
    ld hl,tuneCh1Ins
    jr z,.env
    ld hl,tuneCh2Note
    jr .note
.env
    ld a,1
    jr $+3
.note
    xor a
    add hl,de
    pop de
    pop bc
    or a
    ret

;обработка клавиш выбора октавы и ноты
;вход: A=код клавиши
;выход: если A=0, —=номер ноты

noteKeys
    ld hl,noteKeyTable
    ld b,noteTable-noteKeyTable
.key0
    cp (hl)
    jr z,.key1
    inc hl
    djnz .key0
    or a
    ret
.key1
    ld bc,noteTable-noteKeyTable
    add hl,bc
    ld a,(hl)
    cp 128
    jr c,.key2
    sub 127
    ld (curOctave),a
    or a
    ret
.key2
    cp 60
    jr z,.key3
    push af
    ld a,(curOctave)
    ld b,a
    pop af
    sub 12
    add a,12
    djnz $-2
    cp 60
    jr c,$+4
    ld a,59
.key3
    ld c,a
    xor a
    ret

;ввод строки
;вход: H=x,L=y,DE=адрес,B=длина

strInput
    ld a,OP_NOP
    ld (strInputM.mode),a
strInputM
    xor a
    ld (.cur),a
    ld (.off),de
.loop0
    push hl
    push de
    push bc
    ld c,#4f
    call setAttr
    push bc
    push hl
.cur=$+1
    ld a,0
    add a,h
    ld h,a
    ld bc,#0170
    call setAttr
    pop hl
    pop bc
    call scrPos
.off=$+1
    ld de,0
.loop1
    ld a,(de)
    inc de
    push de
    call chrOutA
    pop de
    djnz .loop1
 
    call waitKey
    pop bc
    pop de
    pop hl
 
    cp 7            ;EDIT
    ret z
    cp 10           ;UP
    ret z
    cp 11           ;DOWN
    ret z
    cp 13           ;ENTER
    ret z
.key0
    cp 8            ;LEFT
    jr nz,.key1
.key0.0
    ld a,(.cur)
    or a
    jr z,.keyE
    dec a
    ld (.cur),a
    dec de
    jr .keyE
.key1
    cp 9            ;RIGHT
    jr nz,.key2
.key1.0
    ld a,(.cur)
    inc a
    cp b
    jr nc,.keyE
    ld (.cur),a
    inc de
    jr .keyE
.key2
    cp '0'          ;ввод цифры
    jr c,.key3
    cp ':'
    jr nc,.key3
    ld (de),a
    jr .key1.0
.key3
.mode=$
    nop
    cp 32           ;ввод буквы
    jr c,.key4
    cp 128
    jr nc,.key4
    ld (de),a
    jr .key1.0
.key4
    cp 12           ;стирание буквы
    jr nz,.keyE
    ld a,32
    ld (de),a
    jr .key0.0
.keyE
    jr .loop0

;ввод числа
;вход: H=x,L=y,B=количество разр€дов,DE=число
;выход: HL=число

numInput
    push hl
    push bc
    ld hl,#5800
    ld bc,#0300
.loop0
    ld a,(hl)
    and %00111000
    cp %00110000
    jr nz,$+4
    ld (hl),64+7+8
    inc hl
    dec bc
    ld a,b
    or c
    jr nz,.loop0
    ex de,hl
    ld de,strNumber
    ld bc,-10000
    call .setDigit
    ld bc,-1000
    call .setDigit
    ld bc,-100
    call .setDigit
    ld bc,-10
    call .setDigit
    ld a,l
    add a,'0'
    ld (de),a
    pop bc
    xor a
    ld a,b
    ld c,b
    ld b,0
    ld hl,strNumber+5
    sbc hl,bc
    ex de,hl
    ld b,a
    pop hl
    ld a,OP_XOR_A
    ld (strInputM.mode),a
    push hl
    push bc
    call strInputM
    pop bc
    pop hl
    push af
    ld c,64+7
    call setAttr
    ld de,strNumber
    ld hl,0
    ld bc,10000
    call .mulDigit
    ld bc,1000
    call .mulDigit
    ld bc,100
    call .mulDigit
    ld bc,10
    call .mulDigit
    ld a,(de)
    sub '0'
    ld c,a
    add hl,bc
    pop af
    ret

.setDigit
    ld a,'0'
.setDigit0
    add hl,bc
    jr nc,.setDigit1
    inc a
    jr .setDigit0
.setDigit1
    sbc hl,bc
    ld (de),a
    inc de
    ret

.mulDigit
    ld a,(de)
    inc de
    sub '0'-1
.mulDigit0
    dec a
    ret z
    add hl,bc
    jr .mulDigit0

;ограничение минимального и максимального значени€
;вход: HL=число,BC=минимум,DE=максимум
;выход: HL=число

minMax
    push hl
    xor a
    sbc hl,bc
    pop hl
    jr nc,$+4
    ld h,b
    ld l,c
    push hl
    xor a
    sbc hl,de
    pop hl
    jr c,$+4
    ld h,d
    ld l,e
    ret

;очистка трека

tuneClearAll
    ld hl,tuneInsTable
    ld de,tuneInsTable+1
    ld bc,4*100-1
    ld (hl),0
    ldir
    ld hl,tuneInsTable
    ld de,4
    ld b,100
.l0
	ld (hl),1
	add hl,de
	djnz .l0
    ld hl,tuneTempoDrum
    ld de,tuneTempoDrum+1
    ld bc,MAXROWS-1
    ld (hl),0
    ldir
    ld hl,tuneCh1Note
    ld de,tuneCh1Note+1
    ld bc,MAXROWS-1
    ld (hl),62
    ldir
    ld hl,tuneCh1Ins
    ld de,tuneCh1Ins+1
    ld bc,MAXROWS-1
    ld (hl),0
    ldir
    ld hl,tuneCh2Note
    ld de,tuneCh2Note+1
    ld bc,MAXROWS-1
    ld (hl),62
    ldir
    ld hl,tuneName
    ld de,tuneName+1
    ld bc,31
    ld (hl),32
    ldir
 
    ld hl,256
    ld (tuneLength),hl
    ld hl,0
    ld (tuneLoop),hl
    xor a
    ld (tuneDrumSet),a

settingsClear
	ld hl,0
    ld (curRow),hl
    ld (tuneMemSize),hl
    xor a
    ld (curCol),a
    ld (curInsParam),a
    inc a
    ld (autoStep),a
    ld (curInstr),a
    inc a
    ld (curOctave),a
    ld hl,16
    ld (copyLen),hl
    ld (blockLen),hl
	ld a,255
    ld (copyBufCol),a

    ret

;копирование блока в буфер

blockCopy
    ld hl,(curRow)
    ld bc,(copyLen)
    add hl,bc
    push bc
    ld bc,-MAXROWS
    add hl,bc
    pop bc
    jr nc,.l0
    ld hl,MAXROWS
    ld bc,(curRow)
    scf
    ccf
    sbc hl,bc
    ld b,h
    ld c,l
.l0
    ld (copyBufLen),bc
    ld a,(curCol)
    ld (copyBufCol),a
    call getColAdr
    ld de,copyBuf
    ldir
    ret

;вставка блока из буфера

blockPaste
    ld bc,(copyBufLen)
    ld a,b
    or c
    jr z,.l0
    ld a,(copyBufCol)
    ld e,a
    ld d,0
    ld hl,copyFromTo
    add hl,de
    add hl,de
    ld a,(hl)
    inc hl
    ld d,(hl)
    ld e,a
    ld a,(curCol)
    cp e
    jr z,.l1
    cp d
    jr z,.l1
.l0
    ld a,2
    out (#fe),a
    ld b,5
    halt
    djnz $-1
    ld a,b
    out (#fe),a
    ret
.l1
    call getColAdr
    ld de,copyBuf
    ex de,hl
    ldir
    ret

;раст€гивание блока вдвое
;вход: HL=длина

blockExpand
	xor a
	ld e,a
	call .expand
	inc a
	inc a
	ld e,62
	call .expand
	inc a
	ld e,0
	call .expand
	inc a
	inc a
	ld e,62

.expand
	push af
	push hl
	ld (.col0),a
	ld (.col1),a
	ld a,e
	ld (.clr),a
	ex de,hl
	ld hl,(curRow)
	add hl,de
	add hl,de
	ld bc,-MAXROWS
	add hl,bc
	ex de,hl
	jr nc,.l0
	ld hl,MAXROWS
	ld bc,(curRow)
	or a
	sbc hl,bc
	srl h
	rr l
.l0
	ld a,h
	or l
	jr z,.l3
	push hl

	ld b,h
	ld c,l
	ld hl,(curRow)
	add hl,bc
	ex de,hl
	ld h,d
	ld l,e
	add hl,bc
	ex de,hl
	push hl
	ld hl,MAXROWS
	ld bc,(curRow)
	or a
	sbc hl,bc
	or a
	sbc hl,de
	ld b,h
	ld c,l
	pop hl
	
	jr c,.l1

	push bc
	add hl,bc
	dec hl
	ex de,hl
	add hl,bc
	dec hl
	ex de,hl
	push hl
.col0=$+1
	ld a,0
	call getColAdr
	ld b,h
	ld c,l
	pop hl
	add hl,bc
	ex de,hl
	add hl,bc
	ex de,hl
	pop bc
	lddr

.l1
	pop bc
.col1=$+1
	ld a,0
	call getColAdr
	ld d,h
	ld e,l
	add hl,bc
	ex de,hl
	add hl,bc
	add hl,bc
.l2
	dec de
	ld a,(de)
	dec hl
.clr=$+1
	ld (hl),0
	dec hl
	ld (hl),a
	dec bc
	ld a,b
	or c
	jp nz,.l2

.l3
	pop hl
	pop af
	ret

;сжатие блока вдвое
;вход: HL=длина

blockShrink
	xor a
	ld e,a
	call .shrink
	inc a
	inc a
	ld e,62
	call .shrink
	inc a
	ld e,0
	call .shrink
	inc a
	inc a
	ld e,62

.shrink
	push af
	push hl
	ld (.col0),a
	ld (.col1),a
	ld a,e
	ld (.clr),a
	srl h
	rr l
	ex de,hl
	ld hl,(curRow)
	add hl,de
	add hl,de
	ld bc,-MAXROWS
	add hl,bc
	ex de,hl
	jr nc,.l0
	ld hl,MAXROWS
	ld bc,(curRow)
	or a
	sbc hl,bc
	srl h
	rr l
.l0
	ld a,h
	or l
	jr z,.l4
	push hl

	ld b,h
	ld c,l
.col0=$+1
	ld a,0
	call getColAdr
	ld d,h
	ld e,l
.l1
	ld a,(hl)
	ld (de),a
	inc hl
	inc hl
	inc de
	dec bc
	ld a,b
	or c
	jp nz,.l1

	pop bc
	ld hl,(curRow)
	add hl,bc
	ex de,hl
	ld h,d
	ld l,e
	add hl,bc
	push hl
	ld b,h
	ld c,l
	ld hl,MAXROWS
	or a
	sbc hl,bc
	jr c,.l4
	ld b,h
	ld c,l
	pop hl
	push bc
	push hl
.col1=$+1
	ld a,0
	call getColAdr
	ld bc,(curRow)
	or a
	sbc hl,bc
	ld b,h
	ld c,l
	pop hl
	push de
	pop ix
	add hl,bc
	ex de,hl
	add hl,bc
	ex de,hl
	pop bc
	ld a,b
	or c
	jr z,.l2
	add ix,bc
	ldir
.l2
	push ix
	pop bc
	ld hl,MAXROWS
	or a
	sbc hl,bc
	jr c,.l4
	ex de,hl
.clr=$+1
	ld b,0
.l3
	ld (hl),b
	inc hl
	dec de
	ld a,d
	or e
	jp nz,.l3
.l4
	pop hl
	pop af
	ret

;вставка пустой ноты со сдвигом

rowInsert
    ld hl,MAXROWS
    ld bc,(curRow)
    sbc hl,bc
    ld b,h
    ld c,l
    ld a,(curCol)
    ld e,62
    cp 2
    jr z,.l0
    cp 5
    jr z,.l0
    ld e,0
.l0
    push af
    push de
    call getColAdr
    pop de
    ld a,e
    ld (.clr),a
    call .move
    pop af
    cp 2
    ret nz
    ld de,MAXROWS
    add hl,de
    xor a
    ld (.clr),a
.move
    push hl
    push bc
    add hl,bc
    dec hl
    ld d,h
    ld e,l
    dec hl
    dec bc
    ld a,b
    or c
    jr z,$+4
    lddr
    pop bc
    pop hl
.clr=$+1
    ld (hl),0
    ret

;удаление ноты со сдвигом

rowDelete
    ld hl,MAXROWS
    ld bc,(curRow)
    sbc hl,bc
    ld b,h
    ld c,l
    ld a,(curCol)
    ld e,62
    cp 2
    jr z,.l0
    cp 5
    jr z,.l0
    ld e,0
.l0
    push af
    push de
    call getColAdr
    pop de
    ld a,e
    ld (.clr),a
    call .move
    pop af
    cp 2
    ret nz
    ld de,MAXROWS
    add hl,de
    xor a
    ld (.clr),a
.move
    push hl
    push bc
    ld d,h
    ld e,l
    inc hl
    dec bc
    ld a,b
    or c
    jr z,$+4
    ldir
    dec hl
.clr=$+1
    ld (hl),0
    ex de,hl
    pop bc
    pop hl
    ret

;генераци€ названий нот

makeNoteNames
    ld hl,tagName
    ld de,tagName+1
    ld bc,3*256-1
    ld (hl),'.'
    ldir

    ld hl,tagName
    ld de,noteName
    call .makeloop
    ld hl,tagName+64
    ld de,noteName+24

.makeloop
    ld bc,#0500+'1'
.l0
    push bc
    push de
    ld b,12
.l1
    ld a,(de)
    ld (hl),a
    inc h
    inc de
    ld a,(de)
    ld (hl),a
    inc h
    inc de
    ld (hl),c
    dec h
    dec h
    inc l
    djnz .l1
    pop de
    pop bc
    inc c
    djnz .l0
    ld (hl),'R'
    ret

;ожидание нажати€ клавиши
;выход: A=код клавиши

waitKey
.loop0
    bit 5,(iy+1)
    jr z,.loop0
    ld a,(23560)
    res 5,(iy+1)
    ret


	include "display.asm"
	include "menu.asm"
	include "playere.asm"


colCurAttr
    db 1,6,1,7,3,9,1,12,1,13,3,15

noteName    db "C-C#D-D#E-F-F#G-G#A-A#B-c-c#d-d#e-f-f#g-g#a-a#b-"

noteKeyTable
    db 33,64,35,36,37   ;выбор октавы
    db "zsxdcvgbhnjm"   ;нижн€€ октава
    db "q2w3er5t6y7u"   ;верхн€€ октава
    db 'a'              ;выключение ноты
noteTable
    db 128,129,130,131,132
    db 0,1,2,3,4,5,6,7,8,9,10,11
    db 12,13,14,15,16,17,18,19,20,21,22,23
    db 60

strTitle    db #06,#0a,"PHASER 1        v1.0"
            db #0c,#0b,"by Shiru 03'10",0
strEditor   db #14,#04,"Octave:"
            db #14,#05,"Autostep:"
            db #14,#06,"Length:"
            db #14,#07,"Loop:"
            db #14,#08,"Memory:"
            db #14,#09,"Copy:"
            db #1f,#08,"%"
            db #14,#0b,"Inst.:"
            db #14,#0c,"Multiple:"
            db #14,#0d,"Detune:"
            db #14,#0e,"Phase:"
            db #14,#10,"Drumset:"
            db #14,#16,"CS+1:Inst.ed"
            db #14,#17,"Space:  Menu",0

copyFromTo
	db 0,1,0,1,2,5,3,4,3,4,2,5
copyName
	db "--","TD","TD","N1","IN","IN","N2"
drumSetName
	db "SNT",0
	db "DGT",0


end

    display "Code ",begin,"..",$," ",/d,$-begin

curRow          dw 0
curRowOff       dw 0
curCol          db 0
curOctave       db 0
autoStep        dw 0
strNumber       ds 5
curInstr        db 0
curInsParam		db 0
tuneMemSize		dw 0
copyLen         dw 0
copyBufLen      dw 0
copyBufCol      db 0
blockLen		dw 0

    align 256

scrTable
    ds 256
tagName
    ds 3*256

    display "Top ",$

tuneData=#6200
tuneInsTable=tuneData
tuneTempoDrum=tuneInsTable+4*100
tuneCh1Note=tuneTempoDrum+MAXROWS
tuneCh1Ins=tuneCh1Note+MAXROWS
tuneCh2Note=tuneCh1Ins+MAXROWS
tuneName=tuneCh2Note+MAXROWS
tuneLength=tuneName+32
tuneLoop=tuneLength+2
tuneDrumSet=tuneLoop+2
tuneDataSize=(tuneDrumSet+2)-tuneData

copyBuf=#8400

compData=#8c00
compSize=begin-compData-#400

    display "Song text ",tuneData,"..",tuneData+tuneDataSize-1," ",/d,tuneDataSize
    display "Copy buffer ",copyBuf,".. ",copyBuf+MAXROWS-1," ",/d,MAXROWS
    display "Compiled data ",compData,"..",compData+compSize-1," ",/d,compSize

    savesna "main.sna",begin
    savebin "phaser1.bin",begin,end-begin