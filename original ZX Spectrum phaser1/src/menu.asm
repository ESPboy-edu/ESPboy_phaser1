;главное меню

menuStart
    ld hl,#5800
    ld bc,#0300
.loop0
    ld a,(hl)
    and %00111000
    ld e,1
    jr z,$+10
    and %00110000
    ld e,8
    jr z,$+4
    ld e,24
    ld (hl),e
    inc hl
    dec bc
    ld a,b
    or c
    jr nz,.loop0

    ld hl,#0016
    ld bc,#404f
    call setAttr
    ld bc,strMainMenu
    call strOut
    ld hl,#0017
    call strOut
    ld hl,#0116
    ld bc,#0170
    call setAttr
    ld h,#06
    call setAttr
    ld h,#0b
    call setAttr
    ld h,#10
    call setAttr
    ld hl,#0117
    call setAttr
    ld h,#06
    call setAttr
    ld h,#0c
    call setAttr
    ld h,#13
    call setAttr

.loop1
    call waitKey
 
    cp 'f'          ;FILE
    jr nz,.key0
    jp fileMenuStart
.key0
    cp 'c'          ;COPY
    jr nz,.key1
    ld de,(copyLen)
    call getBlockLength
    cp 7
    jp z,.keyE
    ld (copyLen),hl
    call blockCopy
    jp .keyE
.key1
    cp 'p'          ;PASTE
    jr nz,.key2
    call blockPaste
    jp .keyE
.key2
    cp 'h'          ;HEAR
    jr nz,.key3
    ld hl,#0016
    ld bc,#2000
    call setAttr
    inc l
    ld bc,#204f
    call setAttr
    ld bc,strPlay
    call strOut
    call playere.compileSong
    call getMemSize
    call viewMemSize
    halt
    halt
    halt
    call playere.playSong
    jr .keyE
.key3
    cp 'n'          ;NAME
    jr nz,.key4
    ld hl,#0016
    ld bc,#2000
    call setAttr
    inc l
    ld bc,#2047
    call setAttr
    ld de,tuneName
    ld b,32
    call strInput
    jr .keyE
.key4
    cp 'i'          ;INFO
    jr nz,.key5
    ld bc,strHelpMain
    xor a
    call showHelp
    call waitKey
    jr .keyE
.key5
	cp 'e'			;EXPAND
	jr nz,.key6
	ld de,(blockLen)
	call getBlockLength
	cp 7
	jr z,.keyE
	ld (blockLen),hl
	call blockExpand
	jr .keyE
.key6
	cp 's'			;SHRINK
	jr nz,.key7
	ld de,(blockLen)
	call getBlockLength
	cp 7
	jr z,.keyE
	ld (blockLen),hl
	call blockShrink
	jr .keyE
.key7
    cp 32
    jp nz,.loop1
.keyE
    jp editorStart

;ввод длины блока

getBlockLength
	push de
    ld hl,#0016
    ld bc,#2000
    call setAttr
    inc l
    ld bc,#204f
    call setAttr
    ld bc,strCopy
    call strOut
    pop de
    ld hl,#0f17
    ld b,4
    call numInput
    push af
    ld bc,1
    ld de,MAXROWS
    call minMax
    pop af
	ret

;файловое меню

fileMenuStart
    ld hl,#0016
    ld bc,#2000
    call setAttr
    inc l
    ld bc,#204f
    call setAttr
    ld bc,strFileMenu
    call strOut
    ld hl,#0117
    ld bc,#0170
    call setAttr
    ld h,#05
    call setAttr
    ld h,#0a
    call setAttr
    ld h,#0f
    call setAttr

.loop0
    call waitKey

.key0
    cp 'n'          ;NEW
    jr nz,.key1
    call confirmDialog
    ld a,e
    or a
    call nz,tuneClearAll
    jp editorStart
.key1
    cp 'l'          ;LOAD
    jr nz,.key2
    call settingsClear
    ld bc,0
    jp exitToBasic
.key2
    cp 's'          ;SAVE
    jr nz,.key3
    ld bc,1
    jp exitToBasic
.key3
    cp 'c'          ;COMPILE
    jr nz,.key4
    call playere.compileSong
    ld h,b
    ld l,c
    ld bc,compData
    or a
    sbc hl,bc
    ld b,h
    ld c,l
	jp exitToBasic
.key4
    cp 32
    jp nz,.loop0
    jp editorStart

;возврат в Basic

exitToBasic
	exx
	ld hl,10072
	exx
	ld iy,#5c3a
	ret

;подтверждение

confirmDialog
    ld hl,#0017
    ld bc,#2057
    call setAttr
    ld bc,strConfirm
    call strOut
    ld hl,#0717
    ld bc,#0170
    call setAttr
    ld hl,#0b17
    call setAttr

.loop0
    call waitKey
    ld e,0
    cp 'n'
    ret z
    inc e
    cp 'y'
    ret z
    jr .loop0

strMainMenu db " File Hear Name Info            ",0
			db " Copy Paste Expand Shrink       ",0
strFileMenu db " New Load Save Compile          ",0
strConfirm  db " Sure? Yes No                   ",0
strPlay     db " Playing, press any key to stop ",0
strCopy     db " Block length:                  ",0

strHelpMain db "CS+3,4,5,6,7,8: move cursor",1
            db "SS+Q: jump to start",1
            db "SS+E: jump to end",2
            db "SS+1,2,3,4: select octave",1
            db "SS+6: autostep",1
            db "SS+7: tempo",1
            db "SS+9: set track length",2
            db "Z,S,X,D,C,V,G,B,H,N,J,M: note",1
            db "Q,2,W,3,E,R,5,T,6,Y,7,U: note up",1
            db "A: key off",1
            db "CS+9: clear note",1
            db "CS+0: delete note",1
            db "SS+W: insert note",1
            db "CS+1: edit instrument",1
            db "CS+2: reset phase for ch1 note",2
            db "L: set loop start",1
            db "ENTER: hold to play tune",1
            db "K: hold to play row",1
            db "I: select drumset type",0
