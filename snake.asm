; Snake game for DOS and VGA by Valerii Mochychuk
;
; nasm -o snake.com snake.asm


ncols equ 40
nrows equ 24
bios equ 0x0040
video equ 0xa000

%define coord(row, col) row<<8|col


	org 0x0100

	mov ax, 0x0013  ; grapthics mode 320x200 8bit color
	int 0x10
	mov [stack], sp
restart:
	mov sp, [stack]
	call menu
	call newboard
	call newsnake
	call newfood
game:
	call delay
	call handlekey
	call step
	jmp game


menu:
	call cls
	mov si, gamename
	mov bl, 4
	mov dx, coord(12,17)
	call putstr
	mov si, anykey
	mov bl, 5
	mov dx, coord(23,13)
	call putstr
	call waitkey
	ret


step:
	mov al, [snakedir]
	mov dx, [snakehead]
	call putcell
	call getnextcoord
	call getcell
	cmp al, emptycell
	jz .head

	cmp al, foodcell
	jnz accident

	call newfood
	add byte[grow], 5
.head:
	mov [snakehead], dx
	mov al, [snakedir]
	call putcell
	cmp byte[grow], 0
	jz .tail

	dec byte[grow]
	inc word[snakelen]
	call putlen
	ret
.tail:
	mov dx, [snaketail]
	push dx
	call getcell
	call getnextcoord
	mov [snaketail], dx
	pop dx
	mov al, emptycell
	call putcell
	ret


; input: DH = board row, DL = board column
accident:
	mov al, badcell
	call putcell
	mov si, gameover
	mov bl, 7
	mov dx, coord(12,15)
	call putstr
	call waitkey
	jmp restart


status:
	mov si, statusbar
	mov bl, 6
	mov dx, coord(0,16)
	call putstr
putlen:
	mov ax, [snakelen]
	call numstr
	mov bl, 7
	mov dx, coord(0,21)
	call putstr
	ret


newboard:
	; fill all the board with wallcell
	mov al, wallcell
	mov cx, nrows*ncols
	mov di, board
	rep stosb

	; fill all but border with emptycell
	mov al, emptycell
	mov dl, nrows-2
	mov di, board+ncols+1
.line:
	mov cx, ncols-2
	rep stosb
	add di, 2
	dec dl
	jnz .line

	; render board
	mov dh, 0
	mov si, board
.row:
	mov dl, 0
.col:
	lodsb
	call rendercell
	inc dl
	cmp dl, ncols
	jc .col

	inc dh
	cmp dh, nrows
	jc .row

	ret


newsnake:
	mov byte[grow], 0
	mov byte[snakedir], moveleft
	mov word[snaketail], coord(11,21)
	mov al, moveleft
	mov cx, 5
	mov dx, coord(11,17)
	mov [snakelen], cx
	mov [snakehead], dx
.show:
	call putcell
	inc dl
	loop .show

	call status
	ret


newfood:
	push dx
.rnd:
	call getrnd
	and ax, 0x03ff
	cmp ax, (nrows-2)*(ncols-2)
	jc .test

	shr ax, 1
.test:
	add ax, ncols+1
	call getposcoord
	call getcell
	cmp al, emptycell
	jnz .rnd
	
	mov al, foodcell
	call putcell
	pop dx
	ret


; input: AX = board index
; output: DH = board row, DL = board column
getposcoord:
	push ax
	mov dl, ncols
	div dl
	mov dh, al
	mov dl, ah
	pop ax
	ret


; output: AX = random number
getrnd:
	push dx
	push ds
	mov ax, bios
	mov ds, ax
	mov ax, 0x0dcd  ; magic
	mul word[0x006c]  ; timer
	pop ds
	pop dx
	ret


; input: DH = board row, DL = board column
; output: DI = cell address
getcelladdr:
	push ax
	push dx
	mov al, ncols
	mul dh
	mov dh, 0
	mov di, ax
	add di, dx
	add di, board
	pop dx
	pop ax
	ret


; input: DH = board row, DL = board column
; output: AL = cell value
getcell:
	call getcelladdr
	mov al, [di]
	ret


; input: AL = cell value, DH = board row, DL = board column
putcell:
	call getcelladdr
	mov [di], al
	call rendercell
	ret


; input: AL = cell value DX = board coordinates
rendercell:
	push ax
	push cx
	push si
	push di
	push es

	; SI = sprite data address for cell value
	mov ah, 0
	shl ax, 1
	add ax, sprites
	mov si, ax
	mov si, [si]

	; DI = video offset for cell
	mov ax, 320*8
	mov cl, dh
	inc cl
	mov ch, 0
	push dx
	mul cx
	pop dx
	mov cl, dl
	shl cx, 3
	add ax, cx
	mov di, ax

	; copy sprite data to video memory
	mov ax, video
	mov es, ax
	mov ax, 8
.line:
	mov cx, 8
	rep movsb
	add di, 312
	dec ax
	jnz .line

	pop es
	pop di
	pop si
	pop cx
	pop ax
	ret


; input: BL = color, DH = screen row, DL = screen column, SI = 0-terminated string
putstr:
	push ax
	push bx
	mov ah, 2  ; move cursor
	mov bh, 0  ; screen page
	int 0x10
	mov ah, 14  ; put char
.char:
	lodsb
	or al, al
	jz .eol

	int 0x10
	jmp .char
.eol:
	pop bx
	pop ax
	ret
	

; input: AX = number
; output: SI = string address
numstr:
	push ax
	push cx
	push dx
	mov cx, 10
	mov si, .str+5
.digit:
	dec si
	xor dx, dx
	div cx
	add dl, "0"
	mov [si], dl
	or ax, ax
	jnz .digit

	pop dx
	pop cx
	pop ax
	ret
.str:
	db "65535", 0


; input: AL = value
cls:
	push es
	mov ax, video
	mov es, ax
	xor ax, ax
	mov cx, 320*200/2
	xor di, di
	rep stosw
	pop es
	ret


; output: AH = scan code, AL = char
waitkey:
	mov ah, 0
	int 0x16
	ret


; output: AH = scan code, AL = char
getkey:
	mov ah, 1
	int 0x16
	jz .nokey
	
	call waitkey
	call clearkbd
	ret
.nokey:
	xor ax, ax	
	ret


delay:
	mov bx, 0x9000
.1:
	mov cx, 16
	loop $
	dec bx
	jnz .1

	ret


clearkbd:
	push ax
	push ds
	mov ax, bios
	mov ds, ax
	mov ax, [0x001a]  ; keyboard buffer head
	mov [0x001c], ax  ; keyboard buffer tail
	pop ds
	pop ax
	ret


handlekey:
	call getkey
	cmp ah, 0x4b  ; cursor left
	jz .left
	cmp ah, 0x4d  ; cursor right
	jz .right
	cmp ah, 0x48  ; cursor up
	jz .up
	cmp ah, 0x50  ; cursor down
	jz .down
	cmp ah, 1  ; Esc
	jz .esc
	ret
.left:
	mov byte[snakedir], moveleft
	ret
.right:
	mov byte[snakedir], moveright
	ret
.up:
	mov byte[snakedir], moveup
	ret
.down:
	mov byte[snakedir], movedown
	ret
.esc:
	int 0x20


; input: AL = direction, DH = board row, DL = board column
; output:  DH = new board row, DL = new board column
getnextcoord:
	cmp al, moveleft
	jz .left
	cmp al, moveright
	jz .right
	cmp al, moveup
	jz .up
	cmp al, movedown
	jz .down
	ret
.left:
	dec dl
	ret
.right:
	inc dl
	ret
.up:
	dec dh
	ret
.down:
	inc dh
	ret


grow: db 0
snakedir: db 0
snakelen: dw 0
snakehead: dw 0
snaketail: dw 0
stack: dw 0

anykey: db "press any key", 0
gamename: db "SNAKE", 0
gameover: db "GAME OVER", 0
statusbar: db "SIZE", 0

emptysprite:
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0
	db 0, 0, 0, 0, 0, 0, 0, 0

wallsprite:
	db 1, 1, 1, 1, 1, 1, 1, 0
	db 1, 1, 1, 1, 1, 1, 1, 0
	db 1, 1, 1, 1, 1, 1, 1, 0
	db 1, 1, 1, 1, 1, 1, 1, 0
	db 1, 1, 1, 1, 1, 1, 1, 0
	db 1, 1, 1, 1, 1, 1, 1, 0
	db 1, 1, 1, 1, 1, 1, 1, 0
	db 0, 0, 0, 0, 0, 0, 0, 0

foodsprite:
	db 5, 5, 5, 5, 5, 5, 5, 0
	db 5, 5, 5, 5, 5, 5, 5, 0
	db 5, 5, 5, 5, 5, 5, 5, 0
	db 5, 5, 5, 5, 5, 5, 5, 0
	db 5, 5, 5, 5, 5, 5, 5, 0
	db 5, 5, 5, 5, 5, 5, 5, 0
	db 5, 5, 5, 5, 5, 5, 5, 0
	db 0, 0, 0, 0, 0, 0, 0, 0

badsprite:
	db 4, 4, 4, 4, 4, 4, 4, 0
	db 4, 4, 4, 4, 4, 4, 4, 0
	db 4, 4, 4, 4, 4, 4, 4, 0
	db 4, 4, 4, 4, 4, 4, 4, 0
	db 4, 4, 4, 4, 4, 4, 4, 0
	db 4, 4, 4, 4, 4, 4, 4, 0
	db 4, 4, 4, 4, 4, 4, 4, 0
	db 0, 0, 0, 0, 0, 0, 0, 0

snakesprite:
	db 2, 2, 2, 2, 2, 2, 2, 0
	db 2, 2, 2, 2, 2, 2, 2, 0
	db 2, 2, 2, 2, 2, 2, 2, 0
	db 2, 2, 2, 2, 2, 2, 2, 0
	db 2, 2, 2, 2, 2, 2, 2, 0
	db 2, 2, 2, 2, 2, 2, 2, 0
	db 2, 2, 2, 2, 2, 2, 2, 0
	db 0, 0, 0, 0, 0, 0, 0, 0

emptycell equ 0
wallcell  equ 1
foodcell  equ 2
badcell   equ 3
moveleft  equ 4
moveright equ 5
moveup    equ 6
movedown  equ 7

sprites:
	dw emptysprite  ; 0
	dw wallsprite   ; 1
	dw foodsprite   ; 2
	dw badsprite    ; 3
	dw snakesprite  ; 4
	dw snakesprite  ; 5
	dw snakesprite  ; 6
	dw snakesprite  ; 7

board:
