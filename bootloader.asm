[bits 16]                   						; Indicate we are using 16-bit instructions
[org 0x7C00]                						; Set origin to 0x7C00


;***************************************************
;***************************************************	
;			   ------ Bootloader ------
;			   First sector (512 bytes)
;			   ------------------------
;***************************************************
;***************************************************


setup:
	mov ax, 0										; Clear Segment Registers
	mov ds, ax
	mov es, ax

	mov [drive_number], dl							; save drive number

start:												; program start
    
	mov si, boot_message
    call print_string								; display boot message

	mov si, sector_read_message						; display sector read message
	call print_string

	mov ax, 0
	mov es, ax
	mov al, 1
	mov ch, 0
	mov cl, 2
	mov dh, 0
	mov bx, 0x7E00
	call read_sector

	jmp Sector2


;***************************************************
;
;--------------------Functions----------------------
;
;***************************************************


print_string:										
; prints string indexed at DS:SI
; address of string			: 	SI
; uses:			DS, SI

	mov ah, 0x0E          							; BIOS teletype function
.next_char:
    lodsb                 							; load byte at ds:si into al and increment si
    test al, al           							; check if end of string (null terminator)
    jz .done              							; if zero, done
    int 0x10             							; call BIOS interrupt to print character
    jmp .next_char        							; repeat for next character
.done:
    ret


read_sector:
; reads sector*s onto memory
; number of sectors to read	:	AL 		cylinder number		:	CH 
; sector number				:	CL		head number			:	DH
; ram address to load into	: 	BX 		load address base	:	ES
; uses		:	AX, BX, CX, DX, ES			calls print_string at error
    mov ah, 0x02          							; BIOS function: read sectorsore data)
    mov dl, [drive_number]  						; Drive number (usually 0x00 for floppy)
    int 0x13             							; Call BIOS interrupt
	jc .read_error         							; Jump if carry flag is set (error)
	ret
.read_error:
	mov si, sector_error_message
    call print_string
	ret


;***************************************************
;
;--------------------Variables----------------------
;
;***************************************************

boot_message db 'Booting...', 13, 10, 0   													; message for boot
sector_read_message db 'Reading sector...', 13, 10, 0										; message for reading sector
sector_error_message db 'Error reading sector!', 13, 10, 0									; error message for read_sector
drive_number db 0																			; spot to save DL, drive number

;***************************************************
;----------------------End--------------------------
;***************************************************

times 510 - ($ - $$) db 0   						; fill with zeros until 510 bytes
dw 0xAA55                   						; boot signature (must be present)

;***************************************************
;***************************************************	
;			------ End of Bootloader ------
;			End of First Sector (512 bytes)
;			-------------------------------
;***************************************************
;***************************************************
;				---- Second Sector ------
;				Second Sector (512 bytes)
;				-------------------------
;***************************************************
;***************************************************


Sector2:											; start of second sector

	mov si, sector_read_message						; display sector read message
	call print_string

	mov ax, 0
	mov es, ax
	mov al, 1
	mov ch, 0
	mov cl, 3
	mov dh, 0
	mov bx, 0x8000
	call read_sector

	jmp Sector3


;***************************************************
;
;--------------------Functions----------------------
;
;***************************************************


writePixelXY:           
; draw pixel at (x, y)
; x-Coordinate				:	AX 		y-Coordinate		:	DH 
; colour					:	DL 
; uses		:	AX, DX, ES, DI
; coordinate mapping into memory address => (Y * 320) + X = DI


    mov di, ax          							; buffer to hold x-coordinate
    xor ax, ax          							; delet ax

    mov al, 5           							; 5
    mul dh              							; al(5) * dh(y-coord)
    shl ax, 6          								; ax * 64 -> dh(y-coord) * 320
    add ax, di          							; add x-coord to y*320
    mov di, ax          							; move offset into di

    mov ax, 0xA000      							; base address: starting address of video memory is 0xA0000, the segment is 0xA000
    mov es, ax          							; since es is segment register, es = 0xA000

    mov byte [es:di], dl							; write pixel

    ret


writePixelOffset:
; draw pixel at 0xA000:offset(AX)
; address offset			:	AX 		colour				: 	DL 
; uses		:     AX, DX, ES, DI

    
    mov di, ax										; offset(ax) into di
    mov ax, 0xA000									; base address into ax
    mov es, ax										; base address(ax) into es

    mov byte [es:di], dl							; write pixel

    ret


writePixelNonVolatile:
; draw pixel at (x, y), without destroying x, y, or colour
; x-coordinate				:	AX 		y-coordinate		:	BX 
; colour					:	DX 
; uses		:	AX, BX, DX, ES, DI
; coordinate mapping into memory address => (Y * 320) + X = DI


    push ax             							; push to save value
    push bx             							; push to save value

    mov di, ax          							; buffer to hold x-coordinate
    xor ax, ax          							; delet ax

    mov al, 5           							; 5
    mul bl              							; al(5) * bl(y-coord)
    shl ax, 6          								; ax * 64 -> (dl(y-coord) * 5)*64
    add ax, di          							; add x-coord to y*320
    mov di, ax          							; move offset into di

    mov ax, 0xA000      							; base address: starting address of video memory is 0xA0000, the segment is 0xA000
    mov es, ax          							; since es is segment register, es = 0xA000

    mov byte [es:di], dl							; write pixel

    pop bx              							; retrieve value
    pop ax              							; retrieve value
    
    ret


displayPalette:
; displays current palette as columns
; uses		:	AX, BX, DX, ES, DI
	
	
	mov ax, 0
	mov dx, 0
.loopColumn:
	mov bx, 0
.loopRow:
	call writePixelNonVolatile
	inc bx
	cmp bx, 199
	jle .loopRow
	inc dx
	inc ax
	cmp ax, 256
	je .end
	jmp .loopColumn
.end:
	ret


drawSquare:
; draws square, left corner at (x, y), keeps ax, bx, and dx
; x-coordinate				:	AX		y-coordinate		:	BX
; side_length				:	CX		colour				:	DX
; uses		:	AX, BX, CX, DX, ES, DI

	push cx
	push bx
	push ax
	dec cx
	mov si, cx
	add cx, bx
	add si, ax
.verticalLoop:
	pop ax
	push ax
.horizontalLoop:
	call writePixelNonVolatile
	inc ax
	cmp ax, si
	jle .horizontalLoop
	inc bx
	cmp bx, cx
	jg	.end
	jmp .verticalLoop
.end:
	pop ax
	pop bx
	pop cx
	ret


displayPaletteSqr:
; displays palette as squares
; uses		:	AX, BX, CX, DX, ES, DI


	mov ax, 20
	mov bx, 20
	mov cx, 10
	mov dx, 0
	call drawSquare
.loopPalette:
	add ax, 10
	inc dx
	call drawSquare
	cmp ax, 170
	jl .loopPalette
	add bx, 10
	mov ax, 10
	cmp bx, 180
	jl .loopPalette
	ret


set_col_at_index:
; change palette colours prior to image display
; palette index				:	BX		red value(0-63)		:	DH
; green value(0-63)			:	CH		blue value(0-63)	:	CL
; uses		:	AX, BX, DH, CX


	mov ax, 1010h
	int 10h
	ret


change_palette:
; change palette colours according to an rgb-bit string at address AX
; rgb bits start addr:	SI
; uses 		:	AX, BX, CX, DX, SI


	mov si, 0x9E00
	mov cx, 31
.loopStart:
	inc cx				; Pixel K
	mov dx, 0x3C8
	mov ax, cx
	out dx, al			; index 0
	mov bx, [si]		; at SI: RRRRRRGG|GGGGBBBB
						; in BX: GGGGBBBB|RRRRRRGG
	mov dx, 0x3C9
	ror bx, 2
	mov ax, bx
	out dx, al

	rol bx, 6
	mov ax, bx
	out dx, al

	inc si				; index 1
	mov bx, [si]		; at SI: GGGGBBBB|BBRRRRRR
						; in BX: BBRRRRRR|GGGGBBBB
	rol bx, 2
	mov ax, bx
	out dx, al

	inc cx				; Pixel K+1
	mov dx, 0x3C8
	mov ax, cx
	out dx, al
	rol bx, 6
	mov dx, 0x3C9
	mov ax, bx
	out dx, al

	inc si				; index 2
	inc si				; index 3
	mov bx, [si]		; at SI: GGGGGGBB|BBBBRRRR	
						; in BX: BBBBRRRR|GGGGGGBB
	ror bx, 2
	mov ax, bx
	out dx, al

	rol bx, 6
	mov ax, bx
	out dx, al

	inc cx				; Pixel K+2
	mov dx, 0x3C8
	mov ax, cx
	out dx, al
	inc si				; index 4
	mov bx, [si]		; at SI: BBBBRRRR|RRGGGGGG
						; in BX: RRGGGGGG|BBBBRRRR
	rol bx, 2
	mov dx, 0x3C9
	mov ax, bx
	out dx, al

	rol bx, 6
	mov ax, bx
	out dx, al
	
	inc si				; index 5
	inc si				; index 6
	mov bx, [si]		; at SI: BBBBBBRR|RRRRGGGG
						; in BX: RRRRGGGG|BBBBBBRR
	ror bx, 2
	mov ax, bx
	out dx, al

	inc cx				; Pixel K+3
	mov dx, 0x3C8
	mov ax, cx
	out dx, al
	mov dx, 0x3C9	
	rol bx, 6
	mov ax, bx
	out dx, al

	inc si				; index 7
	mov bx, [si]		; at SI: RRRRGGGG|GGBBBBBB
						; in BX: GGBBBBBB|RRRRGGGG
	rol bx, 2
	mov ax, bx
	out dx, al

	rol bx, 6
	mov ax, bx
	out dx, al

	inc si				; index 8
	inc si				; index 9(start of next 4-pixel-group)

	cmp cx, 255
	jl .loopStart
.end:
	ret


set_default_palette:
; sets the default palette
; uses		:	AX, CX, DX, SI


		mov si, defaultPalette
		mov cx, 0
.loopPalette:
		mov dx, 0x3C8
		mov ax, cx
		out dx, al
		mov dx, 0x3C9
		mov al, [si]
		out dx, al
		inc si
		mov al, [si]
		out dx, al
		inc si
		mov al, [si]
		out dx, al
		inc si
		inc cx
		cmp cx, 32
		jl .loopPalette
		ret


;***************************************************
;
;--------------------Variables----------------------
;
;***************************************************

defaultPalette:

db 0, 0, 0,     	0, 0, 63,   	0, 63, 0,   	63, 0, 0
db 63, 63, 0,   	63, 0, 63,  	0, 63, 63,  	63, 63, 63
db 0, 42, 63,   	21, 42, 63, 	42, 42, 63, 	0, 21, 63
db 21, 0, 63,		21, 21, 63, 	42, 21, 63, 	42, 0, 63
db 0, 63, 42,   	21, 63, 42, 	42, 63, 42, 	0, 63, 21
db 21, 63, 0,   	21, 63, 21, 	42, 63, 21, 	42, 63, 0
db 63, 0, 42,   	63, 21, 42, 	63, 42, 42, 	63, 0, 21
db 63, 21, 0,   	63, 21, 21, 	63, 42, 21, 	63, 42, 0

;***************************************************
;----------------------End--------------------------
;***************************************************

times 1024 - ($ - $$) db 0							; fill with 0 till next sector

;***************************************************
;***************************************************	
;			----- End of Second Sector -----
;			End of Second Sector (1024 bytes)
;			--------------------------------
;***************************************************
;***************************************************
;			----- Start of Third Sector ------
;			Start of Third Sector (1024 bytes)
;			----------------------------------
;***************************************************
;***************************************************


Sector3:											; start of third sector

	mov ah, 0x00    								; Function 0: Set Video Mode
    mov al, 0x13    								; Mode 13h: 320x200 pixels, 256 colors
    int 0x10        								; Call BIOS interrupt

	call set_default_palette
	
	mov ax, 0
	mov es, ax
	mov al, 2										; Doesn't fit into 1 sector
	mov ch, 0
	mov cl, 4
	mov dh, 0
	mov bx, 0x8200
	call read_sector

	jmp Sector4


;***************************************************
;
;--------------------Functions----------------------
;
;***************************************************


displayImage:
; displays image at head_number(n, n+1) dh = 1 for 1. picture, dh = 3 for 2nd, dh = 5 for 3rd etc.
; head number				:	DH
; uses		:	AX, BX, CX, DX, ES, DI


	xor dl, dl
	push dx

	xor ax, ax
	mov es, ax
	mov al, 1
	mov ch, 0
	mov cl, 1
	pop dx
	push dx
	mov bx, 0x9E00

	call read_sector
	call change_palette

	mov ax, 0xA000
	mov es, ax

	mov al, 62
	mov ch, 0
	mov cl, 2
	pop dx
	push dx
	mov bx, 0x0000

	call read_sector

	mov al, 63
	mov ch, 0
	mov cl, 1
	pop dx
	inc dh
	mov bx, 31744

	call read_sector

	xor ax, ax
	mov es, ax

	ret


setupSpeaker:
; sets speaker up to use PIT and turns it on
; uses		:	AX, DX


	mov dx, 0x43
	mov al, 0xB6									; PIT CHIP RUNS AT 1.193182 MHz
	out dx, al

	mov dx, 0x61
	in 	al, dx		; read status registers
	or 	al, 0x1		; set bit 0 to 1 to connect speaker to PIT channel 2's output, without changing other registers
	out dx, al
	
	ret


disconnectSpeaker:
; detaches speaker from PIT
; uses		:	AX, DX


	mov dx, 0x61
	in 	al, dx		; read status registers
	and	al, 0xFE	; set bit 0 to 0 to disconnect speaker from PIT channel 2's output, without changing other registers
	out dx, al

	ret


speakerOff:
; turns speaker off
; uses		:	AX, DX


	mov dx, 0x61	; setup speaker to use pit channel 2
	in 	al, dx		; read status registers
	and al, 0xFD	; set bit 1 to 0 without changing other bits - bit 1 > turn the speaker on/off
	out dx, al		; write the change back to port 0x61
	ret


speakerOn:
; turns speaker on
; uses		:	AX,	DX


	mov dx, 0x61	; setup speaker to use pit channel 2
	in 	al, dx		; read status registers
	or 	al, 0x2		; set bit 1 to 1 without changing other bits - bit 1 > turn the speaker on/off
	out dx, al		; write the change back to port 0x61
	ret


changeNote:
; changes note of speaker
; frequency of note			:	AX
; uses		:	AX, DX


	and ax, 0xFFFE
	mov dx, 0x42
	out dx, al
	shr ax, 8
	out dx, al
	ret


;***************************************************
;
;--------------------Variables----------------------
;
;***************************************************



;***************************************************
;----------------------End--------------------------
;***************************************************

times 1536 - ($ - $$) db 0							; fill with 0 till next sector

;***************************************************
;***************************************************	
;			----- End of Third Sector ------
;			End of Third Sector (1536 bytes)
;			--------------------------------
;***************************************************
;***************************************************
;		----- Start of Fourth & Fifth Sector -----
;		Start of Fourth & Fifth Sector (1536 bytes)
;		------------------------------------------
;***************************************************
;***************************************************
;----------------------Macros-----------------------
;***************************************************
;***************************************************


%assign 	beat		75
%assign		beatSep		beat * 3 / 10
%assign		pit_freq	1193180

%macro PlayTone 2									; %1 frequency, %2 duration in ms
	mov ax, pit_freq / %1
	call changeNote
	call speakerOn

	mov cx, (%2 * 1000) >> 16
	mov dx, (%2 * 1000) & 0xFFFF
	mov ah, 0x86
	int 15h

	call speakerOff
%endmacro

%macro PlayRest 1									; %1 duration in ms
	mov cx, (%1 * 1000) >> 16
	mov dx, (%1 * 1000) & 0xFFFF
	mov ah, 0x86
	int 15h
%endmacro


;***************************************************
;------------------End Of Macros--------------------
;***************************************************
;------------------Instructions---------------------
;***************************************************


Sector4:											; start of fourth sector

	call setupSpeaker

	PlayTone 120, 1 * beat
	PlayRest 3 * beatSep

	PlayTone 120, 1 * beat
	PlayRest 3 * beatSep

	PlayTone 100, 1 * beat
	PlayRest 2 * beatSep

	PlayTone 100, 1 * beat
	PlayRest 2 * beatSep

	PlayTone 100, 1 * beat
	PlayRest 1 * beatSep

	PlayTone 85, 1 * beat
	PlayRest 1 * beatSep

	PlayTone 85, 1 * beat
	PlayRest 1 * beatSep

	PlayTone 85, 1 * beat
	PlayRest 6 * beatSep

	PlayTone 554, 6 * beat
	PlayRest 6 * beatSep

	PlayTone 622, 10 * beat
	PlayRest 10 * beatSep
	
	PlayTone 622, 6 * beat
	PlayRest 6 * beatSep
	
	PlayTone 698, 6 * beat
	PlayRest 6 * beatSep
	
	PlayTone 831, 1 * beat
	PlayRest 1 * beatSep
	
	PlayTone 740, 1 * beat
	PlayRest 1 * beatSep
	
	PlayTone 698, 1 * beat
	PlayRest 1 * beatSep
	
	PlayTone 622, 1 * beat
	PlayRest 1 * beatSep
	
	PlayTone 554, 6 * beat
	PlayRest 6 * beatSep
	
	PlayTone 622, 10 * beat
	PlayRest 10 * beatSep

	PlayRest 2 * beat
	PlayRest 2 * beatSep
	
	PlayTone 415, 12 * beat
	PlayRest 12 * beatSep

	call speakerOff

	mov dh, 1
	
	call displayImage

	mov cx, 0x004C
	mov dx, 0x4B40
	mov ah, 0x86
	int 15h

	mov dh, 5
	call displayImage

	PlayTone 147, 1200 / 24
	PlayRest 1200 / 16 * 45 / 100

	PlayTone 147, 1200 / 24
	PlayRest 1200 / 16 * 45 / 100

	PlayTone 294, 1200 / 12
	PlayRest 1200 / 8 * 45 / 100

	PlayTone 220, 1200 / 10
	PlayRest 1200 / 6 * 45 / 100

	PlayRest 1000

	mov dh, 3
	call displayImage

	jmp $


;***************************************************
;
;--------------------Functions----------------------
;
;***************************************************



;***************************************************
;
;--------------------Variables----------------------
;
;***************************************************



;***************************************************
;----------------------End--------------------------
;***************************************************

times 2560 - ($ - $$) db 0							; fill with 0 till next sector

;***************************************************
;***************************************************	
;		----- End of Fourth & Fifth Sector ------
;		End of Fourth & Fifth Sector (2560 bytes)
;		-----------------------------------------
;***************************************************
;***************************************************
times 32256 - ($ - $$) db 0							; fill so that it is appropriately adjusted
incbin "image.bin"