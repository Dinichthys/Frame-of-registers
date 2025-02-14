.model tiny
.186
.code
org 100h
locals ll

HOT_KEY         equ 3Ah

VIDEOSEG        equ 0B800h

TOP_GAP         equ 0h
LEFT_GAP        equ 1h

FRAME_LENGTH    equ 12h
FRAME_HEIGHT    equ 0Ah

FRAME_COLOR     equ 0Fh

START_X         equ 3Ch
START_Y         equ 1h

TERMINAL_LEN    equ 50h

END_SYM         equ 0h

; -------MAIN---------

Main:
    mov ax, 0h
    mov es, ax
    mov bx, 08h*4

    cli                                 ; Start of changing

    mov ax, word ptr es:[bx]
    mov RealTimeCtrlOfs, ax
    mov ax, word ptr es:[bx+2]
    mov RealTimeCtrlSeg, ax
    mov es:[bx], offset TimeControl
    mov es:[bx+2], cs

    add bx, 4

    mov ax, word ptr es:[bx]
    mov RealKeyCtrlOfs, ax
    mov ax, word ptr es:[bx+2]
    mov RealKeyCtrlSeg, ax
    mov es:[bx], offset KeyControl
    mov es:[bx+2], cs

    sti                                 ; End of changing

    mov ax, 3100h
    mov dx, offset END_LABEL
    shr dx, 4
    inc dx
    int 21h

; --------------------

; --------KC----------

KeyControl proc

    push ax

    in al, 60h
    cmp al, HOT_KEY

    je llChange_activity
llIf:

    pop ax

    db 0eah
RealKeyCtrlOfs dw 0h
RealKeyCtrlSeg dw 0h

    iret

llChange_activity:
    xor cs:Activity, 0FFh
    jmp llIf

endp

; --------------------


; --------TC----------

TimeControl proc

    push ax

    mov al, cs:Activity
    cmp al, 0h
    pop ax

    je llRealTimeCtrl

    call MyTimeCtrl

llRealTimeCtrl:

    db 0eah
RealTimeCtrlOfs dw 0h
RealTimeCtrlSeg dw 0h

    iret

Activity db 0h

endp

; --------------------


; -------MY-TC--------

MyTimeCtrl proc

    call RegistersValue

    push ax bx cx dx es si di

    call MakeFrame

    pop di si es dx cx bx ax

    ret

endp

; --------------------


;---------------------------------
; It translates registers values
; to string
;
; Entry: REGISTERS
; Exit: STRINGS
; Destrs: None
;---------------------------------

RegistersValue  proc

    push ax di

    mov di, offset Str_ax + 05h         ; 5 = strlen ('ax = ')
    call ValToStr

    mov ax, bx
    mov di, offset Str_bx + 05h
    call ValToStr

    mov ax, cx
    mov di, offset Str_cx + 05h
    call ValToStr

    mov ax, dx
    mov di, offset Str_dx + 05h
    call ValToStr

    pop di ax

    ret

endp

; --------------------


; --------------------

Str_ax db 'ax = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_bx db 'bx = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_cx db 'cx = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_dx db 'dx = ', 0FFh, 0FFh, 0FFh, 0FFh, END_SYM

; --------------------


;---------------------------------
; It translates AX values
; to string pointed by DI
;
; Entry:  AX, DI
; Exit:   STRING
; Destrs: AX
;---------------------------------

ValToStr  proc

    push ax

    shr ah, 04h
    call DigitToStr

    inc di

    pop ax

    shl ah, 04h
    shr ah, 04h
    call DigitToStr

    inc di

    mov ah, al
    shr ah, 04h
    call DigitToStr

    inc di

    mov ah, al
    shl ah, 04h
    shr ah, 04h
    call DigitToStr

    sub di, 03h

    ret

endp

; --------------------


;---------------------------------
; It translates AH small 4 bits
; to byte pointed by DI
;
; Entry:  AX, DI
; Exit:   STRING
; Destrs: AH
;---------------------------------

DigitToStr  proc

    cmp ah, 09h
    ja llLetter

    add ah, '0'
    mov cs:[di], ah

    ret

llLetter:

    add ah, 'A' - 0Ah
    mov cs:[di], ah

    ret

endp

; --------------------


;---------------------------------
; Draw the frame
;
; Entry:  VIDEOSEG
; Exit:   None
; Destrs: AX, BX, CX, DX, ES, SI, DI
;---------------------------------

MakeFrame  proc

    call SetVideoseg

    mov al, FRAME_LENGTH
    mov ah, FRAME_HEIGHT

    mov cl, START_X
    mov ch, START_Y

    mov bx, offset FramePattern

    call DrawFrame

    mov cl, START_X
    mov ch, START_Y

    mov dx, offset Str_ax

    call PrintText

    ret

endp

; --------------------

; -------FRAME--------

;---------------------------------
; The function used VIDEOSEG
; and set it to es
;
; Entry:  VIDEOSEG
; Exit:   None
; Destrs: BX
;---------------------------------

SetVideoseg     proc

    mov bx, VIDEOSEG
    mov es, bx
    ret

endp

;--------------------------------

FramePattern db '/-\| |\-/'

;---------------------------------
; The function draw the frame
; with the text pointed by DX,
; base pointed by BX,
; size in AX (AL - x, AH - y)
; and coordinates in CX
; (CL - x, CH - y)
;
; Entry:  ES, AH, AL, BX, CX
; Exit:   None
; Destrs: AX, CX, DX
;---------------------------------

DrawFrame     proc

    add ah, ch

    mov cs:BotCor, ah
    mov ah, FRAME_COLOR

    call BaseLine
    inc ch

llCond:
    cmp cs:BotCor, ch
    je llEnd

llFor:
    call BaseLine
    inc ch
    sub bx, 03h

    jmp llCond

llEnd:
    add bx, 03h
    call BaseLine

    ret

endp

;--------------------------------

BotCor db 0h

;---------------------------------
; The function draw the line of the frame
; with coordinates in CX (CL - x, CH - y)
; and color in AH
;
; Entry:  ES, CH, CL, BX, AH
; Exit:   None
; Destrs: DX, BX (inc x3)
;---------------------------------

BaseLine     proc

    cld

    mov dl, ch
    mov dh, 0h
    imul dx, dx, TERMINAL_LEN

    push ax

    mov al, cl
    mov ah, 0h
    add dx, ax

    pop ax

    shl dx, 01h                     ; dx = (ch * 80 + cl) * 2

    mov di, dx
    add dx, FRAME_LENGTH * 2 - 2

    mov al, cs:[bx]
    stosw

    inc bx
    mov al, cs:[bx]

llCond:
    cmp di, dx
    je llFor_end

llFor:
    stosw

    jmp llCond

llFor_end:
    inc bx
    mov al, cs:[bx]

    stosw

    inc bx

    ret

endp

;--------------------------------

; -------TEXT---------

;---------------------------------
; Print the text pointed by DX
; to the frame with
; starting coordinates
; CL - x, CH - y
;
; Entry:  ES, DX, CH, CL
; Exit:   None
; Destrs: AX, BX, SI, DI
;---------------------------------

PrintText     proc

    cld

    mov bl, ch
    mov bh, 0h
    add bx, TOP_GAP + 1
    imul bx, bx, TERMINAL_LEN
    mov al, cl
    mov ah, 0h
    add bx, ax
    shl bx, 01h
    add bx, LEFT_GAP * 2 + 2            ; bx - index in memory of starting the string

    mov di, bx
    mov si, dx

    push ds
    mov ax, cs
    mov ds, ax
    mov ax, 0h
llCond:
    cmp byte ptr cs:[si], END_SYM
    je llEnd
    cmp byte ptr cs:[si], 0Ah          ; \n
    je llNewLine

llWhile:
    inc ax
    movsb
    inc di

    jmp llCond

llEnd:
    pop ds

    ret

llNewLine:
    inc si
    add di, TERMINAL_LEN * 2
    shl ax, 01h
    sub di, ax
    mov ax, 0h
    jmp llCond

endp

;--------------------------------

END_LABEL:

end Main
