.model tiny
.186
.code
org 100h
locals ll

VIDEOSEG        equ 0B800h

TOP_GAP         equ 0h
LEFT_GAP        equ 1h

FRAME_LENGTH    equ 0Dh
FRAME_HEIGHT    equ 0Eh

FRAME_COLOR     equ 0Fh

TERMINAL_LEN    equ 50h
TERMINAL_HEIGHT equ 19h

START_X_COOR    equ 41h
START_Y_COOR    equ 1h

END_SYM         equ 0h

ESC_SCAN_CODE   equ 1h
F4_SCAN_CODE    equ 3Eh
SHIFT_PRESS     equ 2Ah
SHIFT_RELEASE   equ 0AAh

MOVE_UP         equ 11h
MOVE_LEFT       equ 1Eh
MOVE_RIGHT      equ 20h
MOVE_DOWN       equ 1Fh

MOVE_PAGE_UP    equ 48h
MOVE_PAGE_LEFT  equ 75h
MOVE_PAGE_RIGHT equ 77h
MOVE_PAGE_DOWN  equ 80h

; -------MAIN---------

Main:
    mov ax, 0h
    mov es, ax
    mov bx, 08h*4

    cli                                 ; Start of changing

    mov ax, word ptr es:[bx]
    mov RealTimeCtrlOfs, ax
    mov ax, word ptr es:[bx+2]
    mov RealTimeCtrlSeg, ax             ; Save the address of previous Time Controller
    mov es:[bx], offset TimeControl
    mov es:[bx+2], cs                   ; Load the address of my Time Controller

    add bx, 4

    mov ax, word ptr es:[bx]
    mov RealKeyCtrlOfs, ax
    mov ax, word ptr es:[bx+2]
    mov RealKeyCtrlSeg, ax              ; Save the address of previous Key Controller
    mov es:[bx], offset KeyControl
    mov es:[bx+2], cs                   ; Load the address of my Key Controller

    sti                                 ; End of changing

    mov ax, 3100h
    mov dx, TERMINAL_LEN * TERMINAL_HEIGHT * 2 + offset END_LABEL
    shr dx, 4
    inc dx
    int 21h                             ; Stop and stay resident

; --------------------

; --------KC----------

KeyControl proc

    push ax bx cx

    in al, 60h

    call CheckOpenFile                  ; Load the screen if file was opened or closed

    cmp cs:Shift_pressed, 0h
    je llDontMove                       ; Move only if shift is pressed

    call CheckMovement
    cmp cs:MOVED, 0h
    jne llSkip                          ; If the frame was moved don't show the SCAN CODE to Volkov Commander

llDontMove:

    call CheckPageMovement              ; Draw the screen if the page was moved

    call CheckShift                     ; Check if shift was pressed

    call CheckHotKey                    ; Start or stop working with frame

    pop cx bx ax

    db 0eah                             ; Jump to real Key Controller
RealKeyCtrlOfs dw 0h
RealKeyCtrlSeg dw 0h

llSkip:
    mov cs:MOVED, 0h

    in al, 61h                          ; Show that SCAN CODE was analyzed
    mov ah, al
    or al, 80h
    out 61h, al
    mov al, ah
    out 61h, al

    mov al, 20h
    out 20h, al                         ; Show that interrupt was done

    pop cx bx ax

    iret

endp

; --------------------


;---------------------------------
; Check if file is opened
;
; Entry:  AL
; Exit:   CS:First_print
; Destrs: BX
;---------------------------------

CheckOpenFile proc

    cmp al, ESC_SCAN_CODE
    je llNewBackGround                      ; Check if ESC was pressed

    cmp al, F4_SCAN_CODE
    je llNewBackGround                      ; Check if F4 was pressed

llDone:
    ret

llNewBackGround:                            ; Show that the frame isn't on the screen
    mov cs:First_print, 0FFh
    jmp llDone

endp

; --------------------


;---------------------------------
; Check if hot key is pressed
;
; Entry:  AL
; Exit:   CS:Activity, Letters activity
; Destrs: BX, CX
;---------------------------------

CheckHotKey proc

    mov bx, offset Letter_D

    cmp al, cs:[bx]                         ; Check if letter D was pressed
    je llChange_letter_activity

    add bx, 02h
    cmp al, cs:[bx]                         ; Check if letter E was pressed
    je llChange_letter_activity

    add bx, 02h
    cmp al, cs:[bx]                         ; Check if letter N was pressed
    je llChange_letter_activity

    jmp llDestroy_letter_activity           ; Another key was pressed

llDone:

    ret

llChange_letter_activity:
    inc bx
    or byte ptr cs:[bx], 0FFh

    mov bx, offset Letter_D
    inc bx
    mov cx, 0h
    add cl, cs:[bx]

    add bx, 02h
    add cl, cs:[bx]

    add bx, 02h
    add cl, cs:[bx]

    cmp cl, 0FFh - 02h                      ; Check if sum of values of pressed keys is correct
    jne llDone

llChange_activity:
    xor cs:Activity, 0FFh                   ; Change activity

    cmp cs:Activity, 0h
    je llStart_frame_pos                    ; If the frame was stopped turn it to the start coordinates and draw the back of the frame

    jmp llDone

llStart_frame_pos:
    mov cs:MOVED, 05h                       ; Draw the background
    call StorageBackground
    mov cs:MOVED, 0h

    mov cs:First_print, 0FFh                ; Show that the frame isn't on the screen

    mov cs:START_X, START_X_COOR            ; Move it to the starting coordinates
    mov cs:START_Y, START_Y_COOR

    jmp llDone

llDestroy_letter_activity:
    mov bx, offset Letter_D
    inc bx

    mov byte ptr cs:[bx], 0h

    add bx, 02h
    mov byte ptr cs:[bx], 0h

    add bx, 02h
    mov byte ptr cs:[bx], 0h

    jmp llDone

endp

; --------------------

Letter_D db 20h, 0h
Letter_E db 12h, 0h
Letter_N db 31h, 0h

First_print db 0FFh

; --------------------


;---------------------------------
; Check if shift was pressed
;
; Entry:  AL
; Exit:   CS:First_print
; Destrs: BX
;---------------------------------

CheckShift proc

    cmp al, SHIFT_PRESS
    je llPressed

    cmp al, SHIFT_RELEASE
    je llReleased

llDone:
    ret

llPressed:
    mov cs:Shift_pressed, 0FFh
    jmp llDone

llReleased:
    mov cs:Shift_pressed, 0h
    jmp llDone

endp

; --------------------


;---------------------------------
; Check if page move's keys are pressed
;
; Entry:  AL
; Exit:   CS:Activity, Letters activity
; Destrs: None
;---------------------------------

CheckPageMovement proc

    cmp cs:Activity, 0FFh
    jne llDone

    cmp al, MOVE_PAGE_UP
    je llUp

    cmp al, MOVE_PAGE_LEFT
    je llLeft

    cmp al, MOVE_PAGE_RIGHT
    je llRight

    cmp al, MOVE_PAGE_DOWN
    je llDown

llDone:

    ret

llUp:
    cmp cs:START_Y, TERMINAL_HEIGHT - FRAME_HEIGHT - 1          ; Check if the frame on the bottom of the screen
    je llDone

    mov cs:MOVED, 04h                                           ; The frame was moved to the upside
    call StorageBackground

    jmp llDone

llLeft:
    cmp cs:START_X, TERMINAL_LEN - FRAME_LENGTH                 ; Check if the frame at right border of the screen
    je llDone

    mov cs:MOVED, 03h                                           ; The frame was moved to the left side
    call StorageBackground

    jmp llDone

llRight:
    cmp cs:START_X, 0h                                          ; Check if the frame at the left border of the screen
    je llDone

    mov cs:MOVED, 02h                                           ; The frame was moved to the right side
    call StorageBackground

    jmp llDone

llDown:
    cmp cs:START_Y, 0h                                          ; Check if the frame on the top of the screen
    je llDone

    mov cs:MOVED, 01h                                           ; The frame was moved to the downside
    call StorageBackground

    jmp llDone

endp

; --------------------


;---------------------------------
; Check if move's keys are pressed
;
; Entry:  AL
; Exit:   CS:Activity, Letters activity
; Destrs: None
;---------------------------------

CheckMovement proc

    cmp cs:Activity, 0FFh                                       ; If the frame is inactive don't move it
    jne llDone

    cmp al, MOVE_UP
    je llUp

    cmp al, MOVE_LEFT
    je llLeft

    cmp al, MOVE_RIGHT
    je llRight

    cmp al, MOVE_DOWN
    je llDown

llDone:

    ret

llUp:
    cmp cs:START_Y, 0h                                      ; Check if the frame on the top of the screen
    je llDone

    mov cs:MOVED, 01h                                       ; The frame was moved to the downside
    call StorageBackground

    dec cs:START_Y
    jmp llDone

llLeft:
    cmp cs:START_X, 0h                                      ; Check if the frame at the left border of the screen
    je llDone

    mov cs:MOVED, 02h                                       ; The frame was moved to the right side
    call StorageBackground

    dec cs:START_X
    jmp llDone

llRight:
    cmp cs:START_X, TERMINAL_LEN - FRAME_LENGTH             ; Check if the frame at right border of the screen
    je llDone

    mov cs:MOVED, 03h                                       ; The frame was moved to the left side
    call StorageBackground

    inc cs:START_X
    jmp llDone

llDown:
    cmp cs:START_Y, TERMINAL_HEIGHT - FRAME_HEIGHT - 1      ; Check if the frame on the bottom of the screen
    je llDone

    mov cs:MOVED, 04h                                       ; The frame was moved to the left side
    call StorageBackground

    inc cs:START_Y
    jmp llDone

endp

; --------------------

Shift_pressed db 0h

MOVED db 0h

; --------------------


; --------TC----------

TimeControl proc

    cmp cs:Activity, 0h                                     ; If the frame is inactive don't do anything
    je llRealTimeCtrl

llComplete:

    call MyTimeCtrl                                         ; My Time Controller

llRealTimeCtrl:

    db 0eah                                                 ; Jump to real Time Controller
RealTimeCtrlOfs dw 0h
RealTimeCtrlSeg dw 0h

    iret

endp

; --------------------

Activity db 0h

; -------MY-TC--------

MyTimeCtrl proc

    cmp cs:First_print, 0FFh
    jne llComplete                                          ; Check if the frame is on the screen

    call LoadBackground                                     ; Load the screen
    mov cs:First_print, 0h

llComplete:
    call RegistersValue                                     ; Put registers' values to the text

    push ax bx cx dx es si di

    call MakeFrame                                          ; Draw the frame

    pop di si es dx cx bx ax

    ret

endp

; --------------------


;---------------------------------
; It loads the background of the frame
;
; Entry:  START_X, START_Y
; Exit:   Background:
; Destrs: None
;---------------------------------

LoadBackground  proc

    cld

    push ax cx ds es si di

    mov ax, VIDEOSEG
    mov ds, ax

    mov si, 0h

    mov cx, TERMINAL_HEIGHT
    imul cx, cx, TERMINAL_LEN

    mov ax, cs
    mov es, ax

    mov di, offset Background

rep movsw                                       ; Load the screen

    pop di si es ds cx ax

    ret

endp

; --------------------


;---------------------------------
; It storage the background of the frame
;
; Entry:  CS:MOVED, START_X, START_Y, Background:
; Exit:   VIDEOSEG
; Destrs: None
;---------------------------------

StorageBackground  proc

    cld

    push ax bx cx ds es si di

    mov bx, offset JumpTableMovement
    mov al, cs:MOVED
    mov ah, 0h
    dec ax
    shl ax, 01h
    add bx, ax
    mov bx, cs:[bx]
    jmp bx                                          ; Jump to the label with number in CS:MOVED

llComplete:

    mov si, ax
    mov di, ax
    add si, offset Background

    mov ax, VIDEOSEG
    mov es, ax

    mov ax, cs
    mov ds, ax

    add ax, TERMINAL_LEN * FRAME_HEIGHT * 2

llWhile:
    mov cx, FRAME_LENGTH

rep movsw                                           ; Draw the line of the frame

    add si, TERMINAL_LEN * 2 - FRAME_LENGTH * 2
    add di, TERMINAL_LEN * 2 - FRAME_LENGTH * 2

    cmp ax, di
    ja llWhile

    pop di si es ds cx bx ax

    ret

;-----------------------------------------------
; AX pointed on the first printed symbol
;-----------------------------------------------

UpMovement:

    mov al, cs:START_Y
    inc al
    mov ah, 0h
    imul ax, ax, TERMINAL_LEN
    mov cl, cs:START_X
    mov ch, 0h
    add ax, cx
    shl ax, 01h

    jmp llComplete

LeftMovement:

    mov al, cs:START_Y
    mov ah, 0h
    imul ax, ax, TERMINAL_LEN
    mov cl, cs:START_X
    inc cl
    mov ch, 0h
    add ax, cx
    shl ax, 01h

    jmp llComplete

RightMovement:

    mov al, cs:START_Y
    mov ah, 0h
    imul ax, ax, TERMINAL_LEN
    mov cl, cs:START_X
    dec cl
    mov ch, 0h
    add ax, cx
    shl ax, 01h

    jmp llComplete

DownMovement:

    mov al, cs:START_Y
    dec al
    mov ah, 0h
    imul ax, ax, TERMINAL_LEN
    mov cl, cs:START_X
    mov ch, 0h
    add ax, cx
    shl ax, 01h

    jmp llComplete

NoMovement:

    mov al, cs:START_Y
    mov ah, 0h
    imul ax, ax, TERMINAL_LEN
    mov cl, cs:START_X
    mov ch, 0h
    add ax, cx
    shl ax, 01h

    jmp llComplete

endp

; --------------------

; --------------------

JumpTableMovement:
    dw offset UpMovement
    dw offset LeftMovement
    dw offset RightMovement
    dw offset DownMovement
    dw offset NoMovement

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

    mov cs:Save_ax, ax
    mov cs:Save_di, di

;---------------AX-----------------
    mov di, offset Str_ax + 05h         ; 5 = strlen ('ax = ')
    call ValToStr
;---------------------------------

;---------------BX-----------------
    mov ax, bx
    mov di, offset Str_bx + 05h
    call ValToStr
;---------------------------------

;---------------CX-----------------
    mov ax, cx
    mov di, offset Str_cx + 05h
    call ValToStr
;---------------------------------

;---------------DX-----------------
    mov ax, dx
    mov di, offset Str_dx + 05h
    call ValToStr
;---------------------------------

;---------------SI-----------------
    mov ax, si
    mov di, offset Str_si + 05h
    call ValToStr
;---------------------------------

;---------------DI-----------------
    mov ax, cs:Save_di
    mov di, offset Str_di + 05h
    call ValToStr
;---------------------------------

;---------------BP-----------------
    mov ax, bp
    mov di, offset Str_bp + 05h
    call ValToStr
;---------------------------------

;---------------SP-----------------
    mov ax, sp
    sub ax, 05h * 2                     ; 5 = return value of RegistersValue | return value of MyTimeCtrl | IP | CS
                                        ; every element have size of 2 bytes
    mov di, offset Str_sp + 05h
    call ValToStr
;---------------------------------

;---------------DS-----------------
    mov ax, ds
    mov di, offset Str_ds + 05h
    call ValToStr
;---------------------------------

;---------------ES-----------------
    mov ax, es
    mov di, offset Str_es + 05h
    call ValToStr
;---------------------------------

;---------------SS-----------------
    mov ax, ss
    mov di, offset Str_ss + 05h
    call ValToStr
;---------------------------------

    pop cs:Save_ret_val_RV cs:Save_ret_val_MYTC cs:Save_ip cs:Save_cs

;---------------CS-----------------
    mov ax, cs:Save_cs
    mov di, offset Str_cs + 05h
    call ValToStr
;---------------------------------

;---------------IP-----------------
    mov ax, cs:Save_ip
    mov di, offset Str_ip + 05h
    call ValToStr
;---------------------------------

    push cs:Save_cs cs:Save_ip cs:Save_ret_val_MYTC cs:Save_ret_val_RV

    mov ax, cs:Save_ax
    mov di, cs:Save_di

    ret

endp

; --------------------


; --------------------

Save_ax dw 0h
Save_di dw 0h
Save_ip dw 0h
Save_cs dw 0h

Save_ret_val_RV   dw 0h                             ; The returning value of RegistersValue
Save_ret_val_MYTC dw 0h                             ; The returning value of MyTimeCtrl

;----------------------TEXT------------------------

Str_ax db 'ax = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_bx db 'bx = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_cx db 'cx = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_dx db 'dx = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah

Str_si db 'si = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_di db 'di = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah

Str_bp db 'bp = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_sp db 'sp = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah

Str_ds db 'ds = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_es db 'es = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_ss db 'ss = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah
Str_cs db 'cs = ', 0FFh, 0FFh, 0FFh, 0FFh, 0Ah

Str_ip db 'ip = ', 0FFh, 0FFh, 0FFh, 0FFh, END_SYM

; -------------------------------------------------


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
    call DigitToStr                     ; Print the first digit of AX

    inc di

    pop ax

    shl ah, 04h
    shr ah, 04h
    call DigitToStr                     ; Print the second digit of AX

    inc di

    mov ah, al
    shr ah, 04h
    call DigitToStr                     ; Print the third digit of AX

    inc di

    mov ah, al
    shl ah, 04h
    shr ah, 04h
    call DigitToStr                     ; Print the fourth digit of AX

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

    mov cl, cs:START_X
    mov ch, cs:START_Y

    mov bx, offset FramePattern

    call DrawFrame

    mov cl, cs:START_X
    mov ch, cs:START_Y

    mov dx, offset Str_ax

    call PrintText

    ret

endp

; --------------------

; -------FRAME--------

START_X db 41h
START_Y db 1h

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
; Destrs: AX, CX, DX, DI
;---------------------------------

DrawFrame     proc

    add ah, ch

    mov cs:BotCor, ah
    mov ah, FRAME_COLOR

    call BaseLine                       ; Draw the first line
    inc ch

llCond:
    cmp cs:BotCor, ch
    je llEnd

llFor:
    call BaseLine                       ; Draw 2nd - (n-1)th line
    inc ch
    sub bx, 03h

    jmp llCond

llEnd:
    add bx, 03h
    call BaseLine                       ; Draw the last line

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
; Destrs: DX, BX (inc x3), DI
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

    mov al, cs:[bx]
    stosw                           ; Draw the first symbol of the line

    inc bx
    mov al, cs:[bx]

    push cx
    mov cx, FRAME_LENGTH - 2
    rep stosw                       ; Draw other symbols (without the last symbol)
    pop cx

    inc bx
    mov al, cs:[bx]                 ; Draw the last symbol
    stosw

    inc bx                          ; BX pointed on the next line pattern

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
    cmp byte ptr cs:[si], END_SYM      ; Check if it is the end
    je llEnd
    cmp byte ptr cs:[si], 0Ah          ; Check if it is a new line ('\n')
    je llNewLine

llWhile:
    inc ax
    movsb                              ; Print the symbol of the text
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

Background:

end Main
