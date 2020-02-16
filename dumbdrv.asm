;
; DumbDrv - Dummy DOS Device Driver
; Copyright (C) 1994-1997 Marcus Better
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;

IDEAL
JUMPS
LOCALS

;========================================================= CONSTANTS
; Status codes
stSuccess 	EQU	0100h
stError		EQU	8100h
stUnknownCmd	EQU	8103h
stDrvNotReady	EQU	8102h

MediaID		EQU	0F0h	; Media ID byte - "other"

;========================================================= Driver segment
SEGMENT DrvSeg
	ASSUME cs:DrvSeg,ds:NOTHING,es:NOTHING,ss:NOTHING
; Device header
	DW	0FFFFh, 0	; Link to next driver
	DW	0		; Attributes
	DW	Strategy, Interrupt
	DB	1, 7 DUP (0)	; Number of units

;========================================================= Resident DATA area
; Data area
ReqHdrOfs DW	0		; Offset ot request header
ReqHdrSeg DW	0		; Segment of request header
VolumeID DB	"DUMBDRV", 0
; BIOS Parameter Block
LABEL	BPB
BytesPerSec	DW	512
SecPerClust	DB	2
ResSectors	DW	1
FATs		DB	1
RootDirEnts	DW	32
Sectors		DW	512
Media		DB	0F0h
FATsecs		DW	4
SecPerTrack	DW	9
Heads		DW	2
HiddenSecs	DD	0
HugeSectors	DD	0
pBPB	DW	26 DUP(OFFSET BPB) ; Array of BPB pointers

JmpTbl	DW	Init, MediaChk, BuildBPB, 0
	DW	NotReady, 0, 0, 0
	DW	NotReady, NotReady

;========================================================= STRATEGY routine
PROC	Strategy FAR
	mov	[ReqHdrOfs], bx
	mov	[ReqHdrSeg], es
	ret
ENDP	Strategy

;========================================================= INTERRUPT routine
PROC	Interrupt FAR
	push	ax bx cx dx si di bp ds es
	pushf

	push	cs
	pop	ds
	ASSUME	ds:DrvSeg
	les	bx, [DWORD ReqHdrOfs]
	mov	al, [es:bx+2]	; Function number
	cmp	al, 09h
	ja	@@errUnknownCommand
	xor	ah, ah
	mov	si, ax
	shl	si, 1
	mov	cx, [JmpTbl+si]
	jcxz	@@errUnknownCommand
	call	cx
	jmp	@@Exit

@@errUnknownCommand:
	mov	[WORD es:bx+3], stUnknownCmd
@@Exit:	popf
	pop	es ds bp di si dx cx bx ax
	ret
ENDP	Interrupt

;========================================================= Functions
; Media check
PROC	MediaChk
	mov	[WORD es:bx+3], stSuccess	; Status
	mov	[BYTE es:bx+14], 1		; Media unchanged
	mov	[WORD es:bx+15], OFFSET VolumeID
	mov	[es:bx+17], cs
	ret
ENDP	MediaChk

; Build BPB
PROC	BuildBPB
	mov	[WORD es:bx+3], stSuccess	; Status
	mov	[WORD es:bx+18], OFFSET BPB
	mov	[es:bx+20], cs
	ret
ENDP	BuildBPB

; Return "Drive not ready" error for Read or Write req.
PROC	NotReady
	mov	[WORD es:bx+3], stDrvNotReady
	mov	[WORD es:bx+18], 0		; Sectors read/written
	mov	[WORD es:bx+22], OFFSET VolumeID
	mov	[WORD es:bx+24], cs
	ret	
ENDP	NotReady

LABEL	EndResCode
;========================================================= INIT Code
PROC	Init
; Say hello
	mov	ah, 09h
	mov	dx, OFFSET HelloMsg
	int	21h
; Parse command line
	call	ParseCmdLine
	jc	@@CmdLineError
	or	al, al
	jz	@@CmdLineError			; Zero drives specified
	mov	ah, [es:bx+22]			; First drive number
	add	ah, al
	cmp	ah, 25				; Maximum number of drives
	jna	@@DrivesOk
	mov	al, 26
	sub	al, [es:bx+22]
@@DrivesOk:
	mov	[Drives], al
; Set fields
	mov	[BYTE es:bx+13], al		; Number of units
	mov	[WORD es:bx+3], stSuccess	; Status
	mov	[WORD es:bx+14], OFFSET EndResCode
	mov	[es:bx+16], cs
	mov	[WORD es:bx+18], OFFSET pBPB
	mov	[es:bx+20], cs
	mov	[WORD es:bx+23], 1		; Error message flag
; Print message
	mov	ah, [es:bx+22]			; Drive number
	add	[DrvLetter], ah
	mov	ah, 09h
	mov	dx, OFFSET InstallMsg
	int	21h
	mov	al, [Drives]
	cmp	al, 1
	je	@@Msg1
	add	al, [es:bx+22]
	dec	al
	add	[DrvLetter2], al
	mov	ah, 09h
	mov	dx, OFFSET InstMsg2
	int	21h
	ret
@@Msg1:
	mov	ah, 09h
	mov	dx, OFFSET MsgEnd
	int	21h
	ret
; Bad cmd line syntax - fail init
@@CmdLineError:
	mov	ah, 09h
	mov	dx, OFFSET Syntax
	int	21h
	mov	[BYTE es:bx+13], 0
	mov	[WORD es:bx+14], 0
	mov	[WORD es:bx+13], stError
	ret
ENDP	Init

;---------------------------------------------------------------------
; Parse command line.
PROC    ParseCmdLine
	push	ds
	push	bx cx dx si di
	cld
	lds	si, [es:bx+18]
	ASSUME	ds:NOTHING
; Scan past the filename to the first space
@@EatFilename:
	lodsb
	cmp	al, 0Dh
	je	@@NoParams
	cmp	al, 0Ah
	je	@@NoParams
	cmp	al, ' '
	jne	@@EatFilename
; Eat spaces
@@EatSpaces:
	lodsb
	cmp	al, ' '
	je	@@EatSpaces
	cmp	al, 0Ah
	je	@@NoParams
	cmp	al, 0Dh
	je	@@NoParams
	xor	dx, dx
; Parse one- or two-digit number
	cmp	al, '0'
	jb	@@Error
	cmp	al, '9'
	ja	@@Error
	mov	dl, al
	sub	dl, '0'
	lodsb
	cmp	al, 0Ah
	je	@@Done
	cmp	al, 0Dh
	je	@@Done
	cmp	al, ' '
	je	@@EndParams
	cmp	al, '0'
	jb	@@Error
	cmp	al, '9'
	ja	@@Error
	sub	al, '0'
	mov	dh, dl
	mov	cl, 3
	shl	dh, cl
	shl	dl, 1
	add	dl, dh
	add	dl, al
; Make sure command line ends here
@@EndParams:
	lodsb
	cmp	al, 0Ah
	je	@@Done
	cmp	al, 0Dh
	je	@@Done
	cmp	al, ' '
	je	@@EndParams

@@Error:
	stc
	jmp	@@Exit
@@NoParams:
	mov	ax, 1
	clc
	jmp	@@Exit
@@Done:
	mov	al, dl
	xor	ah, ah
	clc
	jmp	@@Exit

@@Exit:
	pop	di si dx cx bx
	pop	ds
	ret
ENDP    ParseCmdLine

;========================================================= INIT data
HelloMsg DB	10,13,"DumbDrv     Version 1.10     96-01-03", 10, 13
	DB      "Copyright (c) 1994-96, Marcus Better.",10,13,10,13,"$"
InstallMsg DB	"DumbDrv installed as "
DrvLetter DB	"A:","$"
InstMsg2 DB	" - "
DrvLetter2 DB	"A:"
MsgEnd	DB	10,13,"$"

Syntax	DB	"Invalid arguments.",10,13,"$"
Drives	DB	1	; Number of drives to support

ENDS	DrvSeg
	END