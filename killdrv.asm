;
; KillDrv - Drive removal utility
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
	MODEL	SMALL
	JUMPS
	LOCALS
;========================================================= MACROs
MACRO   Abort   Code
	mov     bx, Code
	call    AbortMsg
ENDM

	STACK
;========================================================= CODE segment
	CODESEG
	STARTUPCODE

PROC	Main
; Write hello message
	mov     ah, 09h
	mov     dx, OFFSET MsgHello
	int     21h
; Check for DR-DOS or Novell DOS
	mov	ax, 4452h
	stc
	int	21h
	jc	@@NotDrDos
; DR-DOS or Novell DOS found. Check version.
	cmp	ah, 10h
	jne	@@errBadDosVer
	cmp	al, 72h		; Novell DOS
	je	@@NovellDOS
	mov	[DrDos], al	; DR-DOS version code
	mov	[CDSSize], 51h	; CDS size different under DR-DOS
	cmp	al, 65h		; DR-DOS 5
	je	@@DosVerOk
	cmp	al, 67h		; DR-DOS 6
	je	@@DosVerOk
	jmp	@@errBadDosVer
@@NovellDOS:
;	mov	[Novell], 1
	jmp	@@DosVerOk
@@NotDrDos:
; Check DOS version
	mov     ax, 3000h
	int     21h
	cmp     al, 4
	jb      @@errBadDOSVer
	cmp     al, 10
	jae	@@errBadDOSVer
@@DosVerOk:

;Parse command line
	call	ParseCmdLine
	cmp	[DriveNo], 0FFh
	jne	@@DrvSpecd
	Abort	1			; Drive not specified
@@DrvSpecd:
; Get list of lists.
	mov	ah, 52h
	int	21h			; ES:BX->List of lists
; Compare drive letter with lastdrive.
	mov	al, [es:bx+21h]	
	cmp	al, [DriveNo]
	ja	@@LastdrvOk
	Abort	2			; Invalid drive letter
@@LastdrvOk:
; Find CDS for drive
	les	bx, [es:bx+16h]		; CDS array
	mov     al, [DriveNo]
	mov     dl, [BYTE CDSSize]
	mul     dl
	add     bx, ax          	; CDS entry for drive
	cmp	[DrDos], 0
	jz	@@DisableMSDOS
	mov	[WORD es:bx+43h], 0
	jmp	@@Disabled
@@DisableMSDOS:
	and	[BYTE PTR es:bx+44h], 03Fh ; Remove drive
@@Disabled:
; Print message and return
	mov	al, [DriveNo]
	add	[RemovedDrv], al
	mov	dx, OFFSET OkMsg
	mov	ah, 9
	int	21h
	mov	ax, 4C00h
	int	21h

@@errBadDosVer:
	Abort   3
ENDP	Main

;---------------------------------------------------------------------
PROC    AbortMsg
	ASSUME  ds:@data,es:NOTHING
	mov     ax, @data
	mov     ds, ax
	shl     bx, 1
	mov     dx, [ErrMsgTbl+bx]
	mov     ah, 9
	int     21h
	mov     ax, bx
	shr     ax, 1
	mov     ah, 4Ch
	int     21h
ENDP    AbortMsg

;---------------------------------------------------------------------
; Parse command line.
PROC    ParseCmdLine STDCALL
	LOCAL   LastByte
	mov	ah, 51h
	int	21h		; Get PSP
	mov	es, bx
	mov     al, [BYTE PTR es:80h]
	cmp     al, 2
	jb      @@Done
	add     al, 80h
	xor     ah, ah
	mov     [LastByte], ax
; Find first non-space character
	mov     di, 81h
@@Next:
	mov     cx, [LastByte]
	sub     cx, di
	jc      @@Done
	inc     cx
	mov     al, ' '
	cmp     al, ' '         ; Set zero flag
	repe scasb
	je      @@Done
	dec     di
; Get char and convert to upper case
	mov     dl, [es:di]
	mov     ax, 6520h
	int     21h
; Check for drive spec 'A'..'Z'
	cmp     dl, 'A'
	jb      @@Parse2
	cmp     dl, 'Z'
	ja      @@Parse2
; Drive spec.
	inc     di
	mov     al, ':'                 ; Match a colon
	scasb
	jne     @@errBadOption
	sub     dl, 'A'                 ; Drive number
	mov     [DriveNo], dl
	jmp     @@Next
@@Parse2:
; Check for switches
	mov     al, '/'
	scasb
	jne     @@errBadOption
; Switch
	cmp     di, [LastByte]
	ja      @@errBadOption
	mov     dl, [es:di]
	inc     di
	mov     ax, 6520h		; Upper case
	int     21h
; Insert any switches here...
	jmp     @@errBadOption

@@errBadOption:
	Abort   0			; Bad cmdline syntax

@@Done:
	ret
ENDP    ParseCmdLine

;========================================================= DATA segment
	DATASEG
DriveNo	DB	0FFh
DrDos	DB	0	; Nonzero if DR-DOS (zero if Novell DOS)
CDSSize	DW	58h	; Size of CDS entry
; Messages
ErrMsgTbl DW	MsgBadOption		; 0 - Bad cmdline syntax
	DW	MsgNoDriveSpec		; 1 - No drive specified
	DW	MsgInvalidDrv		; 2 - Invalid drive letter
	DW	MsgBadDOSVer		; 3 - Bad DOS version

MsgHello DB     "KillDrv            Drive removal utility           "
	DB	"Version 1.01    97-05-03",10,13
	DB      "Copyright (c) 1994-1997 Marcus Better.",10,13,10,13,"$"
MsgBadOption DB "Invalid command line arguments.",10,13,10,13
	DB      "Syntax: KILLDRV d:",10,13
	DB      "where d is the drive letter to be removed.",10,13,"$"
MsgNoDriveSpec DB "No drive letter specified.",10,13,"$"
MsgInvalidDrv DB "Invalid drive letter.",10,13,"$"
MsgBadDOSVer DB	"Wrong DOS version.",10,13,"$"
OkMsg	DB	"Removed drive "
RemovedDrv DB	"A:",10,13,"$"
;========================================================= 
	END