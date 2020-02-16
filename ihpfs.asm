; iHPFS 1.29 2000.07.16..2000.07.29 Veit Kannegieser
;
; iHPFS - HPFS driver for DOS
; Copyright (C) 1993-1998 Marcus Better
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

;==================================================================
; This program is written for Turbo Assembler 4.1, and uses
; ideal mode syntax. To assemble, use the /m option for multiple
; passes.
;==================================================================

IDEAL
P386
JUMPS
LOCALS

;------------------------------------------------- Constants
; Version
VerMajor        EQU     1
VerMinor        EQU     29

; Errors
errFileNotFound EQU     02h
errPathNotFound EQU     03h
errTooManyFiles EQU     04h
errAccessDenied EQU     05h
errInvalidHandle EQU    06h
errNoMoreFiles  EQU     12h
errWriteProt    EQU     13h
errSectorNotFound EQU   1Bh

TimeZone        EQU     -3600
MinCacheSize    EQU     32      ; Minimum cache size in KB
MaxCacheSize    EQU     32768   ; Maximum cache size in KB
LoadFactor      EQU     5       ; Elements/chain in hash.
CacheEntrySize  EQU     14      ; Cache entry size
MaxPathLength   EQU     57      ; Maximum length of pathname in DOS

; Disk access methods
Method_CHS      EQU     0       ; CHS addressing
Method_CHSExt   EQU     1       ; Extended CHS addressing
Method_Ext      EQU     2       ; IBM/MS Extensions
Method_ASPI     EQU     3       ; ASPI manager

HighestDriveLetter EQU '`'    ; Highest drive letter allowed
MaxDrives       EQU HighestDriveLetter-'A'+1

;------------------------------------------------- Macros
MACRO   Abort   Code
        ;int 3
        mov     bx, Code
        call    AbortMsg
ENDM

MACRO   NOASSUME
        ASSUME  cs:NOTHING,ds:NOTHING,es:NOTHING,fs:NOTHING,gs:NOTHING,ss:NOTHING
ENDM

MACRO   DEFASSUME
        ASSUME  cs:ResCode,ds:ResCode,es:NOTHING,fs:NOTHING,gs:ResData,ss:NOTHING
ENDM

MACRO   CALL_XMS
        call    [XMSEntry]
        ;cmp     ax, 1
        ;db 075h,001h
        ;int 3
        or      ax, ax
ENDM

;------------------------------------------------- Structures
STRUC   XMSMoveStruct
  Length        DD      0       ; Number of bytes to transfer
  SourceHandle  DW      0       ; Handle of source block
  SourceOffset  DD      0       ; Offset into source block
  DestHandle    DW      0       ; Handle of dest block
  DestOffset    DD      0       ; Offset into dest block
ENDS    XMSMoveStruct

STRUC   DiskAddrPacketStruct    ; IBM/MS Extensions disk address packet
  Length        DB      10h     ; Length of packet
                DB      0
  Count         DW      0       ; Number of blocks to transfer
  Buffer        DD      0       ; Address of transfer buffer
  Sector        DQ      0       ; Starting absolute sector number
ENDS    DiskAddrPacketStruct

STRUC   SCSIRequestBlockStruct
  request_number                DB     2   ; execute SCSI I/O
  request_status                DB     0   ; not done yet
  host_adapter_id               DB     0
  request_flags                 DB     8   ; transfer from SCSI to host
                                DD     0   ; reserved
  target_id                     DB     0
  logical_unit                  DB     0
  data_allocation_length        DD     0
  sense_allocation_length       DB     0
  data_buffer_pointer           DD     0
                                DD     0   ; next request block
  CDB_length                    DB     10
  host_adapter_status           DB     0
  target_status                 DB     0
                                DD     0   ; post routine address
                                DW     0   ; realmode post DS
                                DD     0   ; SRB pointer
                                DW     0   ; reserved
                                DD     0   ; SRB physical address
                                DB 22 DUP(0) ; SCSIMGR$ workspace
  ccb_00_command                DB   028h  ; group 1 read
  ccb_01_flags                  DB     0
  ccb_02_lba                    DD     0   ; reversed
  ccb_06                        DB     0
  ccb_07_count                  DW 00100h  ; 1 reversed
  ccb_09                        DB     0
                                DB 64 DUP(0)
ENDS    SCSIRequestBlockStruct

;------------------------------------------------- Procedures
PROCDESC DiskRead PASCAL NEAR :DWORD,:WORD,:DWORD
PROCDESC ReadSector PASCAL NEAR :DWORD,:WORD,:DWORD
PROCDESC ReadFileSector PASCAL NEAR :DWORD,:WORD,:DWORD,:DWORD
PROCDESC ChkPathLength PASCAL NEAR :DWORD
PROCDESC ScanPartTbl PASCAL NEAR :WORD,:WORD,:WORD,:WORD,:WORD,:WORD,:WORD,:WORD,:DWORD
PROCDESC CheckHPFSPart PASCAL NEAR :WORD,:WORD,:WORD,:WORD,:WORD,:WORD,:WORD,:DWORD
PROCDESC ReadSectorCHS    PASCAL NEAR :DWORD,:WORD,:WORD,:WORD,:DWORD
PROCDESC ReadSectorExtCHS PASCAL NEAR :DWORD,:WORD,:WORD,:WORD,:DWORD
PROCDESC ReadSectorASPI   PASCAL NEAR :DWORD,:WORD,:WORD,:WORD,:DWORD
PROCDESC CheckCylNumber PASCAL NEAR :DWORD,:WORD,:WORD
PROCDESC IsInstalledPart PASCAL NEAR :WORD
PROCDESC QueryPart PASCAL NEAR :WORD
PROCDESC RemoveDrv PASCAL NEAR :WORD
PROCDESC QueryDrive PASCAL NEAR :WORD
PROCDESC DeleteChangedMediaDrive PASCAL NEAR :WORD
PROCDESC UninstallDriver PASCAL NEAR
;======================================================= Resident section
SEGMENT ResCode
        ASSUME  cs:ResCode,ds:NOTHING,es:NOTHING,ss:NOTHING,fs:NOTHING,gs:NOTHING
;-------------------------------------------------- INT 2D entry
; Follows the Alternate Multiplex Interrupt Specification (AMIS) [v3.5.1]
; This is an IBM interrupt sharing protocol entry point.
; The entry point is located in the beginning of the segment,
; so that the rest of the segment may be released on uninstall.
Int2DEntry:
        jmp     SHORT JmpToInt2DHandler
OldInt2D DD     0       ; Saved vector for next handler in chain
        DW      424Bh   ; Protocol signature
        DB      00h     ; EOI flag - software interrupt
        jmp     SHORT HardwareReset2D   ; Hardware reset routine
        DB      7 DUP(0) ; Reserved
JmpToInt2DHandler:
        jmp     Int2DHandler
HardwareReset2D:
        retf

;-------------------------------------------------- INT 2F entry
; This is an IBM interrupt sharing protocol entry point.
Int2FEntry:
        jmp     SHORT JmpToInt2FHandler
OldInt2F DD     0       ; Saved vector for next handler in chain
        DW      424Bh   ; Protocol signature
        DB      00h     ; EOI flag - software interrupt.
        jmp     SHORT HardwareReset2F ; Hardware reset routine
        DB      7 DUP(0) ; Reserved
JmpToInt2FHandler:
        jmp     Int2FHandler
HardwareReset2F:
        retf
EndUninstalledCode:     ; Last byte to keep when uninstalled

;-------------------------------------------------- Common resident data
DataSegs DW     MaxDrives DUP(0)
                                ; Data segments for the drives
; AMIS information
AMISSign DB     "M Better"      ; Manufacturer name
        DB      "iHPFS   "      ; Product name
        DB      "HPFS Driver for DOS", 0 ; Description, ASCIIZ.
HookList DB     2Fh
        DW      OFFSET Int2FEntry
        DB      2Dh
        DW      OFFSET Int2DEntry
ApiFunc DB     0                ; INT 2D function # for API.
; Saved registers and vectors
FuncAddr DW     0               ; Pointer to current redirector function
ResDataSeg DW   0               ; Current resident data segment
DriveNo DB      0               ; Current drive number
SaveSP  DW      0               ; SP on entry to interrupt handler
SaveSS  DW      0               ; SS on entry to interrupt handler
DosVersion DB   0               ; Major DOS version, zero if Novell/Dr DOS
Novell  DB      0               ; Set if Novell DOS
Win95   DB      0               ; Set if running under Windows95
; Pointers to SDA fields. Layout:
;                               DOS4+   DOS 3, DR-DOS
;  DTA ptr                      0Ch     0Ch
;  First filename buffer        9Eh     92h
;  Search data block            19Eh    192h
;  Dir entry for found file     1B3h    1A7h
;  Search attributes            24Dh    23Ah
;  File access/sharing mode     24Eh    23Bh
;  Ptr to current CDS           282h    26Ch
;  Extended open mode           2E1h    Not supported
SDA     DD      0               ; Address of DOS Swappable Data Area
PSP     DW      0               ; Program Segment Prefix
pCurrCDS DD     282h            ; Pointer to current CDS
pDTA    DD      0Ch             ; Pointer to current DTA
FN1     DD      9Eh             ; Address of first filename field
AccMode DD      24Eh            ; Address of file access/sharing mode field
SrchAttr DD     24Dh            ; Address of search attributes
ExtOpenMode DD  2E1h            ; Address of extended open mode
; Buffers.
Buf1    DB      512 DUP(0)
Buf2    DB      512 DUP(0)
Buf3    DB      512 DUP(0)
Buf4    DB      512 DUP(0)
FNameBuf DB     128 DUP(0)
FNameBuf2 DB    128 DUP(0)
BufUsed DB      0       ; Flag for FindNext, set if buffers untouched.
; Some flags
Multitrack DB   1       ; Allow multitrack reads and writes.
Limit2GB DB     0       ; Let DiskSize/DiskFree report < 2GB
; Characters permitted in filenames, etc
MinPerm DB      0       ; Lowest permissible character
MaxPerm DB      255     ; Highest permissible character
MinExcl DB      0       ; Lowest excluded character
MaxExcl DB      0       ; Highest excluded character
NumTerm DB      0       ; Number of illegal (terminator) characters
TermChars DB    32 DUP(0) ; Array of illegal characters
UpCaseTbl DB    128 DUP(0) ; File Character Upper-Case Table
DaysInMonth DB  31,28,31,30,31,30,31,31,30,31,30,31
ConvertLong DB  0       ; Convert long filenames flag
ConvTable DB    "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_!#%"
; Cache info
CacheOn DB      0       ; Flag: Caching active?
XMSEntry DD     0       ; XMS driver entry point
hHashTable DW   0       ; XMS handle to the hash table
hCacheLists DW  0       ; XMS handle to the hash chains
hCacheSectors DW 0      ; XMS handle to the cache sectors
CacheEntries DW 0       ; Entries in the cache
HashSize DW     0       ; Slots in the hash table
FreeEntry DW    0       ; Next free entry pointer
XMoveStruc XMSMoveStruct <>
XMSError DB     0       ; Set if XMS call failed
EntryBuf DB     20 DUP(0)

; IBM/MS Extensions data
DiskAddrPkt      DiskAddrPacketStruct   <>
SCSIRequestBlock SCSIRequestBlockStruct <>

; Jump Table, 0 means unsupported
JmpTbl  DW      0                       ; 00 Install check, handled separate
        DW      RmDir                   ; 01
        DW      0                       ; 02 DOS 4.x only
        DW      MkDir                   ; 03
        DW      0                       ; 04 DOS 4.x only
        DW      ChDir                   ; 05
        DW      Close                   ; 06
        DW      Commit                  ; 07
        DW      Read                    ; 08
        DW      Write                   ; 09
        DW      LockRegion              ; 0a
        DW      UnlockRegion            ; 0b
        DW      DiskFree                ; 0c
        DW      0                       ; 0d DOS 4.x only, SetAttrib
        DW      SetAttrib               ; 0e
        DW      GetAttrib               ; 0f
        DW      0                       ; 10 DOS 4.x only, GetAttrib
        DW      Rename                  ; 11
        DW      0                       ; 12 DOS 4.x only, Rename
        DW      Delete                  ; 13
        DW      0                       ; 14 DOS 4.x only, Delete
        DW      0                       ; 15 DOS 4.x only, Open
        DW      Open                    ; 16
        DW      Create                  ; 17
        DW      0                       ; 18 !!!! Create/Truncate FILE without CDS
        DW      0                       ; 19 FindFirst withou CDS
        DW      0                       ; 1a DOS 4.x only, FindNext without CDS
        DW      FindFirst               ; 1b
        DW      FindNext                ; 1c
        DW      0                       ; 1d !! close all remote files for process
        DW      0                       ; 1e Do Redirection
        DW      0                       ; 1f Printer Setup
        DW      0                       ; 20 Flush all Disk Buffers
        DW      Seek                    ; 21 !!! Seek from End of remote File
        DW      0                       ; 22 ! Process Termination Hook
        DW      0                       ; 23 !!!! Qualify Remote Filename
        DW      0                       ; 24 Turn Off Remote Printer
        DW      0                       ; 25 Redirected Printer Mode
        DW      0                       ; 26 Remote Printer Echo On/Off
        DW      0                       ; 27 unused/NW Remote File Copy
        DW      0                       ; 28 unused
        DW      0                       ; 29 unused
        DW      0                       ; 2a DOS 4.x only, Close all files
        DW      0                       ; 2b ??? Generic IOCTL
        DW      0                       ; 2c "UPDATE_CB" ?
        DW      0                       ; 2d DOS 4.x only, Extended Attributes
        DW      ExtOpen                 ; 2e
                                        ; 2f DOS 4.x only, IFS IOCTL
                                        ; 30 DOS 4.x only

SCSIMGR DD      0               ; ASPI entrypoint

;------------------------------------------------------------------------
; Int 2D (Alternate Multiplex) handler.
PROC    Int2DHandler FAR
        NOASSUME
        ASSUME  cs:ResCode
        cmp     ah, [ApiFunc]
        je      apiApiCall
        jmp     [OldInt2D]
apiApiCall:
        or      al, al          ; Installation check
        je      apiInstallChk
        cmp     al, 2           ; Uninstall
        je      apiUnInstall
        cmp     al, 4           ; Determine chained interrupts
        je      apiGetHookList
        cmp     al, 10h         ; Query drive installed
        je      apiQueryDrive
        xor     al, al          ; Not implemented
        iret

; iHPFS installation check.
apiInstallChk:
        mov     al, 0FFh        ; Multiplex number in use
        mov     ch, VerMajor
        mov     cl, VerMinor
        mov     dx, cs
        mov     di, OFFSET AMISSign ; DX:DI -> AMIS signature
        iret

apiGetHookList:
        mov     dx, cs
        mov     bx, OFFSET HookList
        mov     al, 04h         ; Hook list returned.
        iret

apiUnInstall:
        mov     bx, ResCode
        mov     al, 03h
        iret

; Query drive installed. Drive number in BX (will be destroyed). Returns
; AH=1 if installed, AH=0 otherwise.
apiQueryDrive:
        shl     bx, 1
        cmp     [DataSegs+bx], 0
        setnz   ah
        iret

ENDP    Int2DHandler

;------------------------------------------------------------------------
; Int 2F (Multiplex) Interrupt handler.
; Processes calls for function 11h.
PROC    Int2FHandler FAR
        NOASSUME
        ASSUME  cs:ResCode
        sti
        cmp     ah, 11h
        je      Function11
        jmp     [OldInt2F]

Function11:
; Decide which method to use for determining if the call is for us.
        or      al, al
        jz      NetInstallChk   ; Redirector installation check
        call    DetectWin95
        cmp     al, 21h         ; Seek
        je      CheckSFT
        cmp     al, 1Ch         ; Find next
        je      CheckFindNext
        cmp     al, 2Eh
        je      CheckCDS
        cmp     al, 1Dh
        jbe     Function11_1
        jmp     [OldInt2F]
Function11_1:
        cmp     al, 06h
        jb      CheckCDS
        cmp     al, 0Bh
        jna     CheckSFT

; CDS method: Check path field of CDS.
CheckCDS:
        push    ds bx
        lds     bx, [pCurrCDS]
        lds     bx, [bx]                ; CDS for current file
        movzx   bx, [BYTE bx]
        cmp     bl, '\'
        je      CheckCDS1               ; Not for us - ZF set
        sub     bl, 'A'
        cmp     bl, MaxDrives
        jb      @@valid1
        pop     bx ds
        jmp     @@NotForUs
@@valid1:
        mov     [DriveNo], bl
        shl     bx, 1
        mov     bx, [DataSegs+bx]
        mov     [ResDataSeg], bx
        or      bx, bx                  ; ZF set if unsupported drive
CheckCDS1:
        pop     bx ds
        jnz     CallForUs
@@NotForUs:
        jmp     [DWORD OldInt2F] ; Call was not for us


; SFT method: Check drive number in SFT entry.
; ES:DI -> SFT entry for file
CheckSFT:
        push    bx
        movzx   bx, [BYTE es:di+5]
        and     bl, 3Fh                 ; Bits 5-0 contain drive number
        cmp     bl, MaxDrives           ; personal netware uses 0EEh
        jb      @@valid2
        pop     bx
        jmp     @@NotForUs
@@valid2:
        mov     [DriveNo], bl
        shl     bx, 1
        mov     bx, [DataSegs+bx]
        mov     [ResDataSeg], bx
        or      bx, bx
        pop     bx
        jnz     CallForUs
        jmp     [DWORD OldInt2F]

; Special check for Find Next function - drive number in SDB.
; Under Windows95 we do not put the drive number in the SDB, so this
; cannot be used.
CheckFindNext:
        cmp     [Win95], 0
        jnz     CheckCDS                ; CDS check under Win95 instead
        cmp     [DosVersion], 7
        jae     CheckCDS                ; CDS check under DOS 7
        push    ds bx
        lds     bx, [pDTA]
        lds     bx, [bx]                ; DS:BX -> DTA
        mov     bl, [BYTE es:di]        ; SDB drive number
        and     bx, 3Fh                 ; Turn off network bit
        cmp     bl, MaxDrives
        jb      @@valid3
        pop     bx ds
        jmp     @@NotForUs
@@valid3:
        mov     [DriveNo], bl
        shl     bx, 1
        mov     bx, [DataSegs+bx]
        mov     [ResDataSeg], bx
        or      bx, bx
        pop     bx ds
        jnz     CallForUs
        jmp     [DWORD OldInt2F]

; Call is for our drive
CallForUs:
; Switch stack
        mov     [cs:SaveSP], sp
        mov     [cs:SaveSS], ss
        push    cs
        pop     ss
        mov     sp, OFFSET ResStack
        ASSUME  ss:SEG ResStack
; Transfer to the subfunction handler
        push    bx
        mov     bl, al
        xor     bh, bh
        shl     bl, 1
        mov     bx, [JmpTbl+bx]
        mov     [FuncAddr], bx
        pop     bx
        cmp     [FuncAddr], 0
        jz      Unsupported
        push    ds gs
        push    cs
        pop     ds
        mov     gs, [ResDataSeg]
        DEFASSUME
        call    [FuncAddr]
        pop     gs ds
        NOASSUME
        ASSUME  cs:ResCode
        lss     sp, [DWORD SaveSP]
        ret     2
Unsupported:
        lss     sp, [DWORD SaveSP]
        jmp     [DWORD OldInt2F] ; Function not supported, ignore it.

; Redirector installed check
NetInstallChk:
        mov     ax, 00FFh       ; Redirector installed.
        iret                    ; Return immediately
ENDP    Int2FHandler

;---------------------------------------------------------------------
; Detect presence of Windows95.
PROC    DetectWin95
        NOASSUME
        ASSUME  cs:ResCode
        pushad
        mov     [Win95], 0
        mov     ax, 1600h
        int     2fh
        cmp     al, 1           ; 0=nothing, 1=Windows/386
        jbe     @@Done
        cmp     al, 80h         ; XMS version 1 driver, no Windows
        je      @@Done
        cmp     al, 0FFh        ; Windows/386
        je      @@Done
        cmp     al, 4           ; Major version
        jb      @@Done
        mov     [Win95], 1
@@Done:
        popad
        ret
ENDP    DetectWin95

;---------------------------------------------------------------------
; Remove Directory
PROC    RmDir
        stc
        mov     ax, errPathNotFound
        ret
ENDP    RmDir

;---------------------------------------------------------------------
; Make Directory
PROC    MkDir
        stc
        mov     ax, errWriteProt
        ret
ENDP    MkDir

;---------------------------------------------------------------------
; Change Directory
PROC    ChDir   STDCALL
        LOCAL   @@CDSPointer:DWORD, @@Result
        DEFASSUME
        pushad
        push    es
        mov     [BufUsed], 0
        les     di, [pCurrCDS]
        mov     eax, [es:di]
        mov     [@@CDSPointer], eax
        les     di, [FN1]
        cmp     [BYTE es:di+3], 0       ; See if root directory
        je      @@Root
        cmp     [BYTE es:di+2], 0       ; See if root in DR-DOS
        je      @@Root
        call    FindFile
        jc      @@PathNotFound
; See if the entry is a subdirectory
        test    [Buf1+bx+03h], 10h      ; Mask out subdirectory attribute
        jz      @@PathNotFound
        call    NEAR ChkPathLength PASCAL, [FN1]
        jc      @@PathNotFound
; Extract the FNode pointer
        mov     ecx, [DWORD Buf1+bx+04h]
        mov     [CDFNode], ecx
; Set the CDS directory name to current directory.
        lds     si, [FN1]
        ASSUME  ds:NOTHING
        les     di, [@@CDSPointer]
        mov     cx, 67                  ; Length of CDS path string
        cld
@@MovePath:
        lodsb
        stosb
        or      al, al
        loopnz  @@MovePath
        clc
        jmp     @@Done

        DEFASSUME
@@Root: mov     eax, [RootFNode]
        mov     [CDFNode], eax
        lds     bx, [@@CDSPointer]
        ASSUME  ds:NOTHING
        mov     [BYTE bx+3], 0      ; Puts 0 after X:\ in CDS Filename
        clc
        jmp     @@Done
@@PathNotFound:
        stc
        mov     [@@Result], errPathNotFound
@@Done:
        pop     es
        popad
        jnc     @@Exit
        mov     ax, [@@Result]
@@Exit:
        ret
ENDP    ChDir

;---------------------------------------------------------------------
; Close File
PROC    Close
        DEFASSUME
        push    ax bx
        mov     ax, 1208h
        int     2Fh             ; Decrease handle count in SFT
        cmp     ax, 01h         ; Last handle closed?
        jne     @@1
; Clear the SFT
        mov     [WORD es:di], 0     ; Number of references
@@1:
        pop     bx ax
        ret
ENDP    Close

;---------------------------------------------------------------------
; Commit File
PROC    Commit
        stc
        mov     ax, errWriteProt
        ret
ENDP    Commit

;---------------------------------------------------------------------
; Read from File
PROC    Read    STDCALL
        DEFASSUME
        LOCAL   @@OrigBytes, @@Bytes, @@RelSect:DWORD, @@SecOfs, @@DTABuf:DWORD
        LOCAL   @@Result, @@SFTOfs, @@FNode:DWORD
        pushad
        push    es fs
        push    es
        pop     fs
        ASSUME  fs:NOTHING
        mov     [@@SFTOfs], di
        mov     [BufUsed], 0
        mov     [@@Bytes], cx
        mov     [@@OrigBytes], 0
        les     bx, [SDA]
        les     bx, [DWORD es:bx+0Ch]       ; DTA Pointer
        mov     [WORD LOW @@DTABuf], bx
        mov     [WORD HIGH @@DTABuf], es
        mov     eax, [fs:di+19h]
        mov     [@@FNode], eax
        push    cs
        pop     es
        ASSUME  es:ResCode
; Adjust bytes to read if necessary.
        movzx   eax, [@@Bytes]           ; Bytes to read
        mov     edx, [fs:di+15h]        ; File pos
        mov     ecx, [fs:di+11h]        ; File size
        cmp     edx, ecx
        jae     @@Succeed               ; Beyond EOF
        add     eax, edx
        cmp     eax, ecx                ; Compare w file size
        jna     @@1
        sub     eax, ecx
        sub     [@@Bytes], ax           ; Actual bytes to read
@@1:    mov     ax, [@@Bytes]
        mov     [@@OrigBytes], ax
        mov     eax, edx                ; File pos
        and     dx, 511
        mov     [@@SecOfs], dx
        shr     eax, 9
        mov     [@@RelSect], eax
        cmp     [@@SecOfs], 0
        je      @@WholeSectors
; Read sector into Buf2 and copy bytes to DTA
        call    NEAR ReadFileSector, [@@RelSect], 1, [@@FNode], ds (OFFSET Buf2)
        jc      @@ReadError
        inc     [@@RelSect]
        mov     cx, 512
        sub     cx, [@@SecOfs]            ; Bytes to read from this sector
        cmp     [@@Bytes], cx
        ja      @@2
        mov     cx, [@@Bytes]
@@2:
        mov     si, OFFSET Buf2
        add     si, [@@SecOfs]
        les     di, [@@DTABuf]
        ASSUME  es:NOTHING
        push    cx
        rep movsb                       ; Transfer bytes to DTA Buffer
        pop     cx
        add     [WORD @@DTABuf], cx     ; Increase user buffer offset
        sub     [@@Bytes], cx           ; Decrease bytes left to read
; Read sectors into DTA
@@WholeSectors:
        cmp     [@@Bytes], 512
        jb      @@LastSector            ; Done if no more bytes
        movzx   eax, [@@Bytes]
        shr     ax, 9                   ; Sectors to read
        call    NEAR ReadFileSector, [@@RelSect], ax, [@@FNode], [@@DTABuf]
        jc      @@ReadError
        add     [@@RelSect], eax
        shl     ax, 9                   ; Bytes read
        add     [WORD @@DTABuf], ax
        sub     [@@Bytes], ax
; Read the last sector into Buf2 and copy part of it to DTA
@@LastSector:
        cmp     [@@Bytes], 0
        jz      @@Succeed               ; No bytes left to read
        call    NEAR ReadFileSector, [@@RelSect], 1, [@@FNode], ds (OFFSET Buf2)
        jc      @@ReadError
        mov     cx, [@@Bytes]
        mov     si, OFFSET Buf2
        les     di, [@@DTABuf]
        ASSUME  es:NOTHING
        rep movsb                       ; Transfer bytes to user buffer
; Done!
@@Succeed:
        movzx   ecx, [@@OrigBytes]
        mov     bx, [@@SFTOfs]
        add     [fs:bx+15h], ecx
        mov     [@@Result], cx
        clc
        jmp     @@Done
@@SectorNotFound:
@@ReadError:
        mov     [@@Result], errSectorNotFound
        stc
@@Done:
        pop     fs es
        ASSUME  es:NOTHING,fs:NOTHING
        popad
        jc      @@Fail
        mov     cx, [@@Result]
        jmp     @@Exit
@@Fail:
        mov     ax, [@@Result]
@@Exit:
        ret
ENDP    Read

;---------------------------------------------------------------------
; Write to File
PROC    Write   STDCALL
        stc
        mov     ax, errWriteProt
        ret
ENDP    Write

;---------------------------------------------------------------------
; Lock Region of File
PROC    LockRegion
        clc
        ret
ENDP    LockRegion

;---------------------------------------------------------------------
; Unlock Region of File
PROC    UnlockRegion
        clc
        ret
ENDP    UnlockRegion

;---------------------------------------------------------------------
; Get Disk Space
PROC    DiskFree
lim2gb  EQU 0ffffh*64
        DEFASSUME
        push    esi edi ebp
        mov     ah, [MediaID]
        mov     al, 1                   ; Sectors per cluster
        mov     esi, [TotalSectors]     ; Number of clusters
        mov     edi, [FreeSectors]
        mov     cx, 512                 ; Bytes/sector
        cmp     [Limit2GB], 1
        jne     @@1
        mov     ebp, lim2gb
        cmp     esi, ebp
        jbe     @@1
        mov     esi, ebp
        cmp     edi, ebp
        jbe     @@1
        mov     edi, ebp
; Adjust clusters so that value is 16-bit
@@1:
        cmp     esi, 0FFFFh             ; Cluster count fits in BX?
        jbe     @@3
        shr     esi, 1                  ; Divide cluster count by 2
        shr     edi, 1
        cmp     al, 128
        je      @@2
        shl     al, 1                   ; Multiply sectors per cluster by 2
        jmp     @@1
@@2:
        shl     cx, 1                   ; double sector size
        jmp     @@1
@@3:
        clc
        mov     bx, si
        mov     dx, di
        pop     ebp edi esi
        ret
ENDP    DiskFree

;---------------------------------------------------------------------
; Set File Attributes
PROC    SetAttrib STDCALL
        stc
        mov     ax, errWriteProt
        ret
ENDP    SetAttrib

;---------------------------------------------------------------------
; Get File Attributes
PROC    GetAttrib STDCALL
        DEFASSUME
        LOCAL   @@Result
        pushad
        push    es
        mov     [BufUsed], 0
        les     di, [FN1]
        call    FindFile
        jc      @@FileNotFound
        mov     al, [Buf1+bx+03h]       ; Extract attributes
        and     al, 10111111b           ; Mask out 8.3 filename bit
        xor     ah, ah
        test    al, 10h                 ; Test if subdir
        jz      @@SubDirOK
        call    NEAR ChkPathLength PASCAL, [FN1]
        jnc     @@SubDirOk
        and     al, NOT 10h             ; Turn off subdir attribute
@@SubDirOk:
        mov     [@@Result], ax
        clc
        jmp     @@Done

@@FileNotFound:
        mov     [@@Result], errFileNotFound
        stc
@@Done:
        pop     es
        popad
        mov     ax, [@@Result]
        ret
ENDP    GetAttrib

;---------------------------------------------------------------------
; Rename File
PROC    Rename
        stc
        mov     ax, errWriteProt
        ret
ENDP    Rename

;---------------------------------------------------------------------
; Delete File
PROC    Delete
        stc
        mov     ax, errWriteProt
        ret
ENDP    Delete

;---------------------------------------------------------------------
; Open File
PROC    Open    STDCALL
        DEFASSUME
        LOCAL   @@OpenMode:BYTE, @@Result:WORD
        pushad
        push    es fs
        mov     ax, es
        mov     fs, ax
        mov     [BufUsed], 0
        mov     si, di
        les     di, [AccMode]
        mov     al, [es:di]             ; File access mode/sharing
        cmp     [Novell], 0
        jnz     @@OpenModeOk            ; Ignore access mode under Novell DOS
        test    al, 7
        jnz     @@errAccessDenied
@@OpenModeOk:
        mov     [@@OpenMode], al
@@FindFile:
        mov     di, [WORD LOW FN1]      ; First filename buffer
        call    FindFile
        jnc     @@Found
        mov     [@@Result], errFileNotFound
        jmp     @@Fail
@@Found:
        mov     al, [Buf1+bx+03h]       ; Attributes
        and     al, 10h                 ; Test subdir attribute
        jnz     @@errAccessDenied

; Set SFT fields
        mov     al, [@@OpenMode]
        and     ax, 007Fh
        mov     [fs:si+02h], ax
        mov     al, [Buf1+bx+03h]       ; Attributes
        and     al, 10111111b           ; Mask out 8.3 filename bit
        mov     [fs:si+04h], al
        mov     ax, 8040h               ; Device info word
        or      al, [DriveNo]           ; Drive number
        mov     [fs:si+05h], ax
        and     [DWORD fs:si+07h], 0    ; Device driver pointer
        and     [WORD fs:si+0Bh], 0     ; Cluster #, local files only
        mov     eax, [DWORD Buf1+bx+08h]; Timestamp
        call    Unix2DosTime
        mov     [fs:si+0Dh], eax
        mov     eax, [DWORD Buf1+bx+0Ch] ; File size
        mov     [fs:si+11h], eax
        and     [DWORD fs:si+15h], 0    ; Current offset in file
        mov     eax, [DWORD Buf1+bx+04h]; FNode sector #
        mov     [fs:si+19h], eax        ; Save in REDIRIFS field
; Convert filename to FCB format (11 bytes, no dot, blank-padded)
; Scan to the terminating 0.
        cld
        xor     al, al
        mov     cx, -1
        repne scasb
; Scan back to the last \
        std
        mov     al, '\'
        mov     cx, -1
        repne   scasb
        add     di, 2

        xchg    si, di
        push    es
        push    fs
        pop     es
        pop     ds
        ASSUME  ds:NOTHING
        add     di, 20h

        mov     cx, 11
        mov     al, ' '
        cld
        rep stosb                       ; Clear the field
        sub     di, 11
        lea     dx, [di+8]              ; Extension part
@@Fn1:  lodsb
        or      al, al
        je      @@Succeed
        cmp     al, '.'
        jne     @@Fn2
        mov     di, dx                  ; Move on to extension field if '.'
        jmp     @@Fn1
@@Fn2:
        stosb
        jmp     @@Fn1

@@errAccessDenied:
        mov     [@@Result], errAccessDenied
        jmp     @@Fail
@@Succeed:
        clc
        jmp     @@Done
@@Fail: stc
@@Done: pop     fs es
        popad
        jnc     @@Exit
        mov     ax, [@@Result]
@@Exit:
        ret
ENDP    Open

;---------------------------------------------------------------------
; Create File
PROC    Create
        stc
        mov     ax, errWriteProt
        ret
ENDP    Create

;---------------------------------------------------------------------
; Find First Matching File
PROC    FindFirst STDCALL
        DEFASSUME
        LOCAL   @@Result, @@SrchTmplPos, @@DirFNode:DWORD, @@Attr:BYTE
        LOCAL   @@LongPath:BYTE, @@DTA:DWORD
        pushad
        push    es
        mov     ax, cs
; Move the filename from SDA First Filename buffer to FNameBuf2
        mov     [BufUsed], 0
        mov     [@@LongPath], 0         ; Set if pathname is long
        lds     si, [pDTA]
        ASSUME  ds:NOTHING
        mov     ecx, [si]
        mov     [@@DTA], ecx            ; Address of DTA
        lds     si, [FN1]
        mov     es, ax
        ASSUME  es:ResCode
        mov     di, OFFSET FNameBuf2
        mov     cx, 32
        cld
        rep movsd
        mov     ds, ax
        ASSUME  ds:ResCode
; Find the last \ and replace it with 0
        xor     al, al
        mov     cx, 128
        mov     di, OFFSET FNameBuf2
        repne scasb
        jne     @@PathNotFound
        dec     di
        mov     al, '\'
        sub     cx, 128
        neg     cx
        std
        repne scasb
        jne     @@PathNotFound
        inc     di
        mov     [BYTE di], 0
        inc     di
        mov     [@@SrchTmplPos], di       ; Where the search template starts
        mov     ax, di
        sub     ax, OFFSET FNameBuf2
        cmp     ax, MaxPathLength-8     ; Long path?
        seta    [@@LongPath]
        ror     [@@LongPath], 1         ; Move flag to high bit
; Find the directory
        cmp     di, OFFSET FNameBuf2+3  ; See if it's in the root dir.
        jne     @@FindDir
        mov     ecx, [RootFNode]
        mov     [@@DirFNode], ecx
        jmp     @@DirOk
@@FindDir:
        mov     di, OFFSET FNameBuf2
        call    FindFile
        jc      @@PathNotFound
; Check that it's really a directory.
        test    [BYTE Buf1+bx+03], 10h
        jz      @@PathNotFound
        mov     ecx, [DWORD Buf1+bx+04h]    ; FNode of directory
        mov     [@@DirFNode], ecx

@@DirOk:
        les     di, [SrchAttr]
        ASSUME  es:NOTHING
        mov     al, [es:di]             ; Search attribute
        mov     [@@Attr], al
; Initialize the search data block in the DTA.
; The "drive number" byte does not seem to work as specified in Windows 95.
; We later overwrite this with a magic value that seems to work. The reason
; for this behaviour is unknown.
        cld
        les     di, [@@DTA]
        mov     al, [DriveNo]           ; Drive number
        or      al, 80h                 ; Set bit 7 for remote drive
        stosb
; Search template - 11 bytes, padded with spaces.
        mov     cx, 11
        mov     al, ' '
        rep stosb                       ; Clear the field
        sub     di, 11
        lea     dx, [di+8]              ; Extension part
        mov     si, [@@SrchTmplPos]
@@1:    lodsb
        or      al, al
        je      @@TemplateDone
        cmp     al, '.'
        jne     @@2
        mov     di, dx                  ; Move on to extension field if '.'
        jmp     @@1
@@2:
        stosb
        jmp     @@1

@@TemplateDone:
        mov     di, dx
        add     di, 3
        mov     al, [@@Attr]
        stosb

; Read the directory FNode
        call    NEAR ReadSector, [@@DirFNode], 1, ds (OFFSET Buf1)
        jc      @@NoMoreFiles
        mov     eax, [DWORD Buf1+48h] ; Starting sector
        les     bx, [@@DTA]
        mov     [es:bx+0Dh], eax     ; Save in SDB
        xor     al, al
        mov     ah, [@@LongPath]
        mov     [WORD es:bx+11h], ax ; Offset of last entry + long path flag

        test    [@@Attr], 08h                 ; Volume label?
        jz      @@RegularFile
; Volume label
        push    ds
        pop     es
        ASSUME  es:ResCode
        mov     ds, [ResDataSeg]
        ASSUME  ds:ResData
; Move volume label to FNameBuf
        mov     di, OFFSET FNameBuf
        mov     cx, 8
        mov     si, OFFSET Volabel
        rep movsb
        mov     al, '.'
        stosb
        mov     cx, 3
        rep movsb
        push    cs
        pop     ds
        ASSUME  ds:ResCode

        mov     dx, OFFSET FNameBuf
        mov     bx, [@@SrchTmplPos]
        call    Match
        jnc     @@DoVolLabel
        cmp     [@@Attr], 08h           ; Only volume label?
        je      @@NoMoreFiles
        jmp     @@RegularFile
@@DoVolLabel:
; Set the directory entry for found vol. label.
        les     di, [@@DTA]
        ASSUME  es:NOTHING
        add     di, 15h                 ; Directory entry for found file
        mov     cx, 11
        mov     al, ' '
        cld
        rep stosb
        sub     di, 11
        lea     dx, [di+11]
        mov     cx, 11
        mov     si, OFFSET Volabel
        mov     ds, [ResDataSeg]
        ASSUME  ds:ResData
@@MoveVolabel:
        lodsb
        or      al, al
        jz      @@VolabelDone
        stosb
        loop    @@MoveVolabel
@@VolabelDone:
        push    cs
        pop     ds
        ASSUME  ds:ResCode
        mov     di, dx
        mov     al, 08h
        stosb                           ; Attributes
        add     di, 10
        xor     eax, eax
        stosd                           ; Time and date
        stosw                           ; Starting cluster
        stosd                           ; File size
        clc
        jmp     @@Done

@@RegularFile:
; Call FindNext to do the actual directory search
        les     di, [@@DTA]
        call    FindNext
        mov     [@@Result], ax
        jmp     @@Done

@@PathNotFound:
        stc
        mov     [@@Result], errPathNotFound
        jmp     @@Done
@@NoMoreFiles:
        stc
        mov     [@@Result], errNoMoreFiles
        jmp     @@Done
@@Done:
        pop     es
        popad
        jnc     @@Exit
        mov     ax, [@@Result]
@@Exit:
        ret
ENDP    FindFirst

;---------------------------------------------------------------------
; Find Next Matching File
; Use of reserved or unused SDB fields:
; Offset Size   Function
; 0Dh    DWORD  First sector of the directory block last searched.
; 11h    WORD   Bit 15   : Set if subdirs are not to be returned (long paths)
;               Bits 0-14: Offset into directory block of the last entry
;               examined, or 0=no last entry, 1=only "." entry returned.
PROC    FindNext STDCALL
        DEFASSUME
        LOCAL   @@Result, @@DirBlock:DWORD, @@LastEntry, @@Root:BYTE, @@NoSubDirs:BYTE
        LOCAL   @@FileAttr:BYTE, @@FileSize:DWORD, @@DTA:DWORD
        pushad
        push    es
        cld
        mov     [@@Root], 0             ; Flag is set if root dir.
        les     bx, [pDTA]
        mov     eax, [es:bx]
        mov     [@@DTA], eax            ; Address of DTA
        les     bx, [es:bx]             ; ES:BX -> DTA
        mov     eax, [es:bx+0Dh]        ; SDB, directory block.
        mov     [@@DirBlock], eax
        mov     ax, [es:bx+11h]         ; SDB, offset of last entry
        mov     [@@NoSubDirs], ah       ; Copy to subdirectory flag
        and     [@@NoSubDirs], 80h
        and     ah, 7Fh
        mov     [@@LastEntry], ax
@@Search:
        cmp     [BufUsed], 0
        jnz     @@GotDirBlock
        call    NEAR ReadSector, [@@DirBlock], 4, ds (OFFSET Buf1)
        jc      @@NoMoreFiles
@@GotDirBlock:
        mov     [BufUsed], 0
; See if we're in the root dir.
        mov     eax, [DWORD Buf1+0Ch]
        cmp     eax, [RootFNode]
        jne     @@TransferTemplate
        mov     [@@Root], 1             ; Set root dir flag.
; Transfer search template to ASCIIZ format in FNameBuf2
@@TransferTemplate:
        push    ds
        pop     es
        lds     si, [@@DTA]
        ASSUME  ds:NOTHING, es:ResCode
        inc     si                              ; SDB Search template
        mov     di, OFFSET FNameBuf2
        mov     cx, 8
@@MoveTmpl1:
        lodsb
        cmp     al, ' '                         ; Go to extension if blank
        je      @@MoveTmplExt
        stosb
        loop    @@MoveTmpl1
        inc     si
@@MoveTmplExt:
        add     si, cx                          ; Extension field in template
        dec     si
        mov     cx, 3
        mov     al, '.'
        stosb
@@MoveTmpl2:
        lodsb
        cmp     al, ' '
        je      @@MoveTmplDone
        stosb
        loop    @@MoveTmpl2
@@MoveTmplDone:
        xor     al, al                          ; Zero terminate
        stosb
        mov     ax, cs
        mov     ds, ax
        mov     es, ax
        ASSUME  ds:ResCode, es:NOTHING

        mov     bx, [@@LastEntry]
        cmp     bx, 1
        ja      @@NextEntry
        je      @@DotDot
        mov     bx, 14h                 ; Offset of first dir. entry
        jmp     @@DoEntry

@@NextEntry:
        test    [BYTE Buf1+bx+02h], 08h ; Last entry in block?
        jz      @@MoveToNextEntry
; Read higher level directory block.
        call    NEAR ReadSector, [DWORD Buf1+0Ch], 4, ds (OFFSET Buf1)
        jc      @@NoMoreFiles
; Check that it's a directory sector, not the FNode.
        cmp     [BYTE Buf1+03h], 0F7h      ; FNode signature
        je      @@NoMoreFiles
; Go through this directory block to find the entry that we came from.
        mov     edx, [@@DirBlock]
        mov     bx, 14h
@@SrchParent:
        test    [Buf1+bx+02h], 04h      ; Entry has B tree pointer?
        jz      @@NotParent
        mov     si, bx
        add     si, [WORD Buf1+bx]
        cmp     [DWORD Buf1+si-4], edx
        je      @@FoundParent
@@NotParent:
        test    [Buf1+bx+02h], 08h      ; Last entry in block?
        jnz     @@NoMoreFiles
        add     bx, [WORD Buf1+bx]
        jmp     @@SrchParent

@@FoundParent:
        mov     eax, [DWORD Buf1+10h]               ; Sector number
        mov     [@@DirBlock], eax
        mov     [@@LastEntry], bx
        jmp     @@NoBTree

@@MoveToNextEntry:
        add     bx, [WORD Buf1+bx]  ; Move to next entry
@@DoEntry:
; Check if entry has a B Tree pointer
        mov     [@@LastEntry], bx
        test    [BYTE Buf1+bx+02h], 04h
        jz      @@NoBTree
; Go down the branch
        add     bx, [WORD Buf1+bx]
        mov     eax, [DWORD Buf1+bx-04h]    ; B Tree pointer
        mov     [@@DirBlock], eax
        mov     [@@LastEntry], 0
        jmp     @@Search

; No B Tree, check this entry for a match.
@@NoBTree:
        test    [BYTE Buf1+bx+02h], 08h     ; Last entry in block?
        jnz     @@NextEntry
        les     di, [@@DTA]
        ASSUME  es:NOTHING
; Match file attributes with AT MOST the specified combination of srch attrib.
        mov     ah, [Buf1+bx+03h]               ; File attributes
        cmp     [@@NoSubDirs], 0                ; Allowed to return subdirs?
        jz      @@SubDirOk
        and     ah, NOT 10h                     ; Subdirectory returned as file
@@SubDirOk:
        mov     al, [es:di+0Ch]                 ; Search attributes
        not     al
        and     al, ah                          ; Compare attributes
        and     al, 10011110b                   ; Ignore bits 0, 5 and 6
        jnz     @@NextEntry
        push    cs
        pop     es
        ASSUME  es:ResCode
; Check if "." entry.
        test    [BYTE Buf1+bx+02h], 01h
        jz      @@NoDot
        cmp     [@@Root], 0                     ; No "." in root.
        jnz     @@NoDot
; Return . file if it matches filespec.
        mov     si, OFFSET FNameBuf2
        call    MatchDot
        jc      @@NoDot
        les     di, [@@DTA]
        ASSUME  es:NOTHING
; Write directory entry for . file
        mov     eax, [@@DirBlock]
        cmp     [Win95], 0
        jz      @@Win1
        mov     [BYTE es:di], 0d2h              ; Win95 kludge
@@Win1:
        mov     [es:di+0Dh], eax                ; Save current dir block
        mov     ax, 01h                         ; Flag "." file returned
        or      ah, [@@NoSubDirs]               ; Keep subdir flag bit
        mov     [es:di+11h], ax                 ; Save offset of found entry
        add     di, 15h                         ; Point to found file field
; Transfer the filename
        mov     eax, '   .'
        stosd
        mov     eax, '    '
        stosd
        stosw
        stosb
        mov     al, [Buf1+14h+03h]               ; Attributes
        stosb
        add     di, 10                           ; Reserved field
        lea     si, [Buf1+14h+08h]               ; Timestamp field
        lodsd
        call    Unix2DosTime
        stosd
        xor     ax, ax
        stosw                                   ; Cluster #
        movsd                                   ; File size
        mov     [BufUsed], 1
        clc
        jmp     @@Done

; Return ".." if attributes match
@@DotDot:
        mov     bx, 14h
        mov     si, OFFSET FNameBuf2
        call    MatchDotDot
        jc      @@NextEntry
        les     di, [@@DTA]
        ASSUME  es:NOTHING
; Match file attributes with AT MOST the specified combination of srch attrib.
        mov     al, [es:di+0Ch]                 ; Search attributes
        not     al
        and     al, [Buf1+14h+03h]              ; File attributes
        and     al, 10011110b                   ; Ignore bits 0, 5 and 6
        jnz     @@NextEntry
; Write directory entry for .. file
        mov     eax, [@@DirBlock]
        cmp     [Win95], 0
        jz      @@Win2
        mov     [BYTE es:di], 0d2h              ; Win95 kludge
@@Win2:
        mov     [es:di+0Dh], eax                ; Save current dir block
        mov     ax, 14h                         ; First entry completed
        or      ah, [@@NoSubDirs]
        mov     [es:di+11h], ax                 ; Save offset of found entry
        add     di, 15h                         ; Point to found file field
; Transfer the filename
        mov     eax, '  ..'
        stosd
        mov     eax, '    '
        stosd
        stosw
        stosb
        mov     al, [Buf1+14h+03h]               ; Attributes
        stosb
        add     di, 10                           ; Reserved field
        lea     si, [Buf1+14h+08h]               ; Timestamp field
        lodsd
        call    Unix2DosTime
        stosd
        xor     ax, ax
        stosw                                   ; Cluster #
        movsd                                   ; File size
        mov     [BufUsed], 1
        clc
        jmp     @@Done

        ASSUME  es:ResCode
@@NoDot:
; See if filename is valid in DOS.
        test    [BYTE Buf1+bx+03h], 40h
        jz      @@MoveFileName
; Convert long filenames?
        cmp     [ConvertLong], 0
        je      @@NextEntry
; Convert long filename to valid name in FNameBuf
        lea     si, [Buf1+bx+1Fh]
        mov     di, OFFSET FNameBuf
        movzx   cx, [BYTE si-1]
        call    ConvertFilename
        jmp     @@MatchFilename

; Move filename to FNameBuf, converting it to upper case
@@MoveFilename:
        lea     si, [Buf1+bx+1Fh]
        mov     di, OFFSET FNameBuf
        mov     cl, [Buf1+bx+1Eh]
        xor     ch, ch
@@MoveChar:
        lodsb
        call    UpCase
        stosb
        loop    @@MoveChar
        xor     al, al
        stosb                                   ; Zero terminate
; Check if filename matches search template
@@MatchFilename:
        mov     dx, OFFSET FNameBuf             ; Filename
        push    bx
        mov     bx, OFFSET FNameBuf2            ; Search template, ASCIIZ
        call    Match
        pop     bx
        jc      @@NextEntry
; Match found - set directory entry
        les     di, [@@DTA]
        ASSUME  es:NOTHING
        cmp     [Win95], 0
        jz      @@Win3
        mov     [BYTE es:di], 0d2h              ; Win95 kludge
@@Win3:
        mov     eax, [@@DirBlock]
        mov     [es:di+0Dh], eax                ; Save current dir block
        mov     ax, [@@LastEntry]
        or      ah, [@@NoSubDirs]
        mov     [es:di+11h], ax                 ; Save offset of found entry
        add     di, 15h                         ; Point to found file field
; Pad with spaces.
        push    di
        mov     cx, 11
        mov     al, ' '
        rep stosb
        pop     di
; Transfer the filename
        mov     si, OFFSET FNameBuf
        mov     cx, 9
        lea     dx, [di+8]
@@Tf1:
        lodsb
        or      al, al
        je      @@TfDone
        cmp     al, '.'
        je      @@TfExt
        stosb
        loop    @@Tf1

; Transfer the extension
@@TfExt:
        mov     cx, 3
        mov     di, dx
@@TfExt1:
        lodsb
        call    UpCase
        or      al, al
        je      @@TfDone
        stosb
        loop    @@TfExt1
@@TfDone:
        mov     di, dx
        add     di, 3
        mov     al, [Buf1+bx+03h]               ; Attributes
        and     al, 10111111b                   ; Mask out 8.3 filename bit
        cmp     [@@NoSubDirs], 0                ; Allowed to return subdir?
        jz      @@AttrOk
        and     al, NOT 10h                     ; Return subdir as file
@@AttrOk:
        stosb
        add     di, 10                          ; Reserved field
        lea     si, [Buf1+bx+08h]               ; Timestamp field
        lodsd
        call    Unix2DosTime
        stosd
        xor     ax, ax
        stosw                                   ; Cluster #
        movsd                                   ; File size
        mov     [BufUsed], 1
        clc
        jmp     @@Done

@@NoMoreFiles:
        stc
        mov     [@@Result], errNoMoreFiles
@@Done:
        pop     es
        popad
        jnc     @@Exit
        mov     ax, [@@Result]
@@Exit:
        ret
ENDP    FindNext

;---------------------------------------------------------------------
; Seek from End of File
; NOTE Seems like this function never gets called! DOS changes the
; file pointers itself.
PROC    Seek    STDCALL
        DEFASSUME
        LOCAL   @@RetValue:DWORD
        pushad
        cmp     [WORD es:di], 0     ; Open files count
        jne     @@SFTOk
        mov     [WORD @@RetValue], errInvalidHandle
        stc
        jmp     @@Done
@@SFTOk:
        shl     ecx, 16
        mov     cx, dx          ; Offset from end of file in ECX
        mov     eax, [es:di+11h] ; File size

        sub     eax, ecx
        jnc     @@1
        xor     eax, eax
@@1:
        mov     [es:di+15h], eax ; New file pos.
        mov     [@@RetValue], eax
        clc
@@Done:
        popad
        jnc     @@2
        mov     ax, [WORD @@RetValue]
        jmp     @@Exit
@@2:    mov     ax, [WORD @@RetValue]
        mov     dx, [WORD @@RetValue+2]
@@Exit:
        ret
ENDP    Seek

;---------------------------------------------------------------------
; Extended Open File
; Never gets called under DR-DOS 6.0 which doesn't support extended open.
PROC    ExtOpen STDCALL
        DEFASSUME
        LOCAL   @@OpenMode, @@RetValue
        pushad
        push    fs
        lfs     si, [ExtOpenMode]
        mov     ax, [fs:si]                     ; Open mode
        mov     bx, [WORD LOW AccMode]          ; Exchange with normal open mode
        xchg    [fs:bx], ax
        mov     [@@OpenMode], ax
        call    Open
        mov     [@@RetValue], ax
        mov     ax, [@@OpenMode]
        mov     [fs:bx], ax
        jc      @@Fail
        mov     [@@RetValue], 01h                 ; File opened
        jmp     @@Done

@@Fail:
@@Done:
        pop     fs
        popad
        jnc     @@Succeeded
        mov     ax, [@@RetValue]
        jmp     @@Exit
@@Succeeded:
        mov     cx, [@@RetValue]
@@Exit:
        ret
ENDP    ExtOpen

;------------------------------------------------------------------------
; Convert logical sector number to Cyl/Head/Sect in CX, DX, and AL
; as passed to INT 13h. Logical sector passed in ECX.
PROC    LogToCHS
        add     ecx, [LBAStart]
        movzx   ax, [nSecs]
        mul     [nHeads]
        mov     bx, ax          ; Sectors/track * heads
        mov     ax, cx
        mov     edx, ecx
        shr     edx, 16         ; DX:AX = logical sector #
        div     bx
        push    ax              ; Cylinder
        mov     ax, dx
        div     [nSecs]
        movzx   dx, al          ; Head
        mov     cl, ah          ; Sector
        inc     cl
        pop     ax              ; Cylinder
        mov     dh, dl          ; Head
        mov     ch, al
        xor     al, al
        shr     ax, 2
        or      cl, al          ; bits 8 and 9 of cyl. number go here
        xor     al, al
        shr     ax, 2
        or      dh, al          ; bits 10 and 11 of cyl (BIOS extension)
        ret
ENDP    LogToCHS

;------------------------------------------------------------------------
; Read sectors from disk.
PROC    DiskRead PASCAL
        DEFASSUME
        ARG     @@Sector:DWORD, @@NumSectors:WORD, @@Dest:DWORD
        LOCAL   @@CurrSector:DWORD, @@SectorsToRead:WORD, @@CurrDest:DWORD
        pushad
        push    es

        cmp     [AccessMethod], Method_ASPI
        je      @@DiskRead_ASPI

        mov     eax, [@@Sector]
        mov     [@@CurrSector], eax
        add     eax, [LBAstart]
        mov     [DWORD LOW DiskAddrPkt.Sector], eax
        mov     ax, [@@NumSectors]
        mov     [@@SectorsToRead], ax
        mov     [DiskAddrPkt.Count], ax
        mov     eax, [@@Dest]
        mov     [@@CurrDest], eax
        mov     [DiskAddrPkt.Buffer], eax
        cmp     [AccessMethod], Method_Ext
        jne     @@ReadCHS
; Read using LBA addressing
        mov     dl, [PhysDrv]
        mov     si, OFFSET DiskAddrPkt
        mov     ah, 42h
        int     13h
        jc      @@Done
        jmp     @@CopyToCache

; Read using ASPI manager
@@DiskRead_ASPI:
        mov     si, OFFSET SCSIRequestBlock     ; ds=ResCode
        mov     ax, [@@NumSectors]
        xchg    al, ah
        mov     [si+SCSIRequestBlockStruct.ccb_07_count       ], ax
        add     ax, ax ; @@NumSectors shl 9=@@NumSectors*512
        movzx   eax, ax
        mov     [si+SCSIRequestBlockStruct.data_allocation_length], eax
        mov     eax,[@@Dest]
        mov     [si+SCSIRequestBlockStruct.data_buffer_pointer], eax
        mov     eax, [@@Sector]
        add     eax, [LBAstart]
        xchg    al, ah  ; 1 2 4 3
        rol     eax, 16 ; 4 3 1 2
        xchg    al, ah  ; 4 3 2 1
        mov     [si+SCSIRequestBlockStruct.ccb_02_lba         ], eax

        mov [si+SCSIRequestBlockStruct.request_number         ], 2
        mov [si+SCSIRequestBlockStruct.request_status         ], 0
        mov [si+SCSIRequestBlockStruct.host_adapter_id        ], 0 ;?
        mov [si+SCSIRequestBlockStruct.request_flags          ], 8
        mov al, [PhysDrv]
        mov [si+SCSIRequestBlockStruct.target_id              ], al
        mov [si+SCSIRequestBlockStruct.logical_unit           ], 0 ;?
        mov [si+SCSIRequestBlockStruct.sense_allocation_length], 0
        mov [si+SCSIRequestBlockStruct.CDB_length             ], 10
        mov [si+SCSIRequestBlockStruct.host_adapter_status    ], 0ffh
        mov [si+SCSIRequestBlockStruct.target_status          ], 0ffh

        push    ds
        push    si
        call    [SCSIMGR]
        add     sp,2*2
  @@wait_scsi:
        cmp     [si+SCSIRequestBlockStruct.request_status], 0
        je      @@wait_scsi

        cmp     [si+SCSIRequestBlockStruct.request_status], 1
        jne     @@Fail

        cmp     [si+SCSIRequestBlockStruct.target_status], 0
        je      @@CopyToCache

  @@Fail:
        stc
        jmp     @@Done

; Read using CHS addressing
@@ReadCHS:
        mov     si, 3
@@ReadCHSRetry:
        mov     ecx, [@@CurrSector]
        call    LogToCHS
        mov     al, [BYTE @@NumSectors]
        test    [PhysDrv], 00080h ; Floppy ?
        jz      @@NoMultitrack
        cmp     [Multitrack], 0
        jnz     @@DoInt13
; Multitrack operations not supported
@@NoMultitrack:
        push    cx
        and     cl, 3Fh
        mov     al, [nSecs]
        sub     al, cl
        inc     al              ; Sectors left on this track
        xor     ah, ah
        cmp     ax, [@@SectorsToRead]
        jbe     @@3
        mov     al, [BYTE @@SectorsToRead] ; Read SectorsToRead sectors
@@3:    pop     cx
@@DoInt13:
        mov     dl, [PhysDrv]
        mov     ah, 02h
        les     bx, [@@CurrDest]
        push    ax
        int     13h
        pop     ax
        jnc     @@DoInt13nc

        mov     ah, 00h                ; Reset Disk
        int     13h
        dec     si
        jnz     @@ReadCHSRetry
        stc
        jmp     @@Done

@@DoInt13nc:
        and     eax, 0FFh
        les     di, [@@CurrDest]        ; Buffer
        movzx   cx, al                  ; Number of sectors
        mov     edx, [@@CurrSector]     ; First sector
        add     [@@CurrSector], eax
        sub     [@@SectorsToRead], ax
        push    dx
        mov     dx, 200h
        mul     dx
        pop     dx
        add     [WORD @@CurrDest], ax
; Check if any more sectors to read
@@SectorsLeft:
        cmp     [@@SectorsToRead], 0
        jnz     @@ReadCHS
; Copy read sectors to cache.
@@CopyToCache:
        cmp     [CacheOn], 0
        jz      @@Done
        cmp     [XMSError], 0
        jnz     @@Done
        mov     edx, [@@Sector]
        les     di, [@@Dest]
        mov     cx, [@@NumSectors]
@@CacheLoop:
        call    GetFreeCacheEntry       ; Get free cache entry in BX.
        or      bx, bx
        sete    [XMSError]
        jz      @@Done
        push    cx
        mov     ecx, edx
        call    CacheInsert             ; Insert sector ECX at ES:DI at entry BX.
        pop     cx
        add     edx, 1
        add     di, 200h
        loop    @@CacheLoop
        clc
@@Done:
        pop     es
        popad
        ret
ENDP    DiskRead

;------------------------------------------------------------------------
; Read logical sectors into memory.
PROC    ReadSector PASCAL
        DEFASSUME
        ARG     @@Sector:DWORD, @@NumSectors:WORD, @@Dest:DWORD
        LOCAL   @@SectorsToRead
        pushad
        push    es
@@Start:
        mov     dx, [@@NumSectors]
        mov     [@@SectorsToRead], dx
        cmp     [CacheOn], 0
        jz      @@DiskRead
        cmp     [XMSError], 0
        jnz     @@DiskRead
@@SearchCache:
        mov     ecx, [@@Sector]
        call    SearchCache             ; Search for sector ECX in cache
        or      bx, bx
        jz      @@CacheMiss
; Cache hit. Move the sector to the destination.
@@CacheHit:
        les     di, [@@Dest]
        call    ReadCacheSector         ; Read sector in entry BX
        or      ax, ax
        sete    [XMSError]
        jz      @@DiskRead
        inc     [@@Sector]
        add     [WORD @@Dest], 200h
        sub     [@@NumSectors], 1               ; Sectors left to read?
        jnz     @@SearchCache
        jmp     @@Done

@@CacheMiss:
; Determine number of consecutive sectors to read from disk.
        mov     ecx, [@@Sector]
        mov     [@@SectorsToRead], 0
@@ConsecutiveSectors:
        inc     ecx
        inc     [@@SectorsToRead]
        mov     ax, [@@NumSectors]
        cmp     ax, [@@SectorsToRead]
        jbe     @@DiskRead
        push    ecx
        call    SearchCache
        pop     ecx
        or      bx, bx
        jz      @@ConsecutiveSectors

; Read sectors from disk.
@@DiskRead:
        movzx   eax, [@@SectorsToRead]
        call    NEAR DiskRead, [@@Sector], ax, [@@Dest]
        jc      @@Done
        add     [@@Sector], eax
        sub     [@@NumSectors], ax
        jz      @@Done          ; No sectors left to read
        mov     dx, 200h
        mul     dx
        add     [WORD @@Dest], ax
        jmp     @@Start

@@Done:
        pop     es
        popad
        ret
ENDP    ReadSector

;---------------------------------------------------------------------
; Inserts a sector into the cache at a free entry. Sector number ECX,
; entry number BX, memory address ES:DI. Returns CF=1 on XMS error.
PROC    CacheInsert STDCALL
        LOCAL   @@EntryNum, @@SectorAddr:DWORD, @@Sector:DWORD
        DEFASSUME
        pushad
        mov     si, OFFSET XMoveStruc
        mov     [WORD @@SectorAddr], di
        mov     [WORD @@SectorAddr+2], es
        mov     [@@Sector], ecx
        mov     [@@EntryNum], bx
; Store the sector number
        mov     eax, [@@Sector]
        mov     [DWORD EntryBuf], eax
        mov     [WORD EntryBuf+0Ch], gs         ; Data segment
; Insert the new sector at the head of the priority list.
; next[x] <- next[sentinel]
        mov     [XMoveStruc.Length], 2
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.SourceHandle], ax
        mov     [XMoveStruc.DestHandle], 0
        mov     [WORD XMoveStruc.DestOffset], OFFSET EntryBuf+0Ah
        mov     [WORD (XMoveStruc.DestOffset)+2], ds
        mov     [XMoveStruc.SourceOffset], 0Ah  ; Next field of sentinel
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; prev[next[sentinel]] <- x
        mov     ax, [WORD EntryBuf+0Ah]
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 08h
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.SourceHandle], 0
        mov     bx, [@@EntryNum]
        mov     [WORD EntryBuf+CacheEntrySize], bx
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+CacheEntrySize
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; next[sentinel] <- x
        mov     [XMoveStruc.DestOffset], 0Ah
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; prev[x] <- sentinel
        mov     [EntryBuf+08h], 0
; Compute hash function
        mov     eax, [@@Sector]
        mov     edx, eax
        shr     edx, 16         ; Sector in DX:AX
        div     [HashSize]      ; Hash slot in DX
        shl     edx, 1          ; Offset into hash table
        mov     edi, edx        ; Save offset
; Insert the entry at the head of the hash table
; next[x] <- head
        mov     ax, [hHashTable]
        mov     [XMoveStruc.SourceHandle], ax
        mov     [XMoveStruc.SourceOffset], edx
        mov     [XMoveStruc.DestHandle], 0
        mov     [WORD XMoveStruc.DestOffset], OFFSET EntryBuf+06h
        mov     [WORD (XMoveStruc.DestOffset)+2], ds
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; if head <> NIL then prev[head] <- x
        mov     ax, [WORD EntryBuf+06h]
        or      ax, ax
        jz      @@HashInsert2
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 04h         ; Prev
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+CacheEntrySize
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
@@HashInsert2:
; head <- x
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+CacheEntrySize
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     ax, [hHashTable]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.DestOffset], edi    ; Saved offset into hash table
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; prev[x] <- NIL
        mov     [WORD EntryBuf+04h], 0
; Write the entry
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     ax, [@@EntryNum]
        mov     dx, CacheEntrySize
        mul     dx
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf
        mov     [XMoveStruc.Length], CacheEntrySize
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError

; Copy the sector to the cache
        mov     [XMoveStruc.Length], 512
        mov     eax, [@@SectorAddr]
        mov     [XMoveStruc.SourceOffset], eax
        mov     [XMoveStruc.SourceHandle], 0
        mov     ax, [hCacheSectors]
        mov     [XMoveStruc.DestHandle], ax
        movzx   eax, [@@EntryNum]
        shl     eax, 9
        mov     [XMoveStruc.DestOffset], eax
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        clc
        jnz     @@Done

@@XMSError:
        mov     [XMSError], 1   ; Flag XMS error
        stc
@@Done: popad
        ret
ENDP    CacheInsert

;---------------------------------------------------------------------
; Searches for a sector in the cache. Sector number in ECX. Returns
; cache entry in BX. If sector is not in cache, BX=0000.
PROC    SearchCache STDCALL
        DEFASSUME
        LOCAL   @@Sector:DWORD
; Compute the hash function (sector number modulo slots in hash table)
        cmp     [XMSError], 0
        jnz     @@XMSError
        mov     [@@Sector], ecx
        mov     ax, cx
        mov     edx, ecx
        shr     edx, 16         ; Sector in DX:AX
        div     [HashSize]      ; Hash slot in DX
; Extract the linked list pointer from the hash table
        mov     [XMoveStruc.Length], 2
        mov     ax, [hHashTable]
        mov     [XMoveStruc.SourceHandle], ax
        shl     edx, 1
        mov     [XMoveStruc.SourceOffset], edx
        mov     [XMoveStruc.DestHandle], 0
        mov     [WORD XMoveStruc.DestOffset], OFFSET EntryBuf
        mov     [WORD (XMoveStruc.DestOffset)+2], ds
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
        mov     bx, [WORD EntryBuf]
; Walk the linked list and search for the sector
; Entry format: Offset  Size    Descr
;               00h     DWORD   Sector number
;               04h     WORD    Prev entry in hash chain
;               06h     WORD    Next entry in hash chain
;               08h     WORD    Prev entry in priority list
;               0Ah     WORD    Next entry in priority list
;               0Ch     WORD    ResData segment of owner drive
        mov     [XMoveStruc.Length], CacheEntrySize
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.SourceHandle], ax
@@SearchList:
        or      bx, bx
        je      @@Done          ; Sector not in cache, exit.
; Copy the entry into first bytes of buffer
        mov     ax, bx
        mov     dx, CacheEntrySize
        mul     dx              ; Offset of entry in DX:AX
        mov     [WORD XMoveStruc.SourceOffset], ax
        mov     [(WORD XMoveStruc.SourceOffset)+2], dx
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        push    bx
        CALL_XMS
        pop     bx
        jz      @@XMSError
; Examine the entry in buffer
        mov     eax, [@@Sector]
        cmp     [DWORD EntryBuf], eax
        jne     @@Search1
        mov     ax, gs
        cmp     [WORD EntryBuf+0Ch], ax ; Check if it's the right drive.
        je      @@Done          ; Sector found, exit with entry in BX.
@@Search1:
        mov     bx, [WORD EntryBuf+06h]       ; Next entry in chain
        jmp     @@SearchList

@@XMSError:
        mov     [XMSError], 1   ; Flag XMS error
        xor     bx, bx
@@Done: ret
ENDP    SearchCache

;---------------------------------------------------------------------
; Delete a cache entry. Entry number in BX. Entry must be in use.
; Returns carry set on XMS error.
PROC    DeleteCacheEntry
        pushad
; Read the entry into buffer
        mov     [XMoveStruc.Length], CacheEntrySize
        mov     ax, bx
        mov     dx, CacheEntrySize
        mul     dx
        mov     [WORD XMoveStruc.SourceOffset], ax
        mov     [WORD (XMoveStruc.SourceOffset)+2], dx
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; Delete the entry from the hash chain
; if prev[x] <> NIL
;   then next[prev[x]] <- next[x]
;   else head <- next[x]
; if next[x] <> NIL
;   then prev[next[x]] <- prev[x]
        mov     ax, [WORD EntryBuf+04h] ; Prev field.
        or      ax, ax                      ; See if entry is head
        je      @@NewHead
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 06h                     ; Next field
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+06h
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     [XMoveStruc.Length], 2
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
        jmp     @@UpdateNextChainEntry
@@NewHead:
        xor     edx, edx
        mov     ax, [WORD EntryBuf]
        mov     dx, [WORD EntryBuf+2]   ; Sector in DX:AX
        div     [HashSize]              ; Hash slot in DX
        mov     ax, [hHashTable]
        mov     [XMoveStruc.DestHandle], ax
        shl     edx, 1
        mov     [XMoveStruc.DestOffset], edx
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+06h
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     [XMoveStruc.Length], 2
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
@@UpdateNextChainEntry:
        mov     ax, [WORD EntryBuf+06h]
        or      ax, ax
        je      @@DeleteEntry2
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 04h
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+04h
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     [XMoveStruc.Length], 2
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
@@DeleteEntry2:
; Delete the entry from the priority list.
; next[prev[x]] <- next[x]
; prev[next[x]] <- prev[x]
        mov     ax, [WORD EntryBuf+08h]         ; Prev
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 0Ah                         ; Next field
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+0Ah
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     [XMoveStruc.Length], 2
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError

        mov     ax, [WORD EntryBuf+0Ah]         ; Next
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 08h                         ; Prev field
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+08h
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
        clc
        jmp     @@Done
@@XMSError:
        stc
@@Done: popad
        ret
ENDP    DeleteCacheEntry

;---------------------------------------------------------------------
; Return a free cache entry. If cache is full, then oldest entry is
; deleted. Free entry returned in BX. BX=0000 if an XMS error occurs.
PROC    GetFreeCacheEntry STDCALL
        LOCAL   Entry
        pushad
        mov     bx, [FreeEntry]
        or      bx, bx
        jz      @@GetOldestEntry
        mov     [Entry], bx
        dec     [FreeEntry]
        jmp     @@Done
; Get the oldest entry.
@@GetOldestEntry:
        mov     [XMoveStruc.Length], 2
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.SourceHandle], ax
        mov     [XMoveStruc.SourceOffset], 08h  ; Tail of priority list
        mov     [XMoveStruc.DestHandle], 0
        mov     [WORD XMoveStruc.DestOffset], OFFSET EntryBuf
        mov     [WORD (XMoveStruc.DestOffset)+2], ds
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
        mov     bx, [WORD EntryBuf]     ; Entry to be deleted
        mov     [Entry], bx
        call    DeleteCacheEntry
        jc      @@XMSError
        jmp     @@Done
@@XMSError:
        mov     [XMSError], 1   ; Flag XMS error
        mov     [Entry], 0
@@Done: popad
        mov     bx, [Entry]
        ret
ENDP    GetFreeCacheEntry

;---------------------------------------------------------------------
; Read a sector from the cache. Cache entry in BX. The entry is moved
; to the head of the priority list. Sector is copied to ES:DI.
; Returns AX=0 if XMS error.
PROC    ReadCacheSector STDCALL
        LOCAL   @@Entry
        mov     [@@Entry], bx
        mov     [XMoveStruc.Length], 512
        mov     ax, [hCacheSectors]
        mov     [XMoveStruc.SourceHandle], ax
        movzx   eax, bx
        shl     eax, 9                  ; Multiply by 512
        mov     [XMoveStruc.SourceOffset], eax
        mov     [XMoveStruc.DestHandle], 0
        mov     [WORD XMoveStruc.DestOffset], di
        mov     [(WORD XMoveStruc.DestOffset)+2], es
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; Remove the entry from the priority list.
; Entry 0 in priority list is a sentinel.
; next[prev[x]] <- next[x]
        mov     [XMoveStruc.Length], 2
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+0Ah ; Next
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     ax, [WORD EntryBuf+08h] ; Prev
        mov     dx, CacheEntrySize
        mul     dx                      ; Offset
        add     ax, 0Ah                 ; Next field of previous entry
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; prev[next[x]] <- prev[x]
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+08h ; Prev
        mov     ax, [WORD EntryBuf+0Ah] ; Next
        mov     dx, CacheEntrySize
        mul     dx                      ; Offset
        add     ax, 08h                 ; Prev field
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; Insert the entry at the head of the priority list
; next[x] <- next[sentinel]
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.SourceHandle], ax
        mov     [XMoveStruc.SourceOffset], 0Ah  ; Next field of sentinel

        mov     ax, [@@Entry]                   ; Entry number
        mov     dx, CacheEntrySize
        mul     dx
        mov     di, dx
        shl     edi, 16
        mov     di, ax
        add     edi, 0Ah                        ; Next field
        mov     [XMoveStruc.DestOffset], edi
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; prev[next[sentinel]] <- x
        mov     [XMoveStruc.DestHandle], 0
        mov     [WORD XMoveStruc.DestOffset], OFFSET EntryBuf+CacheEntrySize
        mov     [WORD (XMoveStruc.DestOffset)+2], ds
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
        mov     ax, [WORD EntryBuf+CacheEntrySize]     ; next[sentinel]
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 08h                 ; prev field
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     ax, [@@Entry]
        mov     [WORD EntryBuf+CacheEntrySize], ax     ; Current entry
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET EntryBuf+CacheEntrySize
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; next[sentinel] <- x
        mov     [XMoveStruc.DestOffset], 0Ah
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
; prev[x] <- sentinel
        mov     [WORD EntryBuf+CacheEntrySize], 0
        mov     ax, [@@Entry]
        mov     dx, CacheEntrySize
        mul     dx
        add     ax, 08h                 ; prev field
        adc     dx, 0
        mov     [WORD XMoveStruc.DestOffset], ax
        mov     [WORD (XMoveStruc.DestOffset)+2], dx
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError
        jmp     @@Done

@@XMSError:
        mov     [XMSError], 1   ; Flag XMS error
@@Done: ret
ENDP    ReadCacheSector

;---------------------------------------------------------------------
; Read specified file sectors. Arguments:
; @@Sector      - starting file sector
; @@NumSectors  - number of sectors to read
; @@FNode       - FNode sector number of file
; @@Dest        - destination buffer
; Uses Buf4. Return CF on error.
PROC    ReadFileSector PASCAL
        DEFASSUME
        ARG     @@Sector:DWORD, @@NumSectors, @@FNode:DWORD, @@Dest:DWORD
        LOCAL   @@Entries:WORD
        pushad
; Read the FNode
        call    NEAR ReadSector, [@@FNode], 1, ds (OFFSET Buf4)
        mov     bx, OFFSET Buf4+38h
@@Start:
        mov     eax, [@@Sector]
        movzx   cx, [BYTE bx+05h]       ; number of entries in sector
        mov     [@@Entries], cx
        test    [BYTE bx], 80h          ; Internal or external node?
        jnz     @@Internal
; Search external node - extents.
        sub     bx, 4                   ; Point to 12 bytes before first entry
        xor     dx, dx                  ; Entry counter
@@NextExtent:
        add     bx, 12                  ; Point to next entry
        inc     dx
        cmp     dx, [@@Entries]         ; Any entries left?
        ja      @@Error
        mov     ecx, [DWORD bx]         ; First file sector in this extent
        cmp     ecx, eax
        ja      @@Error                 ; Too high - something's wrong
        add     ecx, [DWORD bx+04h]     ; Add number of sectors in extent
        sub     ecx, eax                ; Sector in this extent?
        jna     @@NextExtent
; Extent found - calculate disk sector and read.
        mov     esi, ecx                ; Number of sectors left in extent
        mov     ecx, eax
        sub     ecx, [DWORD bx]         ; Offset into extent
        add     ecx, [DWORD bx+08h]     ; First logical sector number
; Calculate number of sectors to read.
        movzx   edi, [@@NumSectors]
        cmp     esi, edi
        jbe     @@1
        mov     si, di
@@1:
        call    NEAR ReadSector, ecx, si, [@@Dest]
        jc      @@Error
        sub     [@@NumSectors], si
        clc
        jz      @@Done
        and     esi, 0FFFFh
        add     [@@Sector], esi
; Reread FNode
        call    NEAR ReadSector, [@@FNode], 1, ds (OFFSET Buf4)
        mov     bx, OFFSET Buf4+38h
        mov     ax, 200h
        mul     si
        add     [WORD LOW @@Dest], ax   ; High word in DX not used, should be 0.
        jmp     @@Start

; Search internal node
@@Internal:
        xor     dx, dx                  ; Entry counter
@@NextSubtree:
        add     bx, 8                   ; Point to next entry
        inc     dx
        cmp     dx, [@@Entries]         ; Any entries left?
        ja      @@Error
        cmp     eax, [DWORD bx] ; If below, then sector is in this tree.
        jae     @@NextSubtree
; Subtree found - read sector and recurse.
        call    NEAR ReadSector, [DWORD bx+04h], 1, ds (OFFSET Buf4)
        jc      @@Error
        mov     bx, OFFSET Buf4+0Ch     ; Point to allocation structure
        jmp     @@Start

@@Error:
        stc
@@Done: popad
        ret
ENDP    ReadFileSector

;---------------------------------------------------------------------
; Convert a character in AL to upper case.
PROC    UpCase
        DEFASSUME
        cmp     al, 128
        jae     @@Extended
        cmp     al, 'a'
        jb      @@Done
        cmp     al, 'z'
        ja      @@Done
        sub     al, 'a'-'A'
@@Done: ret
@@Extended:
        push    bx ds
        push    cs
        pop     ds
        mov     bx, OFFSET UpCaseTbl-128
        xlatb
        pop     ds bx
        ret
ENDP    UpCase

;---------------------------------------------------------------------
; Checks if the character in AL is a valid filename character.
; CF=1 if invalid. Registers preserved.
PROC    FileChar
        DEFASSUME
        push    ax ds
        push    cs
        pop     ds
        cmp     al, [MinPerm]
        jb      @@Fail
        cmp     al, [MaxPerm]
        ja      @@Fail
        cmp     al, [MinExcl]
        jb      @@1
        cmp     al, [MaxExcl]
        jna     @@Fail
@@1:
        push    cx di es
        push    ds
        pop     es
        ASSUME  es:ResCode
        mov     cl, [NumTerm]
        xor     ch, ch
        mov     di, OFFSET TermChars
        cld
        repne scasb
        pop     es di cx
        je      @@Fail
        clc
        jmp     @@Done
@@Fail:
        stc
@@Done:
        pop     ds ax
        ret
ENDP    FileChar

;---------------------------------------------------------------------
; Checks if a filename matches a spec.
; Filename at DS:DX. Filespec at ES:BX.
; Returns CF=0 if match.
PROC    Match
        DEFASSUME
        ASSUME  ds:NOTHING
        push    bx cx dx si di
        cld
        mov     si, dx
@@Next:
        mov     ah, [es:bx]
        inc     bx
        lodsb
; Check if the character is valid in filenames
        or      al, al
        je      @@Valid
        cmp     al, '.'
        je      @@Valid
        cmp     al, ' ' ; "WP ROOT . SF", volume label <11 chars
        je      @@Valid
        call    FileChar
        jc      @@Fail
@@Valid:
        or      ah, ah          ; End of filespec?
        jne     @@1
        or      al, al
        jne     @@Fail
        jmp     @@Succeed
@@1:    cmp     ah, '?'
        jne     @@3
        cmp     al, '.'
        je      @@2
        or      al, al
        je      @@2
        jmp     @@Next
@@2:    dec     si
        jmp     @@Next
@@3:    cmp     ah, '.'
        jne     @@4
        or      al, al
        jne     @@4
        dec     si
        jmp     @@Next
@@4:    cmp     ah, al
        je      @@Next
@@Fail:
        stc
        jmp     @@Done
@@Succeed:
        clc
@@Done: pop     di si dx cx bx
        ret
ENDP    Match

;---------------------------------------------------------------------
; Check if filespec at DS:SI matches . file. Returns CF=0 if match.
; Filespec matched: At least one <?>, optional <dot>, optional more <?>.
PROC    MatchDot
        NOASSUME
        ASSUME  cs:ResCode
        push    ax si
; Match one <?>
        lodsb
        cmp     al, '?'
        jne     @@Fail
; Eat all consecutive <?>
@@1:    lodsb
        cmp     al, '?'
        je      @@1
        or      al, al          ; End of filespec?
        je      @@Ok
        cmp     al, '.'         ; Match dot (separator)
        jne     @@Fail
; Eat all consecutive <?>
@@2:    lodsb
        cmp     al, '?'
        je      @@2
        or      al, al          ; End of filespec?
        jne     @@Fail

@@Ok:   clc
        jmp     @@Done
@@Fail: stc
@@Done: pop     si ax
        ret
ENDP    MatchDot

;---------------------------------------------------------------------
; Check if filespec at DS:SI matches .. file. Returns CF=0 if match.
; Filespec matched: At least two <?>, optional <dot>, optional more <?>.
PROC    MatchDotDot
        NOASSUME
        ASSUME  cs:ResCode
        push    ax si
; Match two <?>
        lodsw
        cmp     ax, '??'
        jne     @@Fail
; Eat all consecutive <?>
@@1:    lodsb
        cmp     al, '?'
        je      @@1
        or      al, al          ; End of filespec?
        je      @@Ok
        cmp     al, '.'         ; Match dot (separator)
        jne     @@Fail
; Eat all consecutive <?>
@@2:    lodsb
        cmp     al, '?'
        je      @@2
        or      al, al          ; End of filespec?
        jne     @@Fail

@@Ok:   clc
        jmp     @@Done
@@Fail: stc
@@Done: pop     si ax
        ret
ENDP    MatchDotDot

;---------------------------------------------------------------------
; Convert an HPFS filename to a valid DOS filename. Only to be called
; from FindNext.
; DS:SI - filename to convert. ES:DI - buffer to place converted name.
; CX - length of filename.
PROC    ConvertFilename STDCALL
        DEFASSUME
        LOCAL   @@Len:WORD
        pusha
        cld
        mov     [@@Len], cx
        xor     ah, ah
@@ConvName:
        jcxz    @@ConvExt
        lodsb
        dec     cx
        cmp     al, '.'
        je      @@ConvExt
        call    FileChar        ; Character valid in filenames?
        jc      @@ConvName
        call    UpCase
        stosb
        inc     ah
        cmp     ah, 8
        jb      @@ConvName
@@ConvExt:
        or      ah, ah          ; Empty name?
        jnz     @@Conv1
        mov     eax, 'SFPH'     ; Dummy name
        stosd
@@Conv1:
        mov     al, '.'
        stosb
        lea     si, [Buf1+bx+1Fh]       ; Point to filename
        mov     cx, [@@Len]             ; Length of filename
        call    CRC16                   ; CRC16 in DX
        mov     ax, dx
        xor     dx, dx
; Convert to base-41
        mov     bx, OFFSET ConvTable    ; Translation table
        mov     cx, 3
@@ConvBase:
        mov     si, 41
        div     si                      ; Remainder in DX (DL)
        xchg    al, dl
        xlatb
        stosb
        mov     al, dl
        xor     dx, dx
        loop    @@ConvBase
        xor     al, al                  ; Zero terminate
        stosb
        popa
        ret
ENDP    ConvertFilename

;---------------------------------------------------------------------
; Find a directory entry. FNode sector of directory passed in ECX.
; Filename at DS:DX. Assumes DS=CS.
; CF=1 if failed.
; The sector is read into Buf1, and BX contains the offset to the
; specified entry on return. Uses last half of FNameBuf
PROC    FindDirEntry STDCALL
        DEFASSUME
        ASSUME  es:ResCode
        LOCAL   @@Len:BYTE, @@BytesRead:WORD, @@Sector:DWORD
        push    es
        push    cs
        pop     es
        mov     [@@Sector], ecx
; Count characters in filename.
        mov     di, dx
        xor     al, al
        mov     cx, 0FFh
        cld
        repne scasb
        neg     cl
        sub     cl, 2
        or      cl, cl
        jz      @@Fail
        mov     [@@Len], cl
        mov     [@@BytesRead], 200h       ; Bytes of directory block in memory
; Read the FNode of the directory
        call    NEAR ReadSector, [@@Sector], 1, es (OFFSET Buf1)
        jc      @@Fail
        mov     ecx, [DWORD Buf1+48h]
        mov     [@@Sector], ecx
        call    NEAR ReadSector, ecx, 4, es (OFFSET Buf1)
        jc      @@Fail

        mov     bx, 14h         ; Offset of first dir. entry
        cld
        cmp     [ConvertLong], 0
        je      @@TestEntry
; Traverse the directory B Tree to find a match. Convert long filenames.
; All entries must be searched.
@@ChkValidName:
        mov     di, OFFSET FNameBuf+64  ; Load regs for call or move later
        lea     si, [Buf1+bx+1Fh]
        movzx   cx, [BYTE si-1]
        test    [Buf1+bx+03h], 40h
        jz      @@DOSName
; Convert filename
        call    ConvertFilename
        jmp     @@CompareName
; Copy filename to buffer
@@DosName:
        test    [Buf1+bx+02h], 01h      ; Skip '.' file
        jnz     @@NextLong
        push    cx di
@@CopyName:
        lodsb
        call    UpCase
        stosb
        loop    @@CopyName
        xor     al, al
        stosb
        pop     di cx
@@CompareName:
        mov     si, dx
@@Cmp1:
        lodsb
        or      al, al
        jz      @@EndOfTmpl             ; End of search filename
        scasb
        je      @@Cmp1
        jmp     @@NextLong
@@EndOfTmpl:
        cmp     [BYTE es:di], 0         ; Check that filename in entry also ends
        je      @@Succeed
@@NextLong:
; See if there's a B Tree pointer
        test    [BYTE Buf1+bx+02h], 04h
        jz      @@NoBTree
; Go down the tree
        add     bx, [WORD Buf1+bx]
        mov     eax, [DWORD Buf1+bx-04h]    ; B Tree pointer
        mov     [@@Sector], eax
        call    NEAR ReadSector, eax, 4, es (OFFSET Buf1)
        jc      @@Fail
        mov     bx, 14h
        jmp     @@ChkValidName                  ; Search this block
@@NoBTree:
; Move to next entry
@@NextEntry:
        test    [BYTE Buf1+bx+02h], 08h     ; See if last entry
        jnz     @@Up
        add     bx, [WORD Buf1+bx]          ; Point to next entry
        jmp     @@ChkValidName                  ; Continue search
; Go up the tree
@@Up:
        mov     eax, [DWORD Buf1+0Ch]   ; Parent node
        call    NEAR ReadSector, eax, 4, es (OFFSET Buf1)
        jc      @@Fail
        cmp     [BYTE Buf1+03h], 0F7h      ; FNode signature
        je      @@Fail
; Go through this directory block to find the entry that we came from.
        mov     bx, 14h
@@SrchParent:
        test    [Buf1+bx+02h], 04h      ; Entry has B tree pointer?
        jz      @@NotParent
        mov     si, bx
        add     si, [WORD Buf1+bx]
        mov     ecx, [@@Sector]
        cmp     [DWORD Buf1+si-4], ecx  ; Is parent?
        jne     @@NotParent
        mov     [@@Sector], eax
        jmp     @@NextEntry
@@NotParent:
        test    [Buf1+bx+02h], 08h      ; Last entry in block?
        jnz     @@Fail
        add     bx, [WORD Buf1+bx]
        jmp     @@SrchParent

; Traverse the directory B Tree to find a match. No long filenames.
@@TestEntry:
        cld
; Compare the filename length byte
        mov     cl, [@@Len]
        cmp     cl, [Buf1+bx+1Eh]
        jna     @@1
        mov     cl, [Buf1+bx+1Eh]       ; Entry shorter than filename
; Compare the filenames
@@1:
        xor     ch, ch
        mov     di, dx
        lea     si, [Buf1+bx+1Fh]
@@Compare:
        lodsb                   ; Load one character
        call    UpCase          ; Convert to upper case
        scasb                   ; Compare
        loope   @@Compare
        jb      @@Next
        ja      @@ChkBTree
        mov     cl, [@@Len]       ; See which of the names is the longest
        cmp     cl, [Buf1+bx+1Eh]
        je      @@Succeed
        ja      @@Next
; See if there's a B Tree pointer
@@ChkBTree:
        test    [BYTE Buf1+bx+02h], 04h
        jz      @@Fail
        add     bx, [WORD Buf1+bx]
        mov     ecx, [DWORD Buf1+bx-4]      ; Extract the B Tree pointer
        mov     [@@Sector], ecx
        mov     [@@BytesRead], 200h
        call    NEAR ReadSector, [@@Sector], 1, es (OFFSET Buf1)
        mov     bx, 14h
        jmp     @@TestEntry

@@Next:
        test    [BYTE Buf1+bx+02h], 08h     ; See if last entry
        jnz     @@Fail
        add     bx, [WORD Buf1+bx]          ; Point to next entry
        mov     ax, [WORD Buf1+bx]
        add     ax, bx
        cmp     ax, [@@BytesRead]                 ; See if enough sectors read.
        jb      @@TestEntry
        cmp     [@@BytesRead], 800h
        jnb     @@Fail                          ; Whole dir. block already read.
        inc     [@@Sector]
        push    bx
        mov     bx, [@@BytesRead]
        add     bx, OFFSET Buf1
        call    NEAR ReadSector, [@@Sector], 1, es bx
        pop     bx
        add     [@@BytesRead], 200h
        jmp     @@TestEntry

@@Fail:
        stc
        jmp     @@Done
@@Succeed:
        clc
@@Done:
        pop     es
        ret
ENDP    FindDirEntry

;---------------------------------------------------------------------
; Find a file. Fully qualified filename at ES:DI. Offset into
; Buf1 to directory entry returned in BX.

PROC    FindFile STDCALL
        DEFASSUME
        LOCAL   @@DirFlag:BYTE, @@CurFNode:DWORD, @@RetValue, @@LongPath:BYTE
        LOCAL   @@BufStart
        pushad
        push    ds
        push    cs
        pop     ds
        mov     [@@DirFlag], 0            ; Set if current level is also the last.
        mov     [@@LongPath], 0         ; Set if path is too long for any more subdirs.
        mov     [@@BufStart], di
        mov     ecx, [RootFNode]
        mov     [@@CurFNode], ecx
; Scan past the first backslash.
        mov     al, '\'
        mov     cx, 80h
        cld
        repne scasb
        dec     di
@@NextLevel:
        cmp     [@@LongPath], 0
        jne     @@Fail                  ; Path too long - can't access directory.
; See if path is too long
        mov     ax, di
        sub     ax, [@@BufStart]
        cmp     ax, MaxPathLength-9
        seta    [@@LongPath]

        mov     bx, OFFSET FNameBuf
        mov     dx, bx
; Move the name of the next subdirectory to FNameBuf
@@MoveName:
        inc     di
        mov     al, [es:di]
        or      al, al
        je      @@SetFlag
        cmp     al, '\'
        je      @@FindEntry
        mov     [bx], al
        inc     bx
        jmp     @@MoveName
@@SetFlag:
        mov     [@@DirFlag], 1            ; This is the last level
@@FindEntry:
        mov     [BYTE bx], 0
; Find the directory entry
        mov     ecx, [@@CurFNode]
        push    di
        call    FindDirEntry
        mov     [@@RetValue], bx
        pop     di
        jc      @@Fail
        mov     ecx, [DWORD Buf1+bx+04h]    ; Get FNode pointer
        mov     [@@CurFNode], ecx
        cmp     [@@DirFlag], 0
        je      @@NextLevel
        clc
        jmp     @@Done

@@Fail: stc
@@Done:
        pop     ds
        popad
        jc      @@Exit
        mov     bx, [@@RetValue]
@@Exit:
        ret
ENDP    FindFile

;---------------------------------------------------------------------
; Check if a fully qualified pathname is too long to be the name of a
; directory, and should be handled as a zero-length file. CF=1 if path
; is too long.
PROC    ChkPathLength PASCAL
        DEFASSUME
        ARG     @@FileName:DWORD
        pushad
        push    es
        les     di, [@@FileName]
        xor     al, al
        mov     cx, 128
        cld
        repne scasb
        mov     al, '\'
        std
        mov     cx, 128
        repne scasb
        cld
        sub     di, [WORD LOW @@FileName]
        cmp     di, MaxPathLength-9
        cmc                             ; CF indicates result
        pop     es
        popad
        ret
ENDP    ChkPathLength

;---------------------------------------------------------------------
; Convert UNIX (and HPFS) timestamp to DOS timestamp (local time).
; UNIX timestamp passed in EAX and DOS timestamp returned in EAX -
; time in low word, date in high word.
PROC    Unix2DosTime STDCALL
        DEFASSUME
        LOCAL   @@Result:DWORD,@@Year,@@Month,@@Day,@@Hour,@@Min,@@Secs
        pushad
        push    ds
        push    cs
        pop     ds
        sub     eax, 24*60*60*3652+TimeZone+60*60
        xor     edx, edx
        mov     ebx, 60
        div     ebx
        mov     [@@Secs], dx

        xor     edx, edx
        div     ebx
        mov     [@@Min], dx               ; Time in minutes in EAX

        mov     ebx, 1461*24
        xor     edx, edx
        div     ebx
        shl     eax, 2
        add     eax, 1980
        mov     [@@Year], ax

        mov     eax, edx
        cmp     eax, 366*24
        jb      @@1
        sub     eax, 366*24
        inc     [@@Year]
        mov     ebx, 365*24
        xor     edx, edx
        div     ebx
        add     [@@Year], ax
        mov     ax, dx

@@1:                                    ; Now in AX
        xor     dx, dx
        mov     bx, 24
        div     bx
        mov     [@@Hour], dx
        inc     ax

        test    [@@Year], 3
        jnz     @@2
        cmp     ax, 60
        jna     @@3
        dec     ax
        jmp     @@2
@@3:    jne     @@2
        mov     [@@Month], 2
        mov     [@@Day], 29
        jmp     @@DecodeDone

@@2:
        xor     bx, bx
@@2_1:
        mov     dl, [DaysInMonth+bx]
        xor     dh, dh
        cmp     dx, ax
        jnb     @@4
        sub     ax, dx
        inc     bx
        jmp     @@2_1
@@4:    inc     bx
        mov     [@@Month], bx
        mov     [@@Day], ax

@@DecodeDone:
; Pack the time in DOS format.
        xor     eax, eax
        mov     ax, [@@Day]
        mov     bx, [@@Month]
        shl     bx, 5
        or      ax, bx
        mov     bx, [@@Year]
        sub     bx, 1980
        shl     bx, 9
        or      ax, bx

        shl     eax, 16
        mov     ax, [@@Secs]
        shr     ax, 1
        mov     bx, [@@Min]
        shl     bx, 5
        or      ax, bx
        mov     bx, [@@Hour]
        shl     bx, 11
        or      ax, bx
        mov     [@@Result], eax

        pop     ds
        popad
        mov     eax, [@@Result]
        ret
ENDP    Unix2DosTime

;---------------------------------------------------------------------
; Compute CRC16 of a string. DS:SI points at the buffer, CX is the
; length of the buffer. CRC16 returned in DX.

PROC    CRC16
        NOASSUME
        ASSUME  cs:ResCode
        push    ax bx
        pushf
        cld
        xor     dx, dx
@@1:    lodsb
        xor     ah, ah
        xchg    ah, al
        xor     dx, ax
        push    cx
        mov     cx, 8
@@2:    mov     bx, dx
        shl     dx, 1
        and     bx, 8000h
        jz      @@3
        xor     dx, 1021h
@@3:    loop    @@2
        pop     cx
        loop    @@1
        popf
        pop     bx ax
        ret
ENDP    CRC16

PROC    ThunkReadSector PASCAL FAR
        ARG     @@Sector:DWORD,@@NumSectors:WORD,@@Dest:DWORD
        call    NEAR ReadSector, [@@Sector], [@@NumSectors], [@@Dest]
        ret
ENDP    ThunkReadSector

        DB      84 DUP('RESSTACK')
LABEL   ResStack
ENDS    ResCode

;================================================== Resident data
SEGMENT ResData
PartitionNr DB  0               ; HPFS partition number
; Partition info
nSecs   DB      0               ; Sectors per track
nHeads  DW      0               ; Heads
LBAstart DD     0               ; LBA starting sector
AccessMethod DB 0               ; Disk access method
PhysDrv DB      80h
; File system info
RootFNode DD    0               ; FNode sector for root dir.
CDFNode DD      0               ; FNode sector for current dir.
TotalSectors DD 0               ; Partition size in sectors
FreeSectors DD  0               ; # of sectors marked as free in bitmaps
MediaID DB      0               ; Media ID byte from boot block
Volabel DB      12 DUP(0)       ; Volume label + an extra null.
BSCRC   DW      0               ; used to find exchanged media
LABEL   EndResdata
ENDS    ResData

;======================================================= Transient section
        NOASSUME
SEGMENT CSeg
        ASSUME  cs:CSeg,ds:DSeg,es:NOTHING,ss:SSeg,fs:ResCode,gs:ResCode
PROC    Main
        mov     ax, DSeg
        mov     ds, ax
        mov     es, ax
        mov     ax, ResCode
        mov     fs, ax
        mov     gs, ax

; Write hello message
        mov     ah, 09h
        mov     dx, OFFSET MsgHello
        int     21h

; Release environment block.
        push    es
        mov     ah, 51h         ; PSP Segment
        int     21h
        mov     es, bx
        sub     ax, ax
        xchg    ax, [es:2Ch]    ; ENV Segment
        mov     es, ax
        mov     ah, 49h
        int     21h
        pop     es
        jc      errEnvRel

; Check for DR-DOS or Novell DOS
        mov     ax, 4452h
        stc
        int     21h
        jc      @@NotDrDos
; DR-DOS or Novell DOS found. Check version.
        cmp     ah, 10h
        jne     errBadDosVer
        cmp     al, 72h         ; Novell DOS 7.0
        jae     @@NovellDOS     ; or higher
        mov     [DrDos], al     ; DR-DOS version code
        cmp     al, 65h         ; DR-DOS 5
        je      @@Dos3
        cmp     al, 67h         ; DR-DOS 6
        je      @@Dos3
        jmp     errBadDosVer
@@NovellDOS:
        mov     [Novell], 1
        mov     [WORD LOW AccMode], 0788h ; 0100:0aa8
                                          ; INT 21/5d06 returns
                                          ; 0100:0320 in NWDOS and DRDOS 7.03
        jmp     @@DosVerOk
@@NotDrDos:
; Check DOS version
        mov     ax, 3000h
        int     21h
        mov     [DosVersion], al
        cmp     al, 3
        je      @@Major3
        jb      errBadDOSVer
        cmp     al, 10
        jae     errBadDOSVer
        jmp     @@DosVerOk
; DOS 3.x, check minor version is at least 10
@@Major3:
        cmp     ah, 10
        jb      errBadDOSVer
; DOS 3.10-3.x or DR DOS 5 and 6
@@Dos3:
        mov     [CDSSize], 51h
        mov     [WORD LOW pCurrCDS], 26Ch
        mov     [WORD LOW FN1], 92h
        mov     [WORD LOW AccMode], 23Bh
        mov     [WORD LOW SrchAttr], 23Ah

@@DosVerOk:

; get info about other iHPFS
        call    GetResCode
        mov     ax, [ResCodeSegment]

        or      ax, ax
        jz      @@AskPSP

        mov     fs, ax
        mov     gs, ax
        ; PSP of resident copy already known
        jmp     @@DonePSP_SDA

@@AskPSP:
; Get PSP
        mov     ah, 51h
        int     21h
        mov     [PSP], bx
; Release heap space above end of stack
        mov     bx, sp
        add     bx, 15
        shr     bx, 4
        mov     ax, ss
        add     bx, ax
        mov     ax, [PSP]
        sub     bx, ax
        push    es
        mov     es, ax
        mov     ah, 4Ah
        int     21h
        pop     es
        jc      errMemRel

; Get SDA address
        mov     ax, 5D06h
        int     21h             ; Address returned in DS:SI
        ASSUME  ds:NOTHING
        jc      errNoSDA

        mov     ax, ds
        shl     eax,16
        mov     ax,si
        mov     [SDA], eax
        add     [pCurrCDS], eax
        add     [pDTA], eax
        add     [FN1], eax
        add     [AccMode], eax
        add     [SrchAttr], eax
        add     [ExtOpenMode], eax

@@DonePSP_SDA:
        mov     ax, DSeg
        mov     ds, ax
        ASSUME  ds:DSeg
; Get address of LoL
        mov     ah, 52h
        int     21h             ; ES:BX -> List of Lists
        ASSUME  es:NOTHING
        mov     [WORD LOW LstOfLst], bx
        mov     [WORD HIGH LstOfLst], es
; Get Lastdrive
        mov     cl, [es:bx+21h]
        mov     [LastDrive], cl
; Check if XMS is present, and get driver entry point
        mov     ax, 4300h
        int     2Fh
        cmp     al, 80h
        jne     NoXMS
        mov     [XMSFound], 1
        mov     ax, 4310h
        int     2Fh
        mov     [WORD LOW XMSEntry], bx
        mov     [WORD HIGH XMSEntry], es
NoXMS:

; Parse command line
        call    ParseCmdLine

; Delete all drives with exchanged media
        cmp     [Uninstall], 0
        je      @@CheckMediaBegin
        cmp     [SpecUninstall], 0
        jz      @@CheckMediaLoopEnd     ; Uninstall all drives
@@CheckMediaBegin:
        mov     bx, -1
@@CheckMediaLoop:
        inc     bx
        cmp     bx, MaxDrives
        je      @@CheckMediaLoopEnd

        cmp     [UninstallDrv+bx], 0    ; prevent error message
        jne     @@CheckMediaLoop

        call    NEAR DeleteChangedMediaDrive, bx
        or      ah, ah
        jnz     @@CheckMediaLoop

        mov     dl, bl
        add     dl, 'A'
        mov     [MsgDrvRemovedLetter], dl
        mov     ah, 9
        mov     dx, OFFSET MsgDrvRemoved
        int     21h
        jmp     @@CheckMediaLoop

@@CheckMediaLoopEnd:

        cmp     [UnInstall], 1
        jne     InstallDriver

        mov     ax, [ResCodeSegment]
        or      ax, ax
        jz      @@NotAlreadyResident

; Installed iHPFS found.
        mov     es, ax
        mov     [DriverLoaded], 1
        cmp     [SpecUninstall], 0
        jz      @@TotalUninstall        ; Uninstall all drives
; Loop through list of drives to uninstall.
        mov     bx, -1
@@UninstallDrives:
        inc     bx
        cmp     bx, MaxDrives
        je      @@NotAlreadyResident
        cmp     [UninstallDrv+bx], 0
        je      @@UninstallDrives
        call    NEAR QueryDrive, bx
        or      ah, ah
        jz      @@UninstallDrives
        mov     [UninstallDrv+bx], 0    ; Driver found
        call    NEAR RemoveDrv, bx
        or      ah, ah
        jnz     @@errRmDrvFail
        mov     dl, bl
        add     dl, 'A'
        mov     [MsgDrvRemovedLetter], dl
        mov     ah, 9
        mov     dx, OFFSET MsgDrvRemoved
        int     21h
        jmp     @@UninstallDrives
@@errRmDrvFail:
        mov     dl, bl
        add     dl, 'A'
        mov     [MsgCantRmDrvLetter], dl
        mov     dx, OFFSET MsgCantRmDrv
        mov     ah, 9
        int     21h
        jmp     @@UninstallDrives

@@TotalUninstall:
        call    NEAR UninstallDriver
        mov     [Installed], 1          ; Driver uninstalled
        ;jmp     @@NotAlreadyResident

@@NotAlreadyResident:
        cmp     [DriverLoaded], 0
        je      @@errNotLoaded
        cmp     [SpecUninstall], 0
        jnz     @@ChkUninstallResult
        cmp     [Installed], 0
        je      @@errCantUninstall
; Total uninstall done.
        mov     ah, 9
        mov     dx, OFFSET MsgUnInstalled
        int     21h
        mov     ax, 4C00h
        int     21h

; Check if any drives couldn't be uninstalled.
@@ChkUninstallResult:
        mov     bx, -1
@@ChkUninstall1:
        inc     bx
        cmp     bx, MaxDrives
        je      @@Exit
        cmp     [UninstallDrv+bx], 0
        je      @@ChkUninstall1
        mov     dl, bl
        add     dl, 'A'
        mov     [MsgDrvNotInstLetter], dl
        mov     dx, OFFSET MsgDrvNotInst
        mov     ah, 9
        int     21h
        jmp     @@ChkUninstall1

InstallDriver:
        cmp     [ResCodeSegment], 0
        jne     @@UseExistingCopy

        ASSUME  cs:CSeg,ds:DSeg,es:NOTHING,fs:NOTHING,ss:SSeg,gs:ResCode

; Find an unused Multiplex function
        xor     ah, ah
FindFreeMux:
        xor     al, al                  ; Installation check
        push    ax
        int     2Dh
        or      al, al
        pop     ax
        je      FoundFreeMux
        add     ah, 1
        jz      errNoFreeMux
        jmp     FindFreeMux
FoundFreeMux:
        mov     [ApiFunc], ah          ; Save Multiplex function

@@UseExistingCopy:
        call    NEAR ScanDisks
; Check if any specified partitions weren't found.
        mov     bx, -1
@@ChkPartFound:
        inc     bx
        cmp     bx, MaxDrives
        je      @@ChkPartFound1
        cmp     [Partitions+bx], 0FEh
        jae     @@ChkPartFound
        mov     ax, bx
        mov     dl, 10
        div     dl
        or      al, al
        jz      @@ChkPartFound2
        add     al, 30h
@@ChkPartFound2:
        add     ah, 30h
        mov     [WORD MsgPartNotFoundNr], ax
        mov     dx, OFFSET MsgPartNotFound
        mov     ah, 9
        int     21h
        mov     [ErrSignaled], 1
        jmp     @@ChkPartFound
@@ChkPartFound1:
        cmp     [Installed], 0
        jnz     @@Installed     ; One or more drives installed
        cmp     [ErrSignaled], 0
        jnz     @@Exit          ; Error message already displayed
        jmp     errNoHPFS       ; No HPFS partitions found

; One or more drives successfully installed.
@@Installed:
        cmp     [ResCodeSegment], 0
        jne     @@Exit          ; do not need to install code a second time

        mov     ax, ResCode
        mov     es, ax
        ASSUME  es:ResCode
; Get File Character Upcase Table
        mov     ax, 6504h
        mov     bx, 0FFFFh
        mov     cx, 5
        mov     dx, 0FFFFh
        mov     di, OFFSET TermChars
        int     21h
        jc      @@GetFileChar
        push    ds
        lds     si, [DWORD TermChars+1]
        add     si, 2
        mov     di, OFFSET UpCaseTbl
        mov     cx, 32
        cld
        rep movsd
        pop     ds

; Get File-Character Table
@@GetFileChar:
        mov     ax, 6505h       ; Get File-Character Table
        mov     bx, 0FFFFh      ; Default codepage
        mov     cx, 5           ; Buffer size
        mov     dx, 0FFFFh      ; Default country ID
        mov     di, OFFSET TermChars
        int     21h
        jc      @@FileChar2
        mov     si, [WORD TermChars+1]
        mov     ax, [WORD TermChars+3]
        mov     ds, ax
        ASSUME  ds:NOTHING
        add     si, 3
        cld
        lodsw
        mov     [WORD MinPerm], ax
        inc     si
        lodsw
        mov     [WORD MinExcl], ax
        inc     si
        lodsb
        mov     cl, al
        mov     [NumTerm], al
        xor     ch, ch
        mov     di, OFFSET TermChars
        rep movsb
@@FileChar2:

; Get interrupt 2D and 2F vectors.
        ASSUME  ds:NOTHING,es:NOTHING,fs:ResCode,gs:NOTHING
        mov     ax, ResCode
        mov     fs, ax
GetIntVectors:
        mov     ax, 352Dh
        int     21h
        mov     [WORD OldInt2D], bx
        mov     [WORD HIGH OldInt2D], es
        mov     ax, 352Fh
        int     21h
        mov     [WORD OldInt2F], bx
        mov     [WORD HIGH OldInt2F], es

; Set new interrupt 2D and 2F vectors.
        push    ds
        mov     ax, ResCode
        mov     ds, ax
        mov     ax, 252Dh
        mov     dx, OFFSET Int2DEntry
        int     21h
        mov     ax, 252Fh
        mov     dx, OFFSET Int2FEntry
        int     21h
        pop     ds

; Terminate and Stay Resident
        mov     ax, 3100h
        mov     dx, ResData
        sub     dx, ResCode
        add     dx, 16          ; 16 paras for PSP
        mov     bx, OFFSET EndResData
        shr     bx, 4
        add     dx, bx
        inc     dx
        int     21h

@@Exit:
        mov     ax, 4C00h
        int     21h
;--------
; Errors

errBadDosVer:
        Abort   0
errEnvRel:
errMemRel:
        Abort   1               ; Malloc error
errNoSDA:
        Abort   3               ; Compatibility error
errNoHPFS:
        Abort   5
errNoFreeMux:
        Abort   14              ; No free multiplex function
@@errCantUninstall:
        Abort   11                      ; Cannot unload
@@errNotLoaded:
        Abort   15                      ; Driver not loaded
ENDP    Main

;---------------------------------------------------------------------
; Scans all hard disks in the system.
PROC    ScanDisks STDCALL
        ASSUME  ds:DSeg,es:ResCode,fs:ResCode,gs:NOTHING
        LOCAL   @@Drive:BYTE, @@Cyls:WORD, @@Heads:WORD, @@Secs:WORD
        LOCAL   @@IBM_MS_Ext:WORD
        LOCAL   @@target_id:WORD
        push    es fs gs

        ;mov     ax, ResCode
        call    SetAXResCode
        mov     es, ax
        mov     fs, ax

        ;------------------------------------------------------------
        mov     [@@Drive], 80h
@@DoDrive:
        mov     al, [@@Drive]
        mov     ah, al
        shr     al, 4
        and     ax, 00f0fh
        add     ax, '00'
        cmp     al, '9'
        jbe     @H109
        add     al,'A'-'0'-10
  @H109:
        cmp     ah, '9'
        jbe     @H209
        add     ah,'A'-'0'-10
  @H209:
        mov     [MsgHDScan1],ax

        mov     ah, 009h
        mov     dx, OFFSET MsgHDScan
        int     021h

; Get drive type
        mov     ah, 15h
        mov     dl, [@@Drive]
        int     13h
        jc      @@DoneHD
        cmp     ah, 03h
        je      @@IsHarddisk
        cmp     [EnableFloppy],1
        jne     @@DoneHD
@@IsHarddisk:
; Get drive parameters
        mov     ah, 08h
        mov     dl, [@@Drive]
        sub     cx, cx
        int     13h
        jc      @@DoneHD
        jcxz    @@DoneHD
        mov     ax, cx
        xchg    ah, al
        shr     ah, 6
        inc     ax
        mov     [@@Cyls], ax
        mov     [BYTE LOW @@Heads], dh
        mov     [BYTE HIGH @@Heads], 0
        inc     [@@Heads]
        and     cx, 3Fh
        mov     [@@Secs], cx
; Check for IBM/MS Extensions
; Some BIOSes return major version 02h in AH, which seems to
; be incorrect. These BIOSes also have a strange support bitmap,
; so we ignore it.
        mov     [@@IBM_MS_Ext], 0
        cmp     [NoIBMExt], 0
        jnz     @@ExtChecked
        mov     ah, 41h
        mov     bx, 55AAh
        mov     dl, [@@Drive]
        int     13h
        jc      @@ExtChecked
        cmp     bx, 0AA55h
        jne     @@ExtChecked
        cmp     ah, 02h                 ; Buggy version
        je      @@HasIBMExt
        test    cl, 1                   ; Extended functions supported
        jz      @@ExtChecked
@@HasIBMExt:
        mov     [@@IBM_MS_Ext], 1
@@ExtChecked:
; Read the first sector just to make sure the drive exists. This is the only
; safe way as the drive type/drive param calls may return crazy values.
        mov     ax, 0201h
        mov     bx, OFFSET Buf1
        mov     cx, 1
        movzx   dx, [@@Drive]
        int     13h
        jc      @@DoneHD

        mov     ah, 009h
        mov     dx, OFFSET clearline
        int     021h

        movzx   dx, [@@Drive]
        mov     [ExtPartBase], 0
        call    NEAR ScanPartTbl, Method_CHS,dx,[@@Heads],[@@Secs],[@@IBM_MS_Ext],0,0,1,0 0
        inc     [@@Drive]
        jnc     @@DoDrive

@@DoneHD:
        mov     ah, 009h
        mov     dx, OFFSET clearline
        int     021h
        ;------------------------------------------------------------

        ;------------------------------------------------------------
        cmp     [EnableAspi],1
        jne     @@DoneAspi

        ; Open ASPI Driver
        mov     ax,03d00h
        mov     dx,offset scsimgr_name
        int     021h
        jc      @@DoneAspi

        ; Get Addr
        mov     bx,ax
        mov     ax,04402h
        mov     cx,4
        mov     dx,offset scsimgr
        push    ds
        push    es
        pop     ds
        int     021h
        pop     ds

        ; Close ASPI Driver
        mov     ah,03eh
        int     021h

        mov     [@@target_id],-1

  @@DoAspi:
        cmp     [@@target_id],00fh
        je      @@DoneAspi

        inc     [@@target_id]
        mov     ax, [@@target_id]
        add     al,'0'
        cmp     al,'9'
        jbe     @A09
        add     al,'A'-'0'-10
  @A09:
        mov     [MsgAspiScan1],al

        mov     ah, 09h
        mov     dx, OFFSET MsgAspiScan
        int     21h


        sub     eax,eax
        call    NEAR ReadSectorASPI PASCAL, eax,[@@target_id],ax,ax,es (OFFSET Buf1)
        pushf

        mov     ah, 009h
        mov     dx, OFFSET clearline
        int     021h

        popf
        jc      @@DoAspi

        mov     al,[Buf1+512-2-4*16+5]
        or      al,[Buf1+512-2-3*16+5]
        or      al,[Buf1+512-2-2*16+5]
        or      al,[Buf1+512-2-1*16+5]
        and     ax,000ffh
        inc     ax
        mov     [@@Heads],ax

        mov     al,[Buf1+512-2-4*16+6]
        or      al,[Buf1+512-2-3*16+6]
        or      al,[Buf1+512-2-2*16+6]
        or      al,[Buf1+512-2-1*16+6]
        and     ax,0003fh
        mov     [@@Secs],ax

        mov     [ExtPartBase], 0
        call    NEAR ScanPartTbl, Method_ASPI,[@@target_id],[@@Heads],[@@Secs],0,0,0,1,0 0
        jmp     @@DoAspi


@@DoneAspi:
        ;------------------------------------------------------------


        ;------------------------------------------------------------
        cmp     [EnableFloppy],1
        jne     @@DoneFloppy
        mov     [@@Drive], 0ffh
@@DoFloppy:
        inc     [@@Drive]
        cmp     [@@Drive], 004h
        je      @@DoneFloppy
        mov     al, [@@Drive]
        add     al,'0'
        cmp     al,'9'
        jbe     @F09
        add     al,'A'-'0'-10
  @F09:
        mov     [MsgFloppyScan1],al

        mov     ah, 09h
        mov     dx, OFFSET MsgFloppyScan
        int     21h

; Get drive type
        mov     ah, 15h
        mov     dl, [@@Drive]
        int     13h
        jc      @@jmpDoFloppy
; Get drive parameters
        mov     ah, 08h
        mov     dl, [@@Drive]
        sub     cx, cx
        push    es
        int     13h
        pop     es
        jc      @@jmpDoFloppy
        jcxz    @@jmpDoFloppy
        mov     ax, cx
        xchg    ah, al
        shr     ah, 6
        inc     ax
        mov     [@@Cyls], ax
        mov     [BYTE LOW @@Heads], dh
        mov     [BYTE HIGH @@Heads], 0
        inc     [@@Heads]
        and     cx, 3Fh
        mov     [@@Secs], cx

        mov     [@@IBM_MS_Ext], 0

        mov     cx, 3
  @@TryFloppy:
        push    cx
        mov     ax, 0201h
        mov     bx, OFFSET Buf1
        mov     cx, 1
        movzx   dx, [@@Drive]
        int     13h
        pop     cx
        jnc     @@ReadFloppy
        loop    @@TryFloppy
        jmp     @@jmpDoFloppy
  @@ReadFloppy:
        mov     ah, 009h
        mov     dx, OFFSET clearline
        int     021h

        movzx   dx, [@@Drive]
        mov     [ExtPartBase], 0

        sub     eax,eax
        movzx   dx,[@@Drive]
        call    NEAR CheckHPFSPart, Method_CHS,dx,[@@Heads],[@@Secs],ax,ax,1,eax
  @@jmpDoFloppy:
        mov     ah, 009h
        mov     dx, OFFSET clearline
        int     021h
        jmp     @@DoFloppy

@@DoneFloppy:
        ;------------------------------------------------------------

@@Done:
        pop     gs fs es
        ret
ENDP    ScanDisks

;---------------------------------------------------------------------
; Scans partition tables on a drive.
; Args: @@Method        Disk access method to use
;       @@Drive         Drive number
;       @@nHeads        Number of heads (logical)
;       @@nSecs         Number of sectors (logical)
;       @@IBM_MS_Ext    IBM/MS Extensions supported
;       @@PartCyl       Cylinder of partition table
;       @@PartHead      Head of partition table
;       @@PartSec       Sector of partition table
;       @@PartLBA       LBA of partition table
; Return CF=1 if a critical error is encountered (scan should not continue)
PROC    ScanPartTbl PASCAL
        ASSUME  ds:DSeg,es:ResCode,fs:ResCode,gs:NOTHING
        ARG     @@Method,@@Drive,@@nHeads,@@nSecs,@@IBM_MS_Ext,@@PartCyl,@@PartHead,@@PartSec,@@PartLBA:DWORD
        LOCAL   @@PartOfs:WORD, @@HighPart:BYTE, @@sLBA:DWORD
        LOCAL   @@sCyl:WORD, @@sHead:WORD, @@sSec:WORD
        LOCAL   @@eCyl:WORD, @@eHead:WORD, @@eSec:WORD

        mov     [@@PartOfs], 1BEh
@@DoEntry:
        mov     [@@HighPart], 0
        cmp     [@@Method], Method_CHS
        je      @@ReadPartCHS
        cmp     [@@Method], Method_CHSExt
        je      @@ReadPartCHSExt
        cmp     [@@Method], Method_ASPI
        je      @@ReadPartASPI
        jmp     @@ReadPartExt

; Read partition table using CHS
@@ReadPartCHS:
        call    NEAR ReadSectorCHS PASCAL, [@@PartLBA],[@@Drive],[@@nHeads],[@@nSecs],es (OFFSET Buf1)
        jc      @@errReadError
        jmp     @@PartTableRead

; Read partition table using extended CHS
@@ReadPartCHSExt:
        call    NEAR ReadSectorExtCHS PASCAL, [@@PartLBA],[@@Drive],[@@nHeads],[@@nSecs],es (OFFSET Buf1)
        jc      @@errReadError
        jmp     @@PartTableRead

; Read partition table using ASPI
@@ReadPartASPI:
        call    NEAR ReadSectorASPI PASCAL, [@@PartLBA],[@@Drive],[@@nHeads],[@@nSecs],es (OFFSET Buf1)
        jc      @@errReadError
        jmp     @@PartTableRead

; Read partition table using IBM/MS Extensions
@@ReadPartExt:
        mov     [DiskAddrPkt1.Count], 1
        mov     eax, [@@PartLBA]
        mov     [DWORD LOW DiskAddrPkt1.Sector], eax
        mov     [WORD LOW DiskAddrPkt1.Buffer], OFFSET Buf1
        mov     [WORD HIGH DiskAddrPkt1.Buffer], SEG Buf1
        mov     dl, [BYTE LOW @@Drive]
        mov     si, OFFSET DiskAddrPkt1
        mov     ah, 42h
        int     13h
        jc      @@errReadError

@@PartTableRead:

; Check partition table signature
        cmp     [WORD Buf1+510], 0AA55h
        jne     @@errBadPartTable

        mov     bx, [@@PartOfs]
        cmp     [BYTE Buf1+bx+04h], 0           ; Unused entry
        jz      @@NextEntry
; Extract CHS information
        movzx   ax, [Buf1+bx+01h]
        mov     [@@sHead], ax
        movzx   ax, [Buf1+bx+02h]
        mov     [@@sSec], ax
        and     [@@sSec], 3Fh
        shl     ax, 2
        mov     al, [Buf1+bx+03h]
        mov     [@@sCyl], ax

        movzx   ax, [Buf1+bx+05h]
        mov     [@@eHead], ax
        movzx   ax, [Buf1+bx+06h]
        mov     [@@eSec], ax
        and     [@@eSec], 3Fh
        shl     ax, 2
        mov     al, [Buf1+bx+07h]
        mov     [@@eCyl], ax
; Calculate partition size from CHS information to determine if high
        movzx   eax, [@@eCyl]
        sub     ax, [@@sCyl]
        mul     [@@nHeads]
        shl     edx, 16
        add     eax, edx
        movzx   edx, [@@nSecs]
        mul     edx
        mov     ecx, eax
        movzx   eax, [BYTE LOW @@eHead]
        sub     al, [BYTE LOW @@sHead]
        mul     [BYTE LOW @@nSecs]
        add     al, [BYTE LOW @@eSec]
        adc     ah, 0
        sub     al, [BYTE LOW @@sSec]
        sbb     ah, 0
        add     ecx, eax
        inc     ecx                     ; ECX = partition size
        cmp     ecx, [DWORD Buf1+bx+0Ch]
        je      @@PartSizeDone
        mov     [@@HighPart], 1
        cmp     ecx, 1
        je      @@PartSizeDone
        cmp     [Buf1+bx+04h], 05h
        jne     @@PartSizeDone
        mov     [@@HighPart], 0         ; Extended partition, partially high
@@PartSizeDone:

        mov     al, [Buf1+bx+04h]       ; Partition type
        cmp     al, 05h
        je      @@ExtPart
        cmp     al, 0fh
        je      @@ExtPart
        cmp     al, 07h
        je      @@HPFSPart
        cmp     al, 17h
        je      @@HPFSPart
        jmp     @@NextEntry

@@ExtPart:
        mov     eax, [DWORD Buf1+bx+08h]
        add     eax, [ExtPartBase]      ; Relative to first extended partition
        mov     [@@sLBA], eax           ; LBA of partition
        cmp     [ExtPartBase], 0
        jnz     @@ExtBaseDone
        mov     [ExtPartBase], eax      ; This is the first ext part
@@ExtBaseDone:

        cmp     [@@Method], Method_ASPI
        je      @@ExtPartASPI

        cmp     [@@HighPart], 0
        jnz     @@ExtPartHigh

        call    NEAR ScanPartTbl, Method_CHS,[@@Drive],[@@nHeads],[@@nSecs],[@@IBM_MS_Ext],[@@sCyl],[@@sHead],[@@sSec],[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry
@@ExtPartHigh:
        cmp     [@@IBM_MS_Ext], 0
        jnz     @@ExtPartIBMExt
        cmp     [UseExtCHS], 0
        jz      @@NextEntry
        test    [BYTE LOW @@nHeads], 0C0h
        jnz     @@NextEntry             ; Must have at most 63 heads
        call    NEAR CheckCylNumber, [@@sLBA], [@@nHeads], [@@nSecs]
        jnz     @@NextEntry
        call    NEAR ScanPartTbl, Method_CHSExt,[@@Drive],[@@nHeads],[@@nSecs],[@@IBM_MS_Ext],0,0,0,[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry
@@ExtPartIBMExt:
        call    NEAR ScanPartTbl, Method_Ext,[@@Drive],[@@nHeads],[@@nSecs],[@@IBM_MS_Ext],0,0,0,[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry

@@ExtPartASPI:
        call    NEAR ScanPartTbl, [@@Method],[@@Drive],[@@nHeads],[@@nSecs],[@@IBM_MS_Ext],0,0,0,[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry

@@HPFSPart:
        mov     eax, [@@PartLBA]        ; Relative to current ext part
        add     eax, [DWORD Buf1+bx+08h]
        mov     [@@sLBA], eax           ; LBA of partition

        cmp     [@@Method], Method_ASPI
        je      @@HPFSPartASPI

        cmp     [@@HighPart], 0
        jnz     @@HPFSPartHigh
        call    NEAR CheckHPFSPart, Method_CHS,[@@Drive],[@@nHeads],[@@nSecs],[@@sCyl],[@@sHead],[@@sSec],[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry
@@HPFSPartHigh:
        cmp     [@@IBM_MS_Ext], 0
        jnz     @@HPFSPartIBMExt
        cmp     [UseExtCHS], 0
        jz      @@NextEntry
        test    [BYTE LOW @@nHeads], 0C0h
        jnz     @@NextEntry             ; Must have at most 63 heads
        mov     eax, [@@sLBA]           ; Check that we can read last sector
        add     eax, [DWORD Buf1+bx+0Ch]
        dec     eax
        call    NEAR CheckCylNumber, eax, [@@nHeads], [@@nSecs]
        jnz     @@NextEntry
        call    NEAR CheckHPFSPart, Method_CHSExt,[@@Drive],[@@nHeads],[@@nSecs],0,0,0,[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry
@@HPFSPartIBMExt:
        call    NEAR CheckHPFSPart, Method_Ext,[@@Drive],[@@nHeads],[@@nSecs],0,0,0,[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry

@@HPFSPartASPI:
        call    NEAR CheckHPFSPart, [@@Method],[@@Drive],[@@nHeads],[@@nSecs],0,0,0,[@@sLBA]
        jc      @@Done
        jmp     @@NextEntry

@@NextEntry:
        add     [@@PartOfs], 10h
        cmp     [@@PartOfs], 1FEh
        jb      @@DoEntry
        clc
        jmp     @@Done

@@errReadError:
        mov     dx, OFFSET MsgDiskError
        mov     ah, 9
        int     21h
        stc
        jmp     @@Done

@@errBadPartTable:
        mov     dx, OFFSET MsgBadPartTable
        mov     ah, 9
        int     21h
        stc
        ; FALL THROUGH to @@Done

@@Done:
        ret
ENDP    ScanPartTbl

;---------------------------------------------------------------------
; Checks and possibly installs an HPFS partition. Uses Buf2
; Args: @@Method        Disk access method to use
;       @@PhysDrive     Drive number
;       @@nHeads        Number of heads (logical)
;       @@nSecs         Number of sectors (logical)
;       @@sCyl          Starting cylinder
;       @@sHead         Starting head
;       @@sSec          Starting sector
;       @@sLBA          Starting LBA
; Returns CF=1 if a critical error occurs.
PROC    CheckHPFSPart PASCAL
        ASSUME  ds:DSeg,es:ResCode,fs:ResCode,gs:NOTHING
        ARG     @@Method,@@PhysDrive,@@nHeads,@@nSecs,@@sCyl,@@sHead,@@sSec,@@sLBA:DWORD
        LOCAL   @@Drive:BYTE
        push    ds es fs gs
        pushad
        cmp     [@@Method], Method_CHS
        je      @@ReadCHS
        cmp     [@@Method], Method_CHSExt
        je      @@ReadCHSExt
        cmp     [@@Method], Method_ASPI
        je      @@ReadASPI
        jmp     @@ReadExt

; Read boot sector using CHS
@@ReadCHS:
        call    NEAR ReadSectorCHS PASCAL, [@@sLBA],[@@PhysDrive],[@@nHeads],[@@nSecs],es (OFFSET Buf2)
        jc      @@errReadError
        jmp     @@BootRead

; Read boot sector using extended CHS
@@ReadCHSExt:
        call    NEAR ReadSectorExtCHS PASCAL, [@@sLBA],[@@PhysDrive],[@@nHeads],[@@nSecs],es (OFFSET Buf2)
        jc      @@errReadError
        jmp     @@BootRead

@@ReadASPI:
        call    NEAR ReadSectorASPI PASCAL, [@@sLBA],[@@PhysDrive],[@@nHeads],[@@nSecs],es (OFFSET Buf2)
        jc      @@errReadError
        jmp     @@BootRead

; Read boot sector using IBM/MS Extensions
@@ReadExt:
        mov     [DiskAddrPkt1.Count], 1
        mov     eax, [@@sLBA]
        mov     [DWORD LOW DiskAddrPkt1.Sector], eax
        mov     [WORD LOW DiskAddrPkt1.Buffer], OFFSET Buf2
        mov     [WORD HIGH DiskAddrPkt1.Buffer], SEG Buf2
        mov     dl, [BYTE LOW @@PhysDrive]
        mov     si, OFFSET DiskAddrPkt1
        mov     ah, 42h
        int     13h
        jc      @@errReadError

@@BootRead:
; Check the HPFS signature
        cmp     [DWORD Buf2+36h], 'SFPH'
        jne     @@Done
; HPFS partition found.
        inc     [PartCount]
        movzx   bx, [PartCount]
        mov     dl, [Partitions+bx]
        mov     [@@Drive], dl
        mov     [Partitions+bx], 0FFh           ; Partition found
        cmp     dl, 0FFh
        je      @@Done                          ; Don't install this partition
; Check that partition is not already installed
        call    NEAR IsInstalledPart, bx
        or      ah, ah
        jnz     @@errPartInstalled

        les     bx, [LstOfLst]
        ASSUME  es:NOTHING
        les     si, [es:bx+16h]         ; CDS array
        cmp     [@@Drive], 0FEh
        je      @@ScanCDS               ; Find first free drive letter
        mov     ah, 36h                 ; Get disk free space
        mov     dl, [@@Drive]
        inc     dl
        int     21h
        cmp     ax, 0FFFFh
        jne     @@errDrvUsed
        mov     al, [BYTE CDSSize]
        mov     cl, [@@Drive]
        mul     cl
        mov     si, ax                  ; Points to CDS for our drive
        cmp     cl, [LastDrive]
        jb      @@FoundCDS
        jmp     @@errInvDrv

; Search the CDS array, look for an unused CDS.
; ES:SI -> CDS.
@@ScanCDS:
        mov     [@@Drive], 0
@@ScanCDS1:
        cmp     [DrDos], 0
        jz      @@ScanCDS2
        cmp     [WORD es:si+43h], 0     ; DR-DOS
        jmp     @@ScanCDS3
@@ScanCDS2:
        test    [WORD es:si+43h], 0C000h  ; Mask bits 15 and 14 of drv attributes
@@ScanCDS3:
        jz      @@FoundCDS                ; If 0, then drive is invalid = free
        add     si, [CDSSize]             ; Point to next CDS
        inc     [@@Drive]
        mov     cl, [@@Drive]
        cmp     cl, [LastDrive]
        jb      @@ScanCDS1                ; Go to next CDS entry
        jmp     @@errOutOfDrv

@@FoundCDS:
; Allocate memory for resident data
        mov     bx, OFFSET EndResData
        shr     bx, 4
        inc     bx
        mov     ah, 48h
        int     21h
        jc      @@errMallocErr
        movzx   bx, [@@Drive]
        shl     bx, 1
        mov     [DataSegs+bx], ax       ; Save ResData segment
        mov     gs, ax
        ASSUME  gs:ResData
; Clear resident data.
        push    es
        dec     ax
        mov     es, ax

        mov     ax, [PSP]               ; PSP of resident iHPFS
        mov     [es:1], ax              ; owner
        xor     al, al
        cld
        mov     cx, OFFSET EndResData
        add     cx, 0000fh
        and     cx, 0fff0h
        add     cx, 16-5
        mov     di, 5
        rep stosb
        mov     di, 8
        mov     ax,':A'
        add     al,[@@Drive]
        stosw
        pop     es
; Set resident data
        mov     al, [BYTE LOW @@PhysDrive]
        mov     [PhysDrv], al
        mov     ax, [@@nHeads]
        mov     [nHeads], ax
        mov     al, [BYTE LOW @@nSecs]
        mov     [nSecs], al
        mov     al, [PartCount]
        mov     [PartitionNr], al
        mov     al, [BYTE LOW @@Method]
        mov     [AccessMethod], al
        mov     eax, [@@sLBA]
        mov     [LBAstart], eax
        call    InitResData
; Set CDS fields
        mov     cl, [@@Drive]
        add     cl, 'A'
        mov     [MsgDrvLetter], cl
        cmp     [DrDos], 0
        jz      @@SetCDS
        mov     [WORD es:si+43h], 8000h ; DR-DOS
        jmp     @@CDSSet
@@SetCDS:
        or      [WORD es:si+43h], 0C000h ; Flags+Physical bits on = Netwrk drive
@@CDSSet:
        mov     [es:si], cl
        mov     [WORD es:si+1], '\:'
        mov     [BYTE es:si+3], 0
        mov     [WORD es:si+4Fh], 2 ; Offset of backslash
        mov     [Installed], 1          ; Drive successfully installed
; Partition installed - print message
        mov     dx, OFFSET MsgInstalled
        mov     ah, 9
        int     21h
        jmp     @@Done

@@errReadError:
        mov     dx, OFFSET MsgDiskError
        mov     ah, 9
        int     21h
        jmp     @@Done
@@errDrvUsed:
        mov     dl, [@@Drive]
        add     dl, 'A'
        mov     [MsgDrvUsedLetter], dl
        mov     dx, OFFSET MsgDrvUsed
        mov     ah, 9
        int     21h
        mov     [ErrSignaled], 1
        jmp     @@Done
@@errInvDrv:
        mov     dl, [@@Drive]
        add     dl, 'A'
        mov     [MsgInvDrvLetter], dl
        mov     dx, OFFSET MsgInvDrv
        mov     ah, 9
        int     21h
        mov     [ErrSignaled], 1
        jmp     @@Done
@@errPartInstalled:
        movzx   ax, [PartCount]
        mov     dl, 10
        div     dl
        or      al, al
        jz      @@errPartInstalled1     ; Leave 00h if first digit 0.
        add     al, 30h
@@errPartInstalled1:
        add     ah, 30h
        mov     [WORD MsgPartInstalledNr], ax
        mov     dx, OFFSET MsgPartInstalled
        mov     ah, 9
        int     21h
        mov     [ErrSignaled], 1
        jmp     @@Done
@@errOutOfDrv:
        mov     dx, OFFSET MsgNoAvailDrvLetter
        mov     ah, 9
        int     21h
        mov     [ErrSignaled], 1
        jmp     @@Fail
@@errMallocErr:
        mov     dx, OFFSET MsgMallocErr
        mov     ah, 9
        int     21h
        mov     [ErrSignaled], 1
        jmp     @@Fail

@@Fail: stc
        jmp     @@Exit
@@Done:
        clc
@@Exit:
        popad
        pop     gs fs es ds
        ret
ENDP    CheckHPFSPart
;---------------------------------------------------------------------
; Read a sector given by LBA using ASPI manager
PROC    ReadSectorASPI PASCAL
        ASSUME  ds:DSeg,es:ResCode,fs:ResCode,gs:NOTHING
        ARG     @@Sector:DWORD, @@Drive:WORD, @@nHeads:WORD, @@nSecs:WORD, @@Buf:DWORD
        pushad

        mov     [SCSIRequestBlock.ccb_07_count],00100h
        mov     [SCSIRequestBlock.data_allocation_length],512
        mov     eax,[@@Buf]
        mov     [SCSIRequestBlock.data_buffer_pointer],eax
        mov     eax,[@@Sector]
        xchg    al,ah  ; 1 2 4 3
        rol     eax,16 ; 4 3 1 2
        xchg    al,ah  ; 4 3 2 1
        mov     [SCSIRequestBlock.ccb_02_lba],eax

        mov [SCSIRequestBlock.request_number],2
        mov [SCSIRequestBlock.request_status],0
        mov [SCSIRequestBlock.host_adapter_id],0 ;?
        mov [SCSIRequestBlock.request_flags],8
        mov ax, [@@Drive]
        mov [SCSIRequestBlock.target_id],al
        mov [SCSIRequestBlock.logical_unit],0 ;?
        mov [SCSIRequestBlock.sense_allocation_length],0
        mov [SCSIRequestBlock.CDB_length],10
        mov [SCSIRequestBlock.host_adapter_status],0ffh
        mov [SCSIRequestBlock.target_status],0ffh

        push    es
        push    offset SCSIRequestBlock
        call    [SCSIMGR]
        add     sp,2*2
        @@wait_scsi:
        cmp     [SCSIRequestBlock.request_status],0
        je      @@wait_scsi

        cmp     [SCSIRequestBlock.request_status],1
        jne     @@Fail

        cmp     [SCSIRequestBlock.target_status],0
        clc
        je      @@Done
  @@Fail:
        stc
  @@Done:
        popad
        ret
ENDP    ReadSectorASPI
;---------------------------------------------------------------------
; Read a sector given by LBA using CHS addressing.
PROC    ReadSectorCHS PASCAL
        ASSUME  ds:DSeg,es:ResCode,fs:ResCode,gs:NOTHING
        ARG     @@Sector:DWORD, @@Drive:WORD, @@nHeads:WORD, @@nSecs:WORD, @@Buf:DWORD
        pushad
        push    es
        mov     ecx, [@@Sector]
        mov     ax, [@@nSecs]
        mul     [@@nHeads]
        mov     bx, ax          ; Sectors/track * heads
        mov     ax, cx
        mov     edx, ecx
        shr     edx, 16         ; DX:AX = logical sector #
        div     bx
        push    ax              ; Cylinder
        mov     ax, dx
        div     [BYTE LOW @@nSecs]
        movzx   dx, al          ; Head
        mov     cl, ah          ; Sector
        inc     cl
        pop     ax              ; Cylinder
        mov     dh, dl          ; Head
        mov     ch, al
        xor     al, al
        shr     ax, 2
        or      cl, al          ; bits 8 and 9 of cyl. number go here
        xor     al, al

        mov     ax, 0201h
        les     bx, [@@Buf]
        mov     dl, [BYTE LOW @@Drive]
        int     13h
        pop     es
        popad
        ret
ENDP    ReadSectorCHS

;---------------------------------------------------------------------
; Read a sector given by LBA using extended CHS addressing.
PROC    ReadSectorExtCHS PASCAL
        ASSUME  ds:DSeg,es:ResCode,fs:ResCode,gs:NOTHING
        ARG     @@Sector:DWORD, @@Drive:WORD, @@nHeads:WORD, @@nSecs:WORD, @@Buf:DWORD
        pushad
        push    es
        mov     ecx, [@@Sector]
        mov     ax, [@@nSecs]
        mul     [@@nHeads]
        mov     bx, ax          ; Sectors/track * heads
        mov     ax, cx
        mov     edx, ecx
        shr     edx, 16         ; DX:AX = logical sector #
        div     bx
        push    ax              ; Cylinder
        mov     ax, dx
        div     [BYTE LOW @@nSecs]
        movzx   dx, al          ; Head
        mov     cl, ah          ; Sector
        inc     cl
        pop     ax              ; Cylinder
        mov     dh, dl          ; Head
        mov     ch, al
        xor     al, al
        shr     ax, 2
        or      cl, al          ; bits 8 and 9 of cyl. number go here
        xor     al, al
        shr     ax, 2
        or      dh, al          ; bits 10 and 11 of cyl (BIOS extension)

        mov     ax, 0201h
        les     bx, [@@Buf]
        mov     dl, [BYTE LOW @@Drive]
        int     13h
        pop     es
        popad
        ret
ENDP    ReadSectorExtCHS

;---------------------------------------------------------------------
; Determines the cylinder number of a given logical sector and returns
; ZF=1 if it is less than 4096, which means it can be read with
; extended CHS addressing.
PROC    CheckCylNumber PASCAL
        ASSUME  ds:DSeg,es:ResCode,fs:ResCode,gs:NOTHING
        ARG     @@Sector:DWORD, @@nHeads:WORD, @@nSecs:WORD
        pushad
        mov     ecx, [@@Sector]
        mov     ax, [@@nSecs]
        mul     [@@nHeads]
        mov     bx, ax          ; Sectors/track * heads
        mov     ax, cx
        mov     edx, ecx
        shr     edx, 16         ; DX:AX = logical sector #
        div     bx
        test    ax, 0F000h
        popad
        ret
ENDP    CheckCylNumber

;---------------------------------------------------------------------
; Initialize resident data. Assumes GS=ResData
PROC    InitResData STDCALL
        ASSUME  ds:ResCode,es:ResData,fs:NOTHING,gs:ResData
        LOCAL   @@BitmapTable:DWORD
        pushad
        push    ds es fs gs

        ;mov     ax, ResCode
        call    SetAXResCode
        mov     ds, ax
        push    gs
        pop     es
; Read the boot sector
        call    FAR ThunkReadSector PASCAL, LARGE 0, 1, ds (OFFSET Buf1)
        jc      @@ReadError
        mov     cx, 11
        mov     si, OFFSET Buf1+2Bh
        mov     di, OFFSET Volabel
        rep movsb
        mov     al, [BYTE Buf1+15h]             ; Media ID byte
        mov     [MediaID], al
        mov     si, OFFSET Buf1
        mov     cx, 512
        call    CRC16T
        mov     [BSCRC], dx
; Read the SuperBlock
        call    FAR ThunkReadSector PASCAL, LARGE 16, 1, ds (OFFSET Buf1)
        jc      @@ReadError
        mov     eax, [DWORD Buf1+0Ch]   ; Root dir fnode
        mov     [CDFNode], eax
        mov     [RootFNode], eax
        mov     eax, [DWORD Buf1+10h]   ; Partition size in sectors
        mov     [TotalSectors], eax
        mov     eax, [DWORD Buf1+18h]
        mov     [@@BitmapTable], eax
; Scan the free space bitmaps and count free sectors.
        mov     edx, [TotalSectors]
        add     edx, 3FFFh
        shr     edx, 14         ; Number of bands
        shl     dx, 2
        xor     bx, bx          ; offset into bitmap table
        xor     ecx, ecx        ; bit count

        push    ds
        mov     ax,DSeg
        mov     ds,ax
        ASSUME  ds:DSeg
        cmp     [ZeroBytesFree],1
        pop     ds
        ASSUME  ds:ResCode
        je      @@BitmapsDone
@@DoBand:
; Read free space bitmap table
        movzx   eax,bx          ; calc bitmap table sector
        shr     eax,9           ; 512=1 shl 9
        add     eax,[@@BitmapTable]
        call    FAR ThunkReadSector PASCAL, eax            , 1, ds (OFFSET Buf1)
        jc      @@ReadError
; Read free space bitmaps
        mov     si,bx           ; index modulo secor size
        and     si,(512-1)
        call    FAR ThunkReadSector PASCAL, [DWORD Buf1+si], 4, ds (OFFSET Buf1)
        jc      @@ReadError
        call    CountBits
        add     ecx, eax
        add     bx, 4
        cmp     bx, dx
        jne     @@DoBand
@@BitmapsDone:
        mov     [FreeSectors], ecx

        pop     gs fs es ds
        popad
        ret
@@ReadError:
        Abort   4
ENDP    InitResData

;---------------------------------------------------------------------
; Count number of set bits in Buf. Assumes DS=ResCode. Returns
; number of set bits in EAX.
PROC    CountBits
        ASSUME  ds:ResCode,es:NOTHING,fs:NOTHING,gs:NOTHING
        push    cx dx si
        xor     dx, dx
        mov     si, OFFSET Buf1
        mov     cx, 4*512
        cld
@@Bytes:
        lodsb
        push    cx
        mov     cx, 8
@@Bits: shr     al, 1
        adc     dx, 0
        loop    @@Bits
        pop     cx
        loop    @@Bytes
        movzx   eax, dx
        pop     si dx cx
        ret
ENDP    CountBits

;---------------------------------------------------------------------
; Write error message BX, release memory and exit
PROC    AbortMsg
        ASSUME  cs:CSeg,ds:DSeg,es:NOTHING,fs:ResCode,gs:NOTHING
        push    bx
        mov     ax, DSeg
        mov     ds, ax
        mov     ax, ResCode
        mov     fs, ax
        shl     bx, 1
        mov     dx, [ErrMsgTbl+bx]
        mov     ah, 9
        int     21h
; Deallocate XMS
        cmp     [CacheOn], 0
        ;jz      @@Exit
        je      @@1
        mov     [CacheOn], 0
        mov     cl, [XMSBlocks]
        xor     ch, ch
        jcxz    @@1
        mov     ah, 0Ah
        mov     dx, [hCacheSectors]
        CALL_XMS
        dec     cx
        jcxz    @@1
        mov     ah, 0Ah
        mov     dx, [hCacheLists]
        CALL_XMS
        dec     cx
        jcxz    @@1
        mov     ah, 0Ah
        mov     dx, [hHashTable]
        CALL_XMS
; Release allocated ResData blocks.
@@1:    xor     bx, bx
        mov     cx, MaxDrives
@@2:    sub     ax, ax
        xchg    ax, [DataSegs+bx]
        or      ax, ax
        jz      @@3
        mov     es, ax
        mov     ah, 49h
        push    bx cx
        int     21h
        pop     cx bx
@@3:    add     bx, 2
        loop    @@2
@@Exit:
        pop     ax
        mov     ah, 4Ch
        int     21h
ENDP    AbortMsg

;---------------------------------------------------------------------
; ax:=ResCode
PROC    SetAXResCode
        push    ds
        mov     ax, Dseg
        mov     ds, ax
        ASSUME  ds:Dseg
        mov     ax, [ResCodeSegment]
        or      ax, ax
        jnz     @@Ret
        mov     ax, ResCode
  @@Ret:
        pop     ds
        ret
ENDP    SetAXResCode
;---------------------------------------------------------------------
; [ResCodeSegment]:=0 if not installed
; [ResCodeSegment]:=ResCode if installed
; updates ApiFunc
PROC    GetResCode PASCAL
        pushad
        push    es
; Scan through INT 2D functions 00h-0FFh.
        mov     [ApiFunc], 0
@@CallMultiplex:
        mov     ah, [ApiFunc]
        xor     al, al          ; Installation check
        int     2Dh
        cmp     al, 0FFh
        jne     @@NextMultiplex
        mov     bx, ResCode     ; Compare signature strings
        mov     si, OFFSET AMISSign
        push    ds
        mov     ds, bx
        mov     es, dx
        mov     cx, 4
        cld
        repe cmpsd
        pop     ds
        je      @@Done ; Installed iHPFS found.
@@NextMultiplex:
        add     [ApiFunc], 1
        jnz     @@CallMultiplex

        sub     dx, dx
@@Done:
        mov     [ResCodeSegment], dx
        pop     es
        popad
        ret
ENDP    GetResCode

;---------------------------------------------------------------------
; Check if iHPFS is alredy loaded for a partition. Returns
; AH=00h if not installed, 01h if installed
; DL=drive number if partition installed.
PROC    IsInstalledPart PASCAL
        ARG     @@Part
        mov     ax, [ResCodeSegment]
        or      ax, ax
        jz      @@NotFound
; Installed iHPFS found.
        call    NEAR QueryPart, [@@Part]
        or      ah, ah
        jnz     @@Done          ; Partition found, exit.
@@NotFound:
        mov     ax, 000FFh      ; Partition not found.
        mov     dl, 0FFh
@@Done:
        ret
ENDP    IsInstalledPart

;---------------------------------------------------------------------
; Uninstall driver.
PROC    UninstallDriver PASCAL
        LOCAL   @@PSP
        ASSUME  ds:ResCode,es:NOTHING,fs:NOTHING,gs:NOTHING
        pushad
        push    ds es gs
; ResCode of driver to uninstall
        call    SetAXResCode
        mov     ds, ax
; Release XMS memory
        cmp     [CacheOn], 0
        jz      @@XMSReleased
        mov     [CacheOn], 0
        mov     ah, 0Ah
        mov     dx, [hHashTable]
        CALL_XMS
        mov     ah, 0Ah
        mov     dx, [hCacheLists]
        CALL_XMS
        mov     ah, 0Ah
        mov     dx, [hCacheSectors]
        CALL_XMS
@@XMSReleased:

; Disconnect drives
        mov     cx, -1
@@Remove:
        inc     cx
        cmp     cx, MaxDrives
        je      @@DrivesDisconnected
        mov     bx, cx
        shl     bx, 1
        cmp     [DataSegs+bx], 0
        jz      @@Remove
        call    NEAR RemoveDrv, cx
        jmp     @@Remove

@@DrivesDisconnected:
; Get PSP.
        mov     ah, 51h
        int     21h
        mov     [@@PSP], bx     ; Save old PSP
; Set PSP to resident driver. This is so the memory block retains its old owner.
        mov     bx, [PSP]
        mov     ah, 50h
        int     21h
        mov     es, bx
        ASSUME  es:NOTHING
; See if interrupt vectors have been hooked by another TSR
        xor     ax, ax
        mov     gs, ax
        call    SetAXResCode
        cmp     [WORD LOW DWORD gs:2Dh*4], OFFSET Int2DEntry
        jne     @@Resize
        cmp     [WORD HIGH DWORD gs:2Dh*4], ax
        jne     @@Resize
        cmp     [WORD LOW DWORD gs:2Fh*4], OFFSET Int2FEntry
        jne     @@Resize
        cmp     [WORD HIGH DWORD gs:2Fh*4], ax
        jne     @@Resize
; Restore interrupt vectors
        push    ds
        pop     gs
        ASSUME  gs:ResCode
        lds     dx, [OldInt2D]
        ASSUME  ds:NOTHING
        mov     ax, 252Dh
        int     21h
        lds     dx, [OldInt2F]
        mov     ax, 252Fh
        int     21h
        push    gs
        pop     ds
        ASSUME  ds:ResCode
; Release memory block
        mov     ah, 49h
        int     21h
        jmp     @@MemReleased
; Resize memory block
@@Resize:
        mov     bx, OFFSET EndUninstalledCode
        shr     bx, 4
        add     bx, 17          ; Paragraphs to keep (code+PSP+1)
        mov     ah, 4Ah
        int     21h
; Patch in far jump into driver's interrupt code to chain to original handler.
        mov     [WORD Int2DEntry], 0EA90h       ; NOP and JMP FAR
        mov     [WORD Int2FEntry], 0EA90h

@@MemReleased:
; Back to original PSP
        mov     bx, [@@PSP]
        mov     ah, 50h
        int     21h

        pop     gs es ds
        popad
        mov     al, 0FFh        ; ?
        ret
ENDP    UninstallDriver

;---------------------------------------------------------------------
; Return: AH=result
;       00h = Uninstalled
;       01h = Drv not installed
;       02h = Failed for other reason
PROC    DeleteChangedMediaDrive PASCAL
        ARG     @@Drive
        LOCAL   @@RetValue:WORD
        ASSUME  ds:ResCode,es:ResData,fs:NOTHING,gs:ResData
        pushad
        push    ds es gs

        mov     [@@RetValue],001ffh

        call    SetAXResCode
        mov     ds, ax

        mov     si, [@@Drive]
        shl     si, 1
        mov     ax, [DataSegs+si]
        or      ax, ax
        jz      @@NoDriveToCheck

        mov     es, ax
        mov     gs, ax

        sub     ax, ax
        xchg    al, [CacheOn]
        push    ax

        call    FAR ThunkReadSector PASCAL, LARGE 0, 1, ds (OFFSET Buf1)

        pop     ax
        mov     [CacheOn], al
        jc      @@RemoveChanged

        mov     si, OFFSET Buf1
        mov     cx, 512
        call    CRC16T
        cmp     [BSCRC], dx
        je      @@UnChanged

@@RemoveChanged:
        call    NEAR RemoveDrv, [@@Drive]
        mov     [@@RetValue],ax

@@UnChanged:
@@NoDriveToCheck:
        pop     gs es ds
        popad
        mov     ax, [@@RetValue]
        ret
ENDP    DeleteChangedMediaDrive

;---------------------------------------------------------------------
; Query logical drive connected.
; Return: AH=Install status (00h Not installed, 01h Installed)
PROC    QueryDrive PASCAL
        ARG     @@Drive
        ASSUME  ds:ResCode,es:NOTHING,fs:NOTHING,gs:NOTHING
        push    bx
        mov     ah, [ApiFunc]
        mov     al, 10h         ; Query drive
        mov     bx, [@@Drive]
        int     2Dh
        pop     bx
        ret
ENDP    QueryDrive

;---------------------------------------------------------------------
; Query HPFS partition connected.
; Return: AH=Install status (00h Not installed, 01h Installed)
;         DL=Drive letter associated with partition (if installed)
PROC    QueryPart PASCAL
        ARG     @@PartNum
        ASSUME  ds:ResCode,es:NOTHING,fs:NOTHING,gs:NOTHING
        push    bx cx dx
        push    ds es
        call    SetAXResCode
        mov     ds, ax
        mov     dx, -1
@@1:
        inc     dx
        cmp     dx, MaxDrives
        je      @@PartNotFound
        movzx   bx, dl
        shl     bx, 1
        mov     bx, [DataSegs+bx]
        or      bx, bx
        je      @@1
        mov     es, bx
        ASSUME  es:ResData
        mov     cl, [BYTE LOW @@PartNum]
        cmp     [PartitionNr], cl
        jne     @@1
        ASSUME  es:NOTHING
        mov     ax, 01FFh
        jmp     @@Done
@@PartNotFound:
        mov     ax, 00FFh
@@Done:
        pop     es ds
        pop     dx cx bx
        ret
ENDP    QueryPart

;---------------------------------------------------------------------
; Removes Cache entries for a drive
; DX=Drive Dataseg
PROC    RemoveDrvCache PASCAL
        ASSUME  ds:ResCode,es:NOTHING,fs:DSeg,gs:NOTHING

        pushad
        cmp     [CacheOn],1
        jne     @@Done

        ; lazy ... just change drive segment
        ; of each matching entry to 0

        mov     di, dx
        mov     [XMoveStruc.Length], CacheEntrySize
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.SourceHandle], ax
        and     [XMoveStruc.SourceOffset], 0
        and     [XMoveStruc.DestHandle], 0
        mov     [WORD XMoveStruc.DestOffset], OFFSET EntryBuf
        mov     [WORD (XMoveStruc.DestOffset)+2], ds

        mov     cx, [CacheEntries]
  @@RemoveCacheLoop:
        jcxz    @@RemoveCacheLoopEnd

        push    cx

        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError

; Examine the entry in buffer
        cmp     [WORD EntryBuf+0Ch], di ; Check if it's the right drive.
        jne     @@RemoveCacheLoopNext

        mov     [WORD EntryBuf+0Ch],0   ; "no drive"

        ; swap for save
        mov     eax, [XMoveStruc.SourceOffset]
        mov     dx, [XMoveStruc.SourceHandle]
        xchg    eax, [XMoveStruc.DestOffset]
        xchg    dx, [XMoveStruc.DestHandle]
        mov     [XMoveStruc.SourceOffset], eax
        mov     [XMoveStruc.SourceHandle], dx
        mov     si, OFFSET XMoveStruc
        mov     ah, 0Bh
        CALL_XMS
        jz      @@XMSError

        ; swap back
        mov     eax, [XMoveStruc.SourceOffset]
        mov     dx, [XMoveStruc.SourceHandle]
        xchg    eax, [XMoveStruc.DestOffset]
        xchg    dx, [XMoveStruc.DestHandle]
        mov     [XMoveStruc.SourceOffset], eax
        mov     [XMoveStruc.SourceHandle], dx

@@RemoveCacheLoopNext:
        add     [XMoveStruc.SourceOffset], CacheEntrySize
        pop     cx
        loop    @@RemoveCacheLoop

@@RemoveCacheLoopEnd:
        jmp     @@Done

@@XMSError:
        mov     [CacheOn],0

@@Done:
        popad
        ret
ENDP    RemoveDrvCache

;---------------------------------------------------------------------
; Remove a drive.
; Return: AH=result
;       00h = Uninstalled
;       01h = Drv not installed
;       02h = Failed for other reason
PROC    RemoveDrv PASCAL
        ARG     @@Drive
        LOCAL   @@RetValue
        ASSUME  ds:ResCode,es:NOTHING,fs:DSeg,gs:NOTHING
        pushad
        push    ds es fs

        mov     ax, DSeg
        mov     fs, ax
        call    SetAXResCode
        mov     ds, ax

; Release resident data segment
        movzx   bx, [BYTE LOW @@Drive]
        shl     bx, 1
        mov     dx, [DataSegs+bx]
        or      dx, dx
        jz      @@NotInstalled

        call    RemoveDrvCache

        mov     [DataSegs+bx], 0
        mov     es, dx
        mov     ah, 49h
        int     21h
; Patch CDS
        mov     ah, 52h
        int     21h             ; Get List of Lists in ES:BX
        les     bx, [es:bx+16h] ; CDS array
        mov     al, [BYTE LOW @@Drive]
        mov     dl, [BYTE CDSSize]
        mul     dl
        add     bx, ax          ; CDS entry for drive
        mov     [WORD es:bx+43h], 0
        mov     [@@RetValue], 0FFh
        jmp     @@Exit

@@NotInstalled:
        mov     [@@RetValue], 01FFh
@@Exit: pop     fs es ds
        popad
        mov     ax, [@@RetValue]
        ret
ENDP    RemoveDrv

;---------------------------------------------------------------------
; Parse command line.
PROC    ParseCmdLine STDCALL
        LOCAL   @@LastByte, @@CacheOpt:BYTE, @@DrvSpecd, @@DriveNo:BYTE
        LOCAL   @@PartNum:BYTE, @@UninstallOpt:BYTE
        ASSUME  ds:DSeg,es:NOTHING,fs:ResCode
        mov     [@@CacheOpt], 0         ; /C option flag
        mov     [@@UninstallOpt], 0     ; /U option flag
        mov     [@@DrvSpecd], 0

        ;mov     es, [PSP]
        mov     ah, 51h                 ; use PSP of current iHPFS
        int     21h
        mov     es, bx
        movzx   ax, [BYTE es:80h]
        cmp     al, 2
        jb      @@Done
        add     al, 80h
        mov     [@@LastByte], ax
; Find first non-space character
        mov     di, 81h
@@Next:
        mov     cx, [@@LastByte]
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
        jb      @@Parse1
        cmp     dl, HighestDriveLetter
        ja      @@Parse1
; Drive spec.
        inc     di
        mov     al, ':'                 ; Match a colon
        scasb
        jne     @@errBadOption
        sub     dl, 'A'                 ; Drive number
        mov     [@@DriveNo], dl
; Check for partition number '1'..'9'
        cmp     di, [@@LastByte]
        ja      @@SetDrvLetter          ; Save drive letter in list
        mov     al, [es:di]
        cmp     al, ' '
        je      @@SetDrvLetter
        cmp     al, '1'
        jb      @@errBadOption
        cmp     al, '9'
        ja      @@errBadOption
; Partition number
        mov     [Install], 1            ; Partition number - installing
        inc     di
        sub     al, '0'
        mov     [@@PartNum], al
        cmp     di, [@@LastByte]
        ja      @@SetPart
        mov     al, [es:di]             ; Second digit
        cmp     al, ' '
        je      @@SetPart
        cmp     al, '0'
        jb      @@errBadOption
        cmp     al, '9'
        ja      @@errBadOption
        inc     di
        sub     al, '0'
        mov     bl, [@@PartNum]
        shl     bl, 3                   ; Multiply by 10
        add     bl, [@@PartNum]
        add     bl, [@@PartNum]
        add     al, bl
        mov     [@@PartNum], al
        cmp     di, [@@LastByte]
        ja      @@SetPart
        mov     al, [es:di]
        cmp     al, ' '
        jne     @@errBadOption
@@SetPart:
        mov     [@@DrvSpecd], 1           ; Drive letter specified
        movzx   bx, [@@PartNum]
        mov     al, [@@DriveNo]
        cmp     [Partitions+bx], 0FEh
        jne     @@errPartUsed
        mov     [Partitions+bx], al
        jmp     @@Next
@@SetDrvLetter:
        movzx   bx, [@@DriveNo]
        mov     [UnInstallDrv+bx], 1    ; Uninstall this drive
        mov     [UnInstall], 1
        mov     [SpecUninstall], 1      ; Drive to uninstall specified
        jmp     @@Next

@@Parse1:
; Check for switches
        mov     al, '/'
        scasb
        jne     @@errBadOption
; Switch
        cmp     di, [@@LastByte]
        ja      @@errBadOption
        mov     dl, [es:di]
        inc     di
        mov     ax, 6520h               ; Upper case
        int     21h
        cmp     dl, 'B'
        je      @@BIOSExtension
        cmp     dl, 'C'
        je      @@CacheSize
        cmp     dl, 'U'
        je      @@UnInstall
        cmp     dl, 'L'
        je      @@ConvertLong
        cmp     dl, 'M'
        je      @@Multitrack
        cmp     dl, '0'
        je      @@ZeroBytesFree
        cmp     dl, 'A'
        je      @@EnableAspi
        cmp     dl, 'F'
        je      @@EnableFloppy
        cmp     dl, '2'
        je      @@Limit2GB
        jmp     @@errBadOption
; /B switch
@@BIOSExtension:
;       mov     [UseExtCHS], 1
;       cmp     di, [@@LastByte]
;       ja      @@Next
;       cmp     [BYTE es:di], ' '
;       jne     @@errBadOption
;       jmp     @@Next
;***
        cmp     di, [@@LastByte]
        ja      @@errBadOption
        mov     dl, [es:di]
        inc     di
        cmp     dl, '1'
        je      @@ExtCHS
        cmp     dl, '2'
        je      @@NoIBMExt
        jmp     @@errBadOption
@@ExtCHS:
        mov     [UseExtCHS], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next
@@NoIBMExt:
        mov     [NoIBMExt], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next

; /C switch
@@CacheSize:
        cmp     [@@CacheOpt], 0
        jnz     @@errBadOption          ; Only one /C option allowed
        cmp     [CacheOn], 1
        je      @@errCacheAlreadyInstalled
        mov     [@@CacheOpt], 1
        mov     [Install], 1          ; /C switch - installing
        cmp     di, [@@LastByte]
        je      @@errBadOption
        mov     al, '='
        scasb
        jne     @@errBadOption
        xor     eax, eax              ; AX=Converted number
@@GetDigit:
        cmp     di, [@@LastByte]
        ja      @@ConvDone
        cmp     [BYTE es:di], ' '
        je      @@ConvDone
        mov     bx, 10
        mul     bx
        or      dx, dx
        jne     @@errBadOption
        cmp     [BYTE es:di], '0'
        jb      @@errBadOption
        cmp     [BYTE es:di], '9'
        ja      @@errBadOption
        add     al, [es:di]
        adc     ah, 0
        jc      @@errBadOption
        sub     ax, 30h
        inc     di
        jmp     @@GetDigit
@@ConvDone:
        cmp     ax, MinCacheSize        ; Check that cache size is ok.
        jb      @@errBadOption
        cmp     ax, MaxCacheSize
        ja      @@errBadOption
        mov     ecx, eax
        shl     eax, 10                 ; Bytes
        mov     edx, eax
        shr     edx, 16                 ; Cache size in bytes in DX:AX
        mov     bx, 512+CacheEntrySize+2
        div     bx                      ; Cache entries
        mov     [CacheEntries], ax
        mov     [FreeEntry], ax
        dec     [FreeEntry]
        xor     dx, dx
        mov     bx, LoadFactor
        div     bx                      ; Hash table size=entries/load factor
        mov     [HashSize], ax
; Allocate the XMS memory blocks
        cmp     [XMSFound], 0
        je      @@errXMSFailure

        mov     dx, [CacheEntries]
        shr     dx, 1
        adc     dx, 0                   ; KB needed for the sectors
        mov     ah, 09h
        CALL_XMS
        jz      @@errAllocFailed
        mov     [hCacheSectors], dx
        inc     [XMSBlocks]

        mov     ax, [CacheEntries]
        mov     dx, CacheEntrySize
        mul     dx
        shl     edx, 16
        mov     dx, ax
        shr     edx, 10
        inc     dx
        mov     ah, 09h
        CALL_XMS
        jz      @@errAllocFailed
        mov     [hCacheLists], dx
        inc     [XMSBlocks]

        mov     dx, [HashSize]
        shr     dx, 9
        inc     dx                      ; KB needed for hash table
        mov     ah, 09h
        CALL_XMS
        jz      @@errAllocFailed
        mov     [hHashTable], dx
        inc     [XMSBlocks]
        mov     [CacheOn], 1
; Clear the hash table

        push    ds es
        push    di
        mov     ax, fs
        mov     ds, ax
        mov     es, ax
        ASSUME  ds:ResCode,es:ResCode
        mov     di, OFFSET Buf1
        mov     cx, 100h
        xor     eax, eax
        rep stosd                       ; Clear Buf1-Buf2

        mov     cx, [HashSize]
        shr     cx, 9
        inc     cx
        mov     [XMoveStruc.Length], 1024
        mov     [XMoveStruc.SourceHandle], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET Buf1
        mov     [WORD (XMoveStruc.SourceOffset)+2], ds
        mov     ax, [hHashTable]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.DestOffset], 0
@@InitCacheTbl:
        mov     ah, 0Bh
        mov     si, OFFSET XMoveStruc
        CALL_XMS
        add     [XMoveStruc.DestOffset], 1024
        or      ax, ax
        loopnz  @@InitCacheTbl
        jz      @@errXMSFailure
; Clear the sentinel, entry 0
        mov     ax, [hCacheLists]
        mov     [XMoveStruc.DestHandle], ax
        mov     [XMoveStruc.DestOffset], 0
        mov     [WORD XMoveStruc.SourceOffset], OFFSET Buf1
        mov     [XMoveStruc.Length], CacheEntrySize
        mov     ah, 0Bh
        CALL_XMS
        jz      @@errXMSFailure
        pop     di
        pop     es ds
        ASSUME  ds:DSeg,es:NOTHING
        jmp     @@Next
; /U switch
@@UnInstall:
        mov     [UnInstall], 1          ; /U switch - uninstall
        mov     [@@UnInstallOpt], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next
; /L switch
@@ConvertLong:
        mov     [ConvertLong], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next

@@Multitrack:
        mov     [Multitrack], 0
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next

@@ZeroBytesFree:
        mov     [ZeroBytesFree], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next

@@EnableAspi:
        mov     [EnableAspi], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next

@@EnableFloppy:
        mov     [EnableFloppy], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next

@@Limit2GB:
        mov     [Limit2GB], 1
        cmp     di, [@@LastByte]
        ja      @@Next
        cmp     [BYTE es:di], ' '
        jne     @@errBadOption
        jmp     @@Next

@@errBadOption:
        Abort   7
@@errPartUsed:
        Abort   17              ; Partition already specified once
@@errXMSFailure:
        Abort   10
@@errAllocFailed:
        cmp     bl, 0A0h
        jne     @@errAlloc1
        Abort   9               ; Out of XMS
@@errAlloc1:
        cmp     bl, 0A1h
        jne     @@errAlloc2
        Abort   13              ; Out of XMS handles
@@errAlloc2:
        Abort   10
@@errCacheAlreadyInstalled:
        Abort   20              ; XMS already allocated
@@Done:
        mov     al, [UnInstall]
        xor     al, [@@UnInstallOpt]
        jnz     @@errBadOption  ; Can't specify "/U" xor "d:"
        mov     al, [Install]
        test    al, [UnInstall]
        jnz     @@errBadOption  ; Can't both install and uninstall
        or      al, al
        jz      @@Exit
        cmp     [@@DrvSpecd], 0
        jz      @@Exit
; Mark unwanted partition numbers
        mov     bx, -1
@@MarkPart:
        inc     bx
        cmp     bx, MaxDrives
        ja      @@Exit
        cmp     [Partitions+bx], 0FEh
        jne     @@MarkPart
        mov     [Partitions+bx], 0FFh
        jmp     @@MarkPart
@@Exit:
        ret
ENDP    ParseCmdLine

;---------------------------------------------------------------------
; Compute CRC16 of a string. DS:SI points at the buffer, CX is the
; length of the buffer. CRC16 returned in DX.
; this is a copy from ResCode

PROC    CRC16T
        NOASSUME
        ASSUME  cs:CSeg
        push    ax bx
        pushf
        cld
        xor     dx, dx
@@1:    lodsb
        xor     ah, ah
        xchg    ah, al
        xor     dx, ax
        push    cx
        mov     cx, 8
@@2:    mov     bx, dx
        shl     dx, 1
        and     bx, 8000h
        jz      @@3
        xor     dx, 1021h
@@3:    loop    @@2
        pop     cx
        loop    @@1
        popf
        pop     bx ax
        ret
ENDP    CRC16T


ENDS    CSeg

;--------------------------------------------- Data for transient section
SEGMENT DSeg
MsgHello        DB "iHPFS     An installable HPFS driver for DOS       "
                DB 'Version 1.29  2000-07-19',13,10
                DB "Copyright (C) 1993-1998, Marcus Better.",13,10
                DB 13,10,13,10,"$"
MsgInstalled    DB "Installed as "
MsgDrvLetter    DB 0, ":",13,10,"$"
MsgUnInstalled  DB "Driver uninstalled.",13,10,"$"
MsgDrvRemoved   DB "Removed drive "
MsgDrvRemovedLetter DB "A:",13,10,"$"
DiskNumber DB   80h     ; Hard disk number
PartCount DB    0       ; Partition counter
Partitions DB   (MaxDrives+1) DUP(0FEh)
                        ; Maps partitions to drive letters.
                        ; 0FFh=don't install, 0FEh=first free drv letter.
XMSFound DB     0       ; Flag: XMS Found?
XMSBlocks DB    0       ; XMS blocks allocated
Install DB      0       ; Install iHPFS
UnInstall DB    0       ; Uninstall iHPFS. Error if both Install AND UnInstall
UnInstallDrv DB MaxDrives DUP(0) ; Uninstall drive if set.
SpecUninstall DB 0      ; Set if specific drives are to be uninstalled.
DriverLoaded DB 0       ; Set if iHPFS loaded (during uninstall)
Installed DB    0       ; Set if any drive successfully installed,
                        ; or if any uninstalled.
ErrSignaled DB  0       ; Error message displayed in ScanPartTbl
ExtPartBase DD  0       ; Base of extended partition offsets
UseExtCHS DB    0       ; Use extended CHS addressing
NoIBMExt DB     0       ; Do not use IBM/MS Extensions
DiskAddrPkt1 DiskAddrPacketStruct <>

; DOS info
LstOfLst DD     0       ; Address of List of Lists
LastDrive DB    0
DrDos   DB      0       ; Nonzero if DR-DOS (zero if Novell DOS)
CDSSize DW      58h     ; Size of CDS entry

ZeroBytesFree DB 0      ; skip free disk block calculation
EnableAspi    DB 0      ; ask ASPI driver (removable devices)
EnableFloppy  DB 0      ; look for HPFS media in A:/B:

ResCodeSegment DW 0     ; ResCode of installed iHPFS

; Error message table
ErrMsgTbl DW    MsgBadDOSVer            ; Error 0
        DW      MsgMallocErr            ; Error 1
        DW      MsgNoAvailDrvLetter     ; Error 2
        DW      MsgNoSDA                ; Error 3
        DW      MsgDiskError            ; Error 4
        DW      MsgNoPart               ; Error 5
        DW      MsgDrvUsed              ; Error 6
        DW      MsgBadCmdLine           ; Error 7
        DW      MsgInvDrv               ; Error 8
        DW      MsgOutOfXMS             ; Error 9
        DW      MsgXMSAllocFailed       ; Error 10
        DW      MsgCantUninstall        ; Error 11
        DW      MsgCantRmDrv            ; Error 12
        DW      MsgOutOfXMSHandles      ; Error 13
        DW      MsgNoMux                ; Error 14
        DW      MsgNotLoaded            ; Error 15
        DW      MsgDrvNotInst           ; Error 16
        DW      MsgOneDrivePerPart      ; Error 17
        DW      MsgPartNotFound         ; Error 18
        DW      MsgPartInstalled        ; Error 19
        DW      MsgCacheAlreadyInstalled; Error 20

; Error messages
MsgBadDOSVer    DB      "Wrong DOS version.",13,10,"$"
MsgMallocErr    DB      "Memory allocation error.",13,10,"$"
MsgNoAvailDrvLetter DB  "Out of drive letters.",13,10,"$"
MsgNoSDA        DB      "Compatibility error.",13,10,"$"
MsgDiskError    DB      "Error reading disk.",13,10,"$"
MsgNoPart       DB      "Cannot find HPFS partition.",13,10,"$"
MsgDrvUsed      DB      "Drive "
MsgDrvUsedLetter DB     "A: already in use.",13,10,"$"
MsgInvDrv       DB      "Invalid drive letter "
MsgInvDrvLetter DB      "A:",13,10,"$"
MsgBadCmdLine   DB      "Invalid command line arguments.",13,10
                DB      "Syntax: IHPFS [options] [d:n d:n ...]",13,10
                DB      "        IHPFS /U [d:]",13,10
                DB      "where d is a drive letter and n is the number of the "
                DB      "HPFS partition.",13,10,13,10
                DB      "Options:",13,10
                DB      "/B1          Use more BIOS extensions to access high partitions.",13,10
                DB      "/B2          Do not use IBM/MS Extensions. (AWARD AT, AMI 386sx)",13,10
                DB      "/C=x         Allocate x KB for cache. Must be between 32 and 32768.",13,10
                DB      "/L           Convert long filenames.",13,10
                DB      "/M           Disable multitrack operations.",13,10
                DB      "/U           Uninstall driver.",13,10
                DB      "/0           Report 0 bytes free. Decreases install time.",13,10
                DB      "/A           Use ASPI manager. Access to popular removable media.",13,10
                DB      "/F           Search for HPFS floppy.",13,10
                DB      "/2           Let DiskSize/DiskFree report < 2GB.",13,10
                DB      "$"
MsgOutOfXMS     DB      "Out of XMS memory.",13,10,"$"
MsgXMSAllocFailed DB    "Cannot allocate XMS memory.",13,10,"$"
MsgCantUninstall DB     "Cannot unload driver.",13,10,"$"
MsgCantRmDrv    DB      "Couldn't uninstall drive "
MsgCantRmDrvLetter DB   "A:",13,10,"$"
MsgOutOfXMSHandles DB   "Out of XMS handles.",13,10,"$"
MsgNoMux        DB      "No free multiplex function found.",13,10,"$"
MsgNotLoaded    DB      "iHPFS not loaded.",13,10,"$"
MsgDrvNotInst   DB      "iHPFS is not installed for drive "
MsgDrvNotInstLetter DB "A:",13,10,"$"
MsgOneDrivePerPart DB   "Cannot install two iHPFS drives for the same partition.",13,10,"$"
MsgPartNotFound DB      "Partition "
MsgPartNotFoundNr DB    " 0 not found.",13,10,"$"
MsgPartInstalled DB     "iHPFS already installed for partition "
MsgPartInstalledNr DB   " 0.",13,10,"$"
MsgBadPartTable DB      "Bad partition table signature.",13,10,"$"
MsgCacheAlreadyInstalled DB "Resident iHPFS has already allocated XMS Cache",13,10,"$"

scsimgr_name    DB      "SCSIMGR$",0
MsgHDScan       DB      "Scanning Hard Disk "
MsgHDScan1      DW      "80"
                DB      13,"$"
MsgAspiScan     DB      "Scanning ASPI target "
MsgAspiScan1    DB      "0"
                DB      13,"$"
MsgFloppyScan   DB      "Scanning Floppy 0"
MsgFloppyScan1  DB      "0"
                DB      13,"$"
clearline       DB      13,"                           ",13,"$"

ENDS    DSeg

SEGMENT SSeg    STACK
        ;DB      128 DUP('STACK---')
        DB      150 DUP('STACK---')
ENDS    SSeg

        END     Main
