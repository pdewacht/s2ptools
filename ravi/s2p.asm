; adapted for DreamBlaster S2P by pdewacht@gmail.com
; 8086-fixed by trixter@oldskool.org on 20211030.
; Original header follows:
;
; GM on MPU-401 driver for SCI0
; plays the MT-32 channels using Rickard's MIDI Mapping Magic
;    (see http://freesci.linuxgames.com)
; assembles with NASM (see http://nasm.sourceforge.net):
;    nasm s2p.asm -o s2p.drv -f bin

; Copyright (c) 1999, 2000, 2002, 2003, 2005 by
;    Rickard Lind
;    Christoph Reichenbach
;    Ravi Iyengar [ravi.i@rarefied.org]
;    Andy Hefner
;
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

; Additionally, there is specific permission given by the authors
; to allow linking with Sierra's SCI0 interpreter. (Which isn't
; free software and is otherwise disallowed by the GPL.)


[BITS 16]
[CPU 8086]

; Drivers are flatform binaries that get loaded at the start of
; a segment. When a driver function needs to be executed, the
; interpreter does a far call to offset 0. The first thing must
; be a jump to a DriverInterface function because the interpreter
; expects certain data starting at offset 3, and there isn't
; space to fit in anything else.

jmp near DriverInterface

; identifiers for a Sierra sound driver
db 0                   ; uncertain
dd 0x87654321          ; Sierra driver id
db 1                   ; sound driver id (0=display, 4=keyboard)

; a string identifying the driver
db 6, 's2pdrv'

; a string with a description of the device
db 16, 'DreamBlaster S2P'



;--------------------------------------------------------------------------
; misc macros and defines

; device information returned by GetDeviceInfo
%define  INFO_PATCH  1       ; patch resource number (0xFFFF means none)
%define  INFO_POLY   32      ; maximum polyphony

; the play flag for this device (used in the sound header)
%define  PLAY_FLAG   1

; playstate constants
%define  STOPPED     0
%define  PLAYING     1
%define  SEEKING     2

; offsets in the sound info structure
%define  SND_RESPTR  8       ; heap pointer to far pointer to resource data
%define  SND_UNK     10      ; zeroed to fix a CB fadeout bug
%define  SND_POS     12      ; current play offset in sound resource
%define  SND_STATE   16      ; 1 = valid, 3 = invalid   (possibly others)
%define  SND_SIGNAL  22      ; the Sound object's signal property
%define  SND_VOLUME  24      ; global sound volume

; MT-32 to GM instrument map structure
%define GM_INSTR       0
%define KEYSHIFT       1
%define FINETUNE       2
%define BENDER_RANGE   4
%define GM_RHYTHMKEY   5
%define VOLUME         6

; Mapping codes
%define MAP_NOT_FOUND   255
%define NOMAP           254
%define RHYTHM          253



;--------------------------------------------------------------------------
; data area
; Sierra's drivers put all their data space in the beginning, though
;    there's no reason why it has to be that way.

ptrparam:      dw 0          ; saves the original si parameter to DriverInterface
playstate:     db 0          ; 0 = not playing sound, 1 = playing sound, 2 = seeking sound
fadeticks:     db 0          ; number of ticks to next volume decrement, 0 = not fading
fadevolume:    db 0          ; the volume setting during fades
playflags:     dw 0          ; specifies whether or not each channel is used by this device
waitcount:     dw 0          ; number of ticks left before next event
sndresptr:     dw 0          ; pointer to sound resource
looppoint:     dw 0          ; sound loop position
cumcue:        dw 0          ; the cumulative cue
status:        db 0          ; saves the last status for running status mode, holds the current status
pssndreset:    db 0          ; boolean describing whether PauseSound should reset the sound
seekto:        dw 0          ; the end point for SeekSound
seekcue:       dw 0          ; the desired sound cue at the end of SeekSound
globalvol:     dw 60         ; global volume on a scale of 0 to 63
muteflags:     dw 0          ; bit flag for each channel used to mute channels that have no GM instrument

MIDI_mapping:   times 128 db 0, 0, 0, 0, 0, 0, 0, 0
end_of_MIDI_mapping:         ; used in ending loop conditions

;                x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF
ccPermit:      db 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ; 0x      Whether or not to pass
               db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 1x      each CC on to the MIDI
               db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 2x      device.
               db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 3x         1 = allow
               db 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 4x         0 = disallow
               db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 5x
               db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ; 6x
               db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ; 7x


;--------------------------------------------------------------------------
; export area
; the interpreter interacts with the driver through these functions


; Export Lookup Table
; used by DriverInterface to call exported functions by number
; Exports must be even numbers because offsets are 2 bytes each.
;    Later interpreters used odd numbers for export functions as
;    well and multiplied by 2 to get the offset in this table.

ExportTable:
   Export0:    dw GetDeviceInfo
   Export2:    dw InitDevice
   Export4:    dw ShutdownDevice
   Export6:    dw LoadSound
   Export8:    dw DoSoundEvents
   Export10:   dw SetVolume
   Export12:   dw FadeOut
   Export14:   dw StopSound
   Export16:   dw PauseSound
   Export18:   dw SeekSound

%macro __pusha 0
        push    ax
        push    cx
        push    dx
        push    bx
        ;push    sp ;8086 PUSH SP behavior not the same as 286!
        push    bp
        push    si
        push    di
%endmacro

%macro __popa 0
        pop     di
        pop     si
        pop     bp
        ;pop    sp ;8086 POP SP behavior not the same as 286!
        pop     bx
        pop     dx
        pop     cx
        pop     ax
%endmacro

%macro __setne 1
        mov %1, 0
        je %%skip
        mov %1, 1
%%skip:
%endmacro

; DriverInterface
; Saves registers and calls export
; Entry: bp = export number
;        ds = heap segment
;        si = heap pointer where applicable
; Exit: ax, cx = return values where applicable
;       data at ds:si modified where applicable

DriverInterface:
   pushf
   push dx
   push bx
   push bp
   push si
   push di
   push es
   push ds
   mov [cs:ptrparam], si
   call near word [cs:ExportTable+bp]
   pop ds
   pop es
   pop di
   pop si
   pop bp
   pop bx
   pop dx
   popf
   retf


; GetDeviceInfo
; Called at startup to determine max poly and which patch
;    resource to load.
; Entry: nothing
; Exit: ax = patch number (0xFFFF if no patch)
;       cx = maximum polyphony

GetDeviceInfo:
   mov ax, INFO_PATCH
   mov cx, INFO_POLY
   retn


; InitDevice
; Called to load the patch if applicable and initialize the device
; Entry: ds:si points to patch resource structure
; Exit: cs:ax points to the area of the driver segment which can be overwritten
;       cx = 0

InitDevice:
   ; the next three lines can be removed if there is no patch to load
   mov bx, [si+8]
   mov es, [bx+2]
   mov di, [bx]              ; es:di points to patch data if applicable

   call mapMIDIInstruments   ; fill the game to GM mapping table

   call mpu_detect
   cmp ah, 0xFF
   jne have_mpu

   mov ax, 0xFFFF
   xor cx, cx
   retn

   have_mpu:
   call mpu_init
   mov ax, temp_area
   xor cx, cx
   retn


; ShutdownDevice
; Called to close the device when the interpreter exits. There
;    is no guarantee that StopSound will have been called on a
;    currently playing sound.
; Entry: ds:si points to sound resource structure
; Exit: nothing

ShutdownDevice:
   call mpu_silence
   call mpu_exit
   retn


; LoadSound
; Called before a sound starts playing so the driver has a chance
;    to load the header and perform initialization work.
; Entry: ds:si points to sound resource structure
; Exit: ax = 3 on unrecognized header byte, 1 on success

LoadSound:
   mov word [cs:playflags], 0x8000 ; channel 15 (the control channel) should always be enabled
   or word [cs:playflags], 0x0200 ; channel 9 (percussion) is implicitly enabled for MIDI

   mov bx, [si+SND_RESPTR]
   mov es, [bx+2]
   mov di, [bx]              ; es:di points to sound data

   xor dx, dx                ; dx will be added to di to compensate for missing channel 15 info
   mov cx, 16
   mov al, byte [es:di]      ; check for the presence of a PCM sample
   inc di
   cmp al, 0
   je load_header
   cmp al, 2
   je fifteen_chan

   mov ax, 3                 ; unplayable / invalid resource
   mov word [si+SND_STATE], ax
   retn

   fifteen_chan:
   dec cx                     ; if there is a PCM sample, the header only has info for 15 channels
   mov dx, 2                  ; compensates for missing channel 15 info

   load_header:
   mov bx, 1

   load_header_loop:
   mov al, byte [es:di]      ; al = initial number of voices on this channel
   inc di
   mov ah, byte [es:di]      ; ah = play flag for this channel
   inc di
   test ah, PLAY_FLAG        ; check for playflag
   jz continue_load

   or word [cs:playflags], bx ; enable channel

   continue_load:
   shl bx, 1
   loop load_header_loop

   add di, dx                 ; dx = 2 if there was only init data for 15 channels

   call getwaitcount

   mov ax, 1
   mov word [si+SND_STATE], ax  ; success loading sound
   mov word [si+SND_POS], di    ; set the position to the start of sound data
   mov word [si+SND_SIGNAL], 0  ; set the signal to 0
   mov word [cs:cumcue], 127    ; initialize the cumulative cue
   mov word [cs:looppoint], 33  ; initialize the loop point to the top
   mov word [cs:pssndreset], 0  ; PauseSound does not reset by default
   mov byte [cs:fadeticks], 0   ; there is no fade in progress
   mov byte [cs:playstate], PLAYING ; we're playing this sound now

   mov cx, word [si+SND_VOLUME]
   shl cx, 1
   shl cx, 1                 ; we use a range of 0-63 for internal global volume
   mov word [cs:globalvol], cx

   call mpu_reset

   retn


; DoSoundEvents
; Called each MIDI tick (60 times per second) to actually play the
;    sound resource
; Entry: ds:si points to sound resource structure
; Exit: nothing

StatusCallTable:             ; call table for handling status messages
   dw StopNote
   dw PlayNote
   dw KeyPressure
   dw ControlChange
   dw PatchChange
   dw ChanPressure
   dw PitchWheel

ParamCount: db 2, 2, 2, 2, 1, 1, 2 ; number of parameters used by each status

DoSoundEvents:
   mov bx, [si+SND_RESPTR]
   mov es, [bx+2]
   mov di, [bx]              ; es:di points to sound resource
   mov word [cs:sndresptr], di
   add di, word [si+SND_POS] ; get the current sound resource position

   call fader
   cmp byte [cs:playstate], STOPPED
   jne do_frame
   retn                      ; the sound isn't playing anymore, so quit

   do_frame:                 ; the loop to do all events in this frame (all events with delta time = 0)

   cmp word [cs:waitcount], 0 ; check if there's a new event
   je next_event
   dec word [cs:waitcount]
   jmp end_frame

   next_event:
   xor cx, cx
   mov cl, byte [es:di]
   inc di
   test cl, 0x80             ; check for running status mode
   jnz check_status
   mov cl, byte [cs:status]  ; repeat the last status
   test cl, 0x80             ; sanity check
   jz do_frame               ;   - the first event uses running status, which is insane for this driver
   dec di                    ; rewind di for re-reading the last byte as a parameter

   check_status:
   mov dl, cl                ; dl = full status byte (status and channel)
   mov byte [cs:status], cl
   mov al, cl
   shr al, 1
   shr al, 1
   shr al, 1
   shr al, 1                 ; al = status
   and cl, 0x0F              ; cl = channel

   cmp al, 0x0F
   je sys_message            ; check for a system message before we check the channel

   mov bx, 1
   shl bx, cl
   test word [cs:playflags], bx ; test to see if this channel is enabled
   jnz do_status

   mov bl, al                ; al is still the status
   sub bl, 8
   xor bh, bh
   mov bl, [cs:ParamCount+bx]
   add di, bx
   call getwaitcount
   jmp do_frame              ; this channel is to be ignored, so skip this status

   do_status:
   mov bl, al                ; al is still the status
   sub bl, 8
   xor bh, bh
   shl bx, 1
   call word [cs:StatusCallTable+bx] ; call the handler for this particular status
   call getwaitcount
   jmp do_frame

   sys_message:
   ; FIXME: We assume that this is 0xFC, but we should really check
   mov word [si+SND_SIGNAL], 0xFFFF ; the sound is done - set the signal appropriately
   xor bx, bx
   mov [cs:waitcount], bx
   mov bx, word [si+SND_RESPTR]
   mov di, word [bx]
   add di, word [cs:looppoint] ; set es:di to appropriate loop point

   end_frame:
   sub di, word [cs:sndresptr]
   mov word [si+SND_POS], di ; update the sound position
   retn


; SetVolume
; Called when the user changes the global sound volume (Ctrl-V in the game)
; Entry: ds:si points to sound resource structure
; Exit: nothing

SetVolume:
   mov cx, [si+SND_VOLUME]   ; cx = volume level (0-15)
   shl cx, 1
   shl cx, 1                 ; we use a range of 0-63 for internal global volume
   mov [cs:globalvol], cx
   retn


; FadeOut
; Called to slowly fade and stop the currently playing sound
; Entry: ds:si points to sound resource structure
; Exit: nothing

FadeOut:
   mov cx, [si+SND_VOLUME]   ; cx = volume level (0-15)
   shl cx, 1
   shl cx, 1                 ; we use a range of 0-63 for internal global volume
   test cx, cx
   jz f_stop                 ; just stop the sound now if the volume starts at 0

   mov [cs:fadevolume], cl
   mov byte [cs:fadeticks], 5
   retn

   f_stop:
   mov word [si+SND_SIGNAL], 0xFFFF ; the sound is done - set the signal appropriately
   call mpu_silence
   mov byte [cs:playstate], STOPPED
   retn


; StopSound
; Stops playback by turning off sound output. Nothing needs to be done to
;   prevent further MIDI events from occurring because the interpreter will
;   simply stop calling DoSoundEvents.
; Entry: ds:si points to sound resource structure
; Exit: the sound's signal property is set to -1

StopSound:
   call mpu_silence
   mov word [si+SND_SIGNAL], 0xFFFF
   mov word [cs:playstate], STOPPED
   retn


; PauseSound
; Pauses playback by turning off sound output. Nothing needs to be done to
;   prevent further MIDI events from occurring because the interpreter will
;   simply stop calling DoSoundEvents until playback resumes. If CC4C has been
;   set, the sound position should be reset. Otherwise it should be left alone.
; Entry: ds:si points to sound resource structure
; Exit: nothing

PauseSound:
   call mpu_silence

   cmp byte [cs:pssndreset], 0
   je exitpausesound

   mov ax, word [cs:looppoint] ; reset sound position to loop point
   mov word [si+SND_POS], ax

   mov bx, [si+SND_RESPTR]
   mov es, [bx+2]
   mov di, [bx]              ; es:di points to sound resource
   add di, ax                ; move forward to the loop position
   call getwaitcount         ; get a new waitcount for when the sound resumes

   exitpausesound:
   retn


; SeekSound
; Called when a sound needs to be quickly set to a certain point (e.g. on
;    restore, when one sound interrupts another, and so on)
; Entry: ds:si points to sound resource structure
; Exit: nothing

SeekSound:
   mov ax, word [si+SND_POS] ; this is where we need to seek to
   mov word [cs:seekto], ax
   mov ax, word [si+SND_SIGNAL] ; this is the what the cue should be
   call LoadSound
   mov byte [cs:playstate], SEEKING
   cmp ax, 1
   je do_seek
   retn

   do_seek:
   call DoSoundEvents
   mov bx, word [si+SND_POS]
   cmp bx, word [cs:seekto]
   jb do_seek                ; make sure we're at the right sound position
   mov bx, word [si+SND_SIGNAL]
   cmp bx, word [cs:seekcue]
   jne do_seek               ; make sure we're at the right cue position

   mov byte [cs:playstate], PLAYING
   retn



;--------------------------------------------------------------------------
; status functions

; on entry for each of these functions:
;   al = status
;   cl = channel
;   dl = full status byte (status and channel)
;   es:di points to parameters for the status
; on exit:
;   es:di points to the delta time until the next sound element


; two parameter status messages that get passed through to the device
KeyPressure:
PitchWheel:
   mov al, dl
   call mpu_output
   mov al, byte [es:di]
   call mpu_output
   mov al, byte [es:di+1]
   call mpu_output
   add di, 2
   ret


StopNote:
   cmp byte [cs:playstate], PLAYING
   je stop_note

   add di, 2                 ; we're just seeking, don't play anything
   ret

   stop_note:
   mov al, dl
   call mpu_output
   mov al, byte [es:di]
   call mpu_output
   mov al, byte [es:di+1]
   call mpu_output
   add di, 2
   ret


PlayNote:
   cmp byte [cs:playstate], PLAYING
   je check_mute

   add di, 2                 ; we're just seeking, don't play anything
   ret

   check_mute:               ; check the channel mute flag
   mov ax, 1
   shl ax, cl                ; cl = channel
   test word [cs:muteflags], ax
   jz play_note

   add di, 2                 ; skip over parameters
   ret                       ; return without playing because the mute flag is set

   play_note:
   mov al, dl
   call mpu_output
   mov al, byte [es:di]
   call mpu_output
   mov al, byte [es:di+1]
   cbw
   mul word [cs:globalvol]   ; multiply by [cs:globalvol]/64 to scale volume
   mov bx, 64
   div bx
   call mpu_output
   add di, 2
   ret


ControlChange:
   mov bh, byte [es:di]      ; bh = control number
   mov bl, byte [es:di+1]    ; bl = control value
   add di, 2
   cmp bh, 0x60
   jne not_CC60h

   xor bh, bh
   add bx, word [cs:cumcue]
   mov word [si+SND_SIGNAL], bx ; set the new signal
   mov word [cs:cumcue], bx  ; update our cumulative cue variable
   ret

   not_CC60h:
   cmp bh, 0x4C
   jne not_CC4Ch

   mov byte [cs:pssndreset], bl ; set the PauseSound reset control value
   ret

   not_CC4Ch:
   push bx
   xchg bl, bh
   xor bh, bh
   cmp byte [cs:ccPermit+bx], 0 ; check CC permit table
   pop bx
   je allow_cc
   ret                       ; ignore disallowed CCs

   allow_cc:
   mov al, dl
   call mpu_output
   mov al, bh
   call mpu_output
   mov al, bl
   call mpu_output
   ret


PatchChange:
   mov bl, byte [es:di]      ; bl = patch
   inc di
   cmp cl, 15
   jne normal_patch_change

   cmp bl, 127
   je set_loop_point

   mov byte [si+SND_SIGNAL], bl ; this is a cue - set the signal property
   ret

   set_loop_point:           ; set the sound loop point
   push di
   sub di, word [cs:sndresptr]
   sub di, 2
   mov word [cs:looppoint], di
   pop di
   ret

   normal_patch_change:      ; this is a simple patch change
   ; get the GM instrument
   xor bh, bh
   shl bx, 1
   shl bx, 1
   shl bx, 1
   mov cl, [cs:MIDI_mapping+bx+GM_INSTR]

   test cl, 128
   jz do_patch_change

   mov al, dl
   and al, 0x0F              ; al = channel
   cmp al, 9                 ; see if this is the percussion channel
   __setne al                ; mute this channel unless it's the percussion channel
   jmp set_channel_mute

   do_patch_change:          ; send the translated patch change to the mpu
   mov al, dl
   call mpu_output
   mov al, cl
   call mpu_output
   mov al, 0                 ; make sure the channel isn't muted

   set_channel_mute:
   mov cl, dl                ; dl is still the status
   and cl, 0x0F              ; cl = channel
   mov bx, 1
   shl bx, cl
   not bx
   and word [cs:muteflags], bx ; clear the mute flag for the channel
   xor bx, bx
   mov bl, al
   shl bx, cl
   or word [cs:muteflags], bx ; set the mute flag for the channel if al is set

   ret


ChanPressure:
   mov al, dl
   call mpu_output
   mov al, [es:di]
   call mpu_output
   add di, 1
   ret



;----------------------------------------------------------------------------
; MPU functions


%define PORT2 [cs:lpt_ctrl]
lpt_ctrl: dw 0


; mpu_output
; output the specified byte on the specified port
; in: al = byte
; out:

mpu_output:
   push ax
   push bx
   push dx
   mov bx, ax
   %define ARG bl
   %include '../s2ppatch/asm/data16.s'
   %undef ARG
   pop dx
   pop bx
   pop ax
   ret


; mpu_detect
; check for the existance of the MPU on the specified base port
; in:
; out: ah = 0 on success, -1 on fail, al destroyed

mpu_detect:
   ; get LPT1 port
   push ds
   mov ax, 0x40
   mov ds, ax
   mov ax, [0x0008]
   pop ds
   test ax, ax
   jz md_fail

   add ax, 2
   mov PORT2, ax
   xor ah, ah                ; success
   ret

   md_fail:
   mov ah, 0xFF              ; fail
   ret


; mpu_init
; initialize the mpu on the specified base port
; in:
; out:

mpu_init:
   push ax
   push dx
   %define ARG 0x3F
   %include '../s2ppatch/asm/cmd16.s'
   %undef ARG
   pop dx
   pop ax

   mov al, 0xB1
   call mpu_output
   mov al, 121
   call mpu_output
   mov al, 0
   call mpu_output

   ret


; mpu_exit
; close the mpu on the specified base port
; in:
; out:

mpu_exit:
   ret


; mpu_silence
; silence the mpu
; in:
; out:

mpu_silence:
   push ax
   push cx
   mov cx, 0x0F
   mov ah, 0xB0
   mpu_silence_loop:
   mov al, ah
   or al, cl
   call mpu_output
   mov al, 123
   call mpu_output
   mov al, 0
   call mpu_output
   loop mpu_silence_loop
   pop cx
   pop ax
   ret


; mpu_reset
; reset the mpu for playback of a new sound
; in:
; out:

mpu_reset:
   call mpu_silence
   push ax
   push cx
   mov cx, 0x0F
   mov ah, 0xB0
   mpu_reset_loop:
   mov al, ah
   or al, cl
   call mpu_output
   mov al, 121
   call mpu_output
   mov al, 0
   call mpu_output
   loop mpu_reset_loop
   pop cx
   pop ax
   ret



;--------------------------------------------------------------------------
; misc functions


; getwaitcount
; retrieves the next delta time from the sound resource
; entry: es:di points to the current delta time value
; exit: [cs:waitcount] set, es:di points to the sound element following the delta time

getwaitcount:
   push ax
   push dx
   xor ax, ax
   xor dx, dx

   get_delta_time_loop:
   mov dl, byte [es:di]
   cmp dl, 0xF8
   jne add_dt

   mov dl, 0xF0              ; delta time extension

   add_dt:
   add ax, dx
   inc di
   cmp dl, 0xF0
   je get_delta_time_loop

   mov word [cs:waitcount], ax ; set waitcount
   pop dx
   pop ax
   ret


; fader
; checks for a fade in progress and acts accordingly
; entry: es:di points to the current position in the sound resource
; exit:

fader:
   cmp byte [cs:fadeticks], 0 ; check to see if there's a fade in progress
   je exit_fader

   dec byte [cs:fadeticks]
   cmp byte [cs:fadeticks], 0 ; check if we need to decrease the volume yet
   jne exit_fader

   dec byte [cs:fadevolume]
   cmp byte [cs:fadevolume], 0 ; just stop the sound if we're done fading
   je f_stopsnd

   mov byte [cs:fadeticks], 5
   push cx
   xor cx, cx
   mov cl, byte [cs:fadevolume]
   mov [cs:globalvol], cx
   pop cx
   jmp exit_fader

   f_stopsnd:
   mov byte [cs:playstate], STOPPED
   push si
   mov si, [cs:ptrparam]
   mov word [si+SND_SIGNAL], 0xFFFF ; the sound is done - set the signal appropriately
   mov word [si+SND_UNK], 0  ; fixes fadeout bugs in CB
   call mpu_silence
   pop si

   exit_fader:
   ret



;--------------------------------------------------------------------------
; data / functions that will only be used before or during initialization
temp_area:




; begin code translated from midi.c written by Rickard Lind and Christoph Reichenbach of
; the FreeSCI project

MT32_PresetTimbreMaps:
   db  0, NOMAP    ; "Acou Piano 1"
   db  1, NOMAP    ; "Acou Piano 2"
   db  2, NOMAP    ; "Acou Piano 3"
   db  4, NOMAP    ; "Elec Piano 1"
   db  5, NOMAP    ; "Elec Piano 2"
   db  4, NOMAP    ; "Elec Piano 3"
   db  5, NOMAP    ; "Elec Piano 4"
   db  3, NOMAP    ; "Honkytonk"
   db  16, NOMAP    ; "Elec Org 1"
   db  17, NOMAP    ; "Elec Org 2"
   db  18, NOMAP    ; "Elec Org 3"
   db  18, NOMAP    ; "Elec Org 4"
   db  19, NOMAP    ; "Pipe Org 1"
   db  19, NOMAP    ; "Pipe Org 2"
   db  20, NOMAP    ; "Pipe Org 3"
   db  21, NOMAP    ; "Accordion"
   db  6, NOMAP    ; "Harpsi 1"
   db  6, NOMAP    ; "Harpsi 2"
   db  6, NOMAP    ; "Harpsi 3"
   db  7, NOMAP    ; "Clavi 1"
   db  7, NOMAP    ; "Clavi 2"
   db  7, NOMAP    ; "Clavi 3"
   db  8, NOMAP    ; "Celesta 1"
   db  8, NOMAP    ; "Celesta 2"
   db  62, NOMAP    ; "Syn Brass 1"
   db  63, NOMAP    ; "Syn Brass 2"
   db  62, NOMAP    ; "Syn Brass 3"
   db  63, NOMAP    ; "Syn Brass 4"
   db  38, NOMAP    ; "Syn Bass 1"
   db  39, NOMAP    ; "Syn Bass 2"
   db  38, NOMAP    ; "Syn Bass 3"
   db  39, NOMAP    ; "Syn Bass 4"
   db  88, NOMAP    ; "Fantasy"
   db  89, NOMAP    ; "Harmo Pan"
   db  52, NOMAP    ; "Chorale"
   db  98, NOMAP    ; "Glasses"
   db  97, NOMAP    ; "Soundtrack"
   db  99, NOMAP    ; "Atmosphere"
   db  89, NOMAP    ; "Warm Bell"
   db  85, NOMAP    ; "Funny Vox"
   db  39, NOMAP    ; "Echo Bell"
   db  101, NOMAP    ; "Ice Rain"
   db  68, NOMAP    ; "Oboe 2001"
   db  87, NOMAP    ; "Echo Pan"
   db  86, NOMAP    ; "Doctor Solo"
   db  103, NOMAP    ; "Schooldaze"
   db  88, NOMAP    ; "Bellsinger"
   db  80, NOMAP    ; "Square Wave"
   db  48, NOMAP    ; "Str Sect 1"
   db  48, NOMAP    ; "Str Sect 2"
   db  49, NOMAP    ; "Str Sect 3"
   db  45, NOMAP    ; "Pizzicato"
   db  40, NOMAP    ; "Violin 1"
   db  40, NOMAP    ; "Violin 2"
   db  42, NOMAP    ; "Cello 1"
   db  42, NOMAP    ; "Cello 2"
   db  43, NOMAP    ; "Contrabass"
   db  46, NOMAP    ; "Harp 1"
   db  46, NOMAP    ; "Harp 2"
   db  24, NOMAP    ; "Guitar 1"
   db  25, NOMAP    ; "Guitar 2"
   db  26, NOMAP    ; "Elec Gtr 1"
   db  27, NOMAP    ; "Elec Gtr 2"
   db  104, NOMAP    ; "Sitar"
   db  32, NOMAP    ; "Acou Bass 1"
   db  33, NOMAP    ; "Acou Bass 2"
   db  34, NOMAP    ; "Elec Bass 1"
   db  39, NOMAP    ; "Elec Bass 2"
   db  36, NOMAP    ; "Slap Bass 1"
   db  37, NOMAP    ; "Slap Bass 2"
   db  35, NOMAP    ; "Fretless 1"
   db  35, NOMAP    ; "Fretless 2"
   db  73, NOMAP    ; "Flute 1"
   db  73, NOMAP    ; "Flute 2"
   db  72, NOMAP    ; "Piccolo 1"
   db  72, NOMAP    ; "Piccolo 2"
   db  74, NOMAP    ; "Recorder"
   db  75, NOMAP    ; "Pan Pipes"
   db  64, NOMAP    ; "Sax 1"
   db  65, NOMAP    ; "Sax 2"
   db  66, NOMAP    ; "Sax 3"
   db  67, NOMAP    ; "Sax 4"
   db  71, NOMAP    ; "Clarinet 1"
   db  71, NOMAP    ; "Clarinet 2"
   db  68, NOMAP    ; "Oboe"
   db  69, NOMAP    ; "Engl Horn"
   db  70, NOMAP    ; "Bassoon"
   db  22, NOMAP    ; "Harmonica"
   db  56, NOMAP    ; "Trumpet 1"
   db  56, NOMAP    ; "Trumpet 2"
   db  57, NOMAP    ; "Trombone 1"
   db  57, NOMAP    ; "Trombone 2"
   db  60, NOMAP    ; "Fr Horn 1"
   db  60, NOMAP    ; "Fr Horn 2"
   db  58, NOMAP    ; "Tuba"
   db  61, NOMAP    ; "Brs Sect 1"
   db  61, NOMAP    ; "Brs Sect 2"
   db  11, NOMAP    ; "Vibe 1"
   db  11, NOMAP    ; "Vibe 2"
   db  12, NOMAP    ; "Syn Mallet"
   db  88, NOMAP    ; "Windbell"
   db  9, NOMAP    ; "Glock"
   db  14, NOMAP    ; "Tube Bell"
   db  13, NOMAP    ; "Xylophone"
   db  12, NOMAP    ; "Marimba"
   db  107, NOMAP    ; "Koto"
   db  111, NOMAP    ; "Sho"
   db  77, NOMAP    ; "Shakuhachi"
   db  78, NOMAP    ; "Whistle 1"
   db  78, NOMAP    ; "Whistle 2"
   db  76, NOMAP    ; "Bottleblow"
   db  121, NOMAP    ; "Breathpipe"
   db  47, NOMAP    ; "Timpani"
   db  117, NOMAP    ; "Melodic Tom"
   db  RHYTHM, 37    ; "Deep Snare"
   db  115, NOMAP    ; "Elec Perc 1"
   db  118, NOMAP    ; "Elec Perc 2"
   db  116, NOMAP    ; "Taiko"
   db  118, NOMAP    ; "Taiko   Rim"
   db  RHYTHM, 50    ; "Cymbal"
   db  RHYTHM, NOMAP    ; "Castanets"
   db  112, NOMAP    ; "Triangle"
   db  55, NOMAP    ; "Orche Hit"
   db  124, NOMAP    ; "Telephone"
   db  123, NOMAP    ; "Bird Tweet"
   db  NOMAP, NOMAP    ; "One Note Jam"
   db  98, NOMAP    ; "Water Bells"
   db  NOMAP, NOMAP    ; "Jungle Tune"


MT32_RhythmTimbreMaps:
   db  RHYTHM, 34    ; "Acou BD"
   db  RHYTHM, 37    ; "Acou SD"
   db  117, 49    ; "Acou Hi Tom"
   db  117, 46    ; "Acou Mid Tom"
   db  117, 40    ; "Acou Low Tom"
   db  RHYTHM, 39    ; "Elec SD"
   db  RHYTHM, 41    ; "Clsd Hi Hat"
   db  RHYTHM, 45    ; "Open Hi Hat 1"
   db  RHYTHM, 48    ; "Crash Cym"
   db  RHYTHM, 50    ; "Ride Cym"
   db  RHYTHM, 36    ; "Rim Shot"
   db  RHYTHM, 38    ; "Hand Clap"
   db  RHYTHM, 55    ; "Cowbell"
   db  RHYTHM, 61    ; "Mt High Conga"
   db  RHYTHM, 62    ; "High Conga"
   db  RHYTHM, 63    ; "Low Conga"
   db  RHYTHM, 64    ; "High Timbale"
   db  RHYTHM, 65    ; "Low Timbale"
   db  RHYTHM, 59    ; "High Bongo"
   db  RHYTHM, 60    ; "Low Bongo"
   db  113, 66    ; "High Agogo"
   db  113, 67    ; "Low Agogo"
   db  RHYTHM, 53    ; "Tambourine"
   db  RHYTHM, 74    ; "Claves"
   db  RHYTHM, 69    ; "Maracas"
   db  78, 71    ; "Smba Whis L"
   db  78, 70    ; "Smba Whis S"
   db  RHYTHM, 68    ; "Cabasa"
   db  RHYTHM, 72    ; "Quijada"
   db  RHYTHM, 43    ; "Open Hi Hat 2"


MT32_PresetRhythmKeymap:
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, 34,    34,    36,    37,    38,    39,
   db  40,    41,    42,    43,    44,    45,    46,    47,    48,    49,
   db  50,    NOMAP, NOMAP, 53,    NOMAP, 55,    NOMAP, NOMAP, NOMAP, 59,
   db  60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
   db  70,    71,    72,    NOMAP, 74,    NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP,
   db  NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP, NOMAP


MT32_MemoryTimbreMaps:
   db "AccPnoKA2 ", 1, NOMAP
   db "Acou BD   ", RHYTHM, 34
   db "Acou SD   ", RHYTHM, 37
   db "AcouPnoKA ", 0, NOMAP
   db "BASS      ", 32, NOMAP
   db "BASSOONPCM", 70, NOMAP
   db "BEACH WAVE", 122, NOMAP
   db "BagPipes  ", 109, NOMAP
   db "BassPizzMS", 45, NOMAP
   db "BassoonKA ", 70, NOMAP
   db "Bell    MS", 112, NOMAP
   db "Big Bell  ", 14, NOMAP
   db "Bird Tweet", 123, NOMAP
   db "BrsSect MS", 61, NOMAP
   db "CLAPPING  ", 126, NOMAP
   db "Cabasa    ", RHYTHM, 68
   db "Calliope  ", 82, NOMAP
   db "CelticHarp", 46, NOMAP
   db "Chicago MS", 3, NOMAP
   db "Chop      ", 117, NOMAP
   db "Chorale MS", 52, NOMAP
   db "ClarinetMS", 71, NOMAP
   db "Claves    ", RHYTHM, 74
   db "ClockBell ", 14, NOMAP
   db "ConcertCym", RHYTHM, 54
   db "Conga   MS", RHYTHM, 63
   db "CoolPhone ", 124, NOMAP
   db "CracklesMS", NOMAP, NOMAP
   db "CreakyD MS", NOMAP, NOMAP
   db "CrshCymbMS", RHYTHM, 56
   db "CstlGateMS", NOMAP, NOMAP
   db "CymSwellMS", RHYTHM, 54
   db "CymbRollKA", RHYTHM, 56
   db "Cymbal Lo ", NOMAP, NOMAP
   db "card      ", NOMAP, NOMAP
   db "DirtGtr MS", 30, NOMAP
   db "DirtGtr2MS", 29, NOMAP
   db "E Bass  MS", 33, NOMAP
   db "ElecBassMS", 33, NOMAP
   db "ElecGtr MS", 27, NOMAP
   db "EnglHornMS", 69, NOMAP
   db "FantasiaKA", 88, NOMAP
   db "Fantasy   ", 99, NOMAP
   db "Fantasy2MS", 99, NOMAP
   db "Filter  MS", 95, NOMAP
   db "Filter2 MS", 95, NOMAP
   db "Flute   MS", 73, NOMAP
   db "FogHorn MS", 58, NOMAP
   db "FrHorn1 MS", 60, NOMAP
   db "FunnyTrmp ", 56, NOMAP
   db "GameSnd MS", 80, NOMAP
   db "Glock   MS", 9, NOMAP
   db "Gunshot   ", 127, NOMAP
   db "Hammer  MS", NOMAP, NOMAP
   db "Harmonica2", 22, NOMAP
   db "Harpsi 1  ", 6, NOMAP
   db "Harpsi 2  ", 6, NOMAP
   db "Heart   MS", 116, NOMAP
   db "Horse1  MS", NOMAP, NOMAP
   db "Horse2  MS", NOMAP, NOMAP
   db "InHale  MS", 121, NOMAP
   db "KenBanjo  ", 105, NOMAP
   db "Kiss    MS", 25, NOMAP
   db "KongHit   ", NOMAP, NOMAP
   db "Koto      ", 107, NOMAP
   db "Laser   MS", 103, NOMAP
   db "MTrak   MS", 97, NOMAP
   db "MachGun MS", 127, NOMAP
   db "OCEANSOUND", 122, NOMAP
   db "Oboe 2001 ", 68, NOMAP
   db "Ocean   MS", 122, NOMAP
   db "PPG 2.3 MS", 75, NOMAP
   db "PianoCrank", NOMAP, NOMAP
   db "PicSnareMS", RHYTHM, 39
   db "PiccoloKA ", 72, NOMAP
   db "PinkBassMS", 39, NOMAP
   db "Pizz2     ", 45, NOMAP
   db "Portcullis", NOMAP, NOMAP
   db "RatSqueek ", 72, NOMAP
   db "Record78  ", NOMAP, NOMAP
   db "RecorderMS", 74, NOMAP
   db "Red Baron ", 125, NOMAP
   db "ReedPipMS ", 20, NOMAP
   db "RevCymb MS", 119, NOMAP
   db "RifleShot ", 127, NOMAP
   db "RimShot MS", RHYTHM, 36
   db "SHOWER    ", 52, NOMAP
   db "SQ Bass MS", 38, NOMAP
   db "ShakuVibMS", 79, NOMAP
   db "SlapBassMS", 36, NOMAP
   db "Snare   MS", RHYTHM, 37
   db "Some Birds", 123, NOMAP
   db "Sonar   MS", 78, NOMAP
   db "Soundtrk2 ", 97, NOMAP
   db "Soundtrack", 97, NOMAP
   db "SqurWaveMS", 80, NOMAP
   db "StabBassMS", 34, NOMAP
   db "SteelDrmMS", 114, NOMAP
   db "StrSect1MS", 48, NOMAP
   db "String  MS", 45, NOMAP
   db "Syn-Choir ", 91, NOMAP
   db "Syn Brass4", 63, NOMAP
   db "SynBass MS", 38, NOMAP
   db "SwmpBackgr", 88, NOMAP
   db "T-Bone2 MS", 57, NOMAP
   db "Taiko     ", 116, 34
   db "Taiko Rim ", 118, 36
   db "Timpani1  ", 47, NOMAP
   db "Tom     MS", 117, 47
   db "Toms    MS", 117, 47
   db "Tpt1prtl  ", 56, NOMAP
   db "TriangleMS", 112, 80
   db "Trumpet 1 ", 56, NOMAP
   db "Type    MS", 114, NOMAP
   db "WaterBells", 98, NOMAP
   db "WaterFallK", NOMAP, NOMAP
   db "Whiporill ", 123, NOMAP
   db "Wind      ", NOMAP, NOMAP
   db "Wind    MS", NOMAP, NOMAP
   db "Wind2   MS", NOMAP, NOMAP
   db "Woodpecker", 115, NOMAP
   db "WtrFall MS", NOMAP, NOMAP
   db 0




; entry: es:di points to string to match
;        al = 0 for instrument / 1 for rythm key
; exit: al = map
lookup_memtimbre:
   push bx
   push dx
   push si

   mov si, MT32_MemoryTimbreMaps

   l_search:

   xor bx, bx
   l_strncasecmp:
   mov dl, [es:di+bx]
   cmp dl, 'a'
   jb setDH
   cmp dl, 'z'
   ja setDH
   sub dl, 0x20

   setDH:
   mov dh, [cs:si+bx]
   cmp dh, 'a'
   jb cmpDX
   cmp dh, 'z'
   ja cmpDX
   sub dh, 0x20

   cmpDX:
   cmp dl, dh
   jne l_continue
   inc bx
   cmp bx, 10
   jl l_strncasecmp

   ; we have a match
   jmp l_found

   ; no match
   l_continue:
   add si, 12
   cmp byte [cs:si], 0
   jne l_search

   pop si
   pop dx
   pop bx
   mov al, MAP_NOT_FOUND
   ret

   l_found:
   push ax
   xor ah, ah
   add si, ax
   pop ax
   mov al, [cs:si+10]
   pop si
   pop dx
   pop bx
   ret



second:         db 0

group:          db 0
number:         db 0
keyshift:       db 0
finetune:       db 0
bender_range:   db 0
patchpointer:   dw 0

; entry: es:di points to patch 1
; exit: MIDI_mapping filled
mapMIDIInstruments:
   __pusha

   mov ax, MIDI_mapping
   mov bx, MT32_PresetTimbreMaps
   mov cx, MT32_PresetRhythmKeymap
   init_preset_mapping:
   mov si, bx
   mov dl, [cs:si]           ; MT32_PresetTimbreMaps[i].gm_instr
   mov si, cx
   mov dh, [cs:si]           ; MT32_PresetRhythmKeymap[i]
   mov si, ax
   mov [cs:si+GM_INSTR], dl
   mov byte [cs:si+KEYSHIFT], 0x40
   mov word [cs:si+FINETUNE], 0x2000
   mov byte [cs:si+BENDER_RANGE], 0x0C
   mov [cs:si+GM_RHYTHMKEY], dh
   mov byte [cs:si+VOLUME], 100
   add ax, 8
   add bx, 2
   inc cx
   cmp ax, end_of_MIDI_mapping
   jl init_preset_mapping

   mov si, MIDI_mapping
   mov [cs:patchpointer], di ; patchpointer = patchdata
   add di, 0x6B
   mov cx, 48
   patch_loop:

      xor ah, ah                ; will be used to test for an all 0 patch

      mov al, [es:di]
      mov [cs:group], al        ; group = *(patchdata + 0x6B + 8*i)
      or ah, al

      mov al, [es:di+1]
      mov [cs:number], al       ; number = *(patchdata + 0x6B + 8*i + 1)
      or ah, al

      mov al, [es:di+2]
      mov [cs:keyshift], al     ; keyshift = *(patchdata + 0x6B + 8*i + 2)
      or ah, al

      mov al, [es:di+3]
      mov [cs:finetune], al     ; finetune = *(patchdata + 0x6B + 8*i + 3)
      or ah, al

      mov al, [es:di+4]
      mov [cs:bender_range], al ; bender_range = *(patchdata + 0x6B + 8*i + 4)
      or ah, al

      or ah, [es:di+5]
      or ah, [es:di+6]
      or ah, [es:di+7]

      test ah, ah
      jnz map_patch

      ; this patch will not be mapped
      mov byte [cs:si+GM_INSTR], NOMAP
      jmp next_patch

      map_patch:
      mov al, [cs:group]
      cmp al, 0
      jne not_group_0

      ; group 0
      ; MIDI_mapping[i].gm_instr = MT32_PresetTimbreMaps[number].gm_instr
      xor bx, bx
      mov bl, [cs:number]
      shl bx, 1
      mov al, [cs:MT32_PresetTimbreMaps+bx]
      mov [cs:si+GM_INSTR], al
      jmp continue_mapping

      not_group_0:
      cmp al, 1
      jne not_group_1

      ; group 1
      ; MIDI_mapping[i].gm_instr = MT32_PresetTimbreMaps[number + 64].gm_instr
      xor bx, bx
      mov bl, [cs:number]
      add bl, 64
      shl bx, 1
      mov al, [cs:MT32_PresetTimbreMaps+bx]
      mov [cs:si+GM_INSTR], al
      jmp continue_mapping

      not_group_1:
      cmp al, 2
      jne not_group_2

      ; group 2
      ; MIDI_mapping[i].gm_instr = _lookup_instrument (patchdata + 0x1EC + number * 0xF6)
      push di
      mov ax, [cs:patchpointer]
      mov di, ax
      add di, 0x1EC
      mov al, [cs:number]
      mov ah, 0xF6
      mul ah
      add di, ax
      xor al, al
      call lookup_memtimbre
      pop di
      mov [cs:si+GM_INSTR], al
      jmp continue_mapping

      not_group_2:
      cmp al, 3
      jne continue_mapping

      ; group 3
      ; MIDI_mapping[i].gm_instr = MT32_RhythmTimbreMaps[number].gm_instr
      xor bx, bx
      mov bl, [cs:number]
      shl bx, 1
      mov al, [cs:MT32_RhythmTimbreMaps+bx]
      mov [cs:si+GM_INSTR], al

      continue_mapping:

      ; MIDI_mapping[i].keyshift = 0x40 + ((keyshift & 0x3F) - 24)
      mov al, [cs:keyshift]
      and al, 0x3F
      add al, 40
      mov [cs:si+KEYSHIFT], al

      ; MIDI_mapping[i].finetune = 0x2000 + (((finetune & 0x7F) - 50) << 11) / 25
      xor ax, ax
      mov al, [cs:finetune]
      and al, 0x7F
      sub ax, 50
      mov bx, 82
      mul bx
      add ax, 0x2000
      mov [cs:si+FINETUNE], ax

      ; MIDI_mapping[i].bender_range = bender_range & 0x1F
      mov al, [cs:bender_range]
      and al, 0x1F
      mov [cs:si+BENDER_RANGE], al

      next_patch:
      add di, 8
      add si, 8
      dec cx
      test cx, cx
      jnz patch_loop
;      loop patch_loop

   cmp byte [cs:second], 0
   jne done_loading_patches

   ; skip timbre memory block
   mov al, [es:di]
   inc di
   mov bl, 246
   mul bl
   add di, ax

   ; check for the second Patch Memory block
   mov ah, [es:di]
   mov al, [es:di+1]
   cmp ax, 0xABCD
   jne done_loading_patches

   mov cx, 48
   add di, 2
   mov byte [cs:second], 1
   jmp patch_loop

   done_loading_patches:
   mov ah, [es:di]
   mov al, [es:di+1]
   add di, 2
   cmp ax, 0xDCBA
   jne done_mapping

   mov cx, 64
   mov si, MIDI_mapping
   add si, 184               ; start with entry 23
   rhythm_loop:

      mov al, [es:di]
      mov [cs:number], al       ; number = *(patchdata + pos + 4*i + 2)

      cmp al, 64
      jge notlessthan64

      ; MIDI_mapping[i+23].gm_rhythmkey = _lookup_rhythm_key (patchdata + 0x1EC + number * 0xF6)
      push di
      mov ax, [cs:patchpointer]
      mov di, ax
      add di, 0x1EC
      mov al, [cs:number]
      mov ah, 0xF6
      mul ah
      add di, ax
      mov al, 1
      call lookup_memtimbre
      pop di
      mov [cs:si+GM_RHYTHMKEY], al
      jmp continue_mapping_2

      notlessthan64:
      cmp al, 94
      jge not64to94

      ; MIDI_mapping[i+23].gm_rhythmkey = MT32_RhythmTimbreMaps[number-64].gm_rhythmkey
      xor bh, bh
      mov bl, bl
      sub bx, 64
      shl bx, 1
      inc bx
      mov al, [cs:MT32_RhythmTimbreMaps+bx]
      mov [cs:si+GM_RHYTHMKEY], al
      jmp continue_mapping_2

      not64to94:
      ; MIDI_mapping[i+23].gm_rhythmkey = NOMAP
      mov byte [cs:si+GM_RHYTHMKEY], NOMAP

      continue_mapping_2:
      add di, 4
      add si, 8
      loop rhythm_loop

   done_mapping:
   __popa
   ret
