; input given by macro ARG
; ARG can't be al, dx
; modifies al, dx


	%if __BITS__ != 16
	%error This file is intended for 16-bit code
	%endif

	mov dx, PORT2
	mov al, 3
	out dx, al
	dec dx
	dec dx
	mov al, ARG
	out dx, al
	inc dx
	inc dx
	mov al, 7
	out dx, al
	in al, dx
	in al, dx
	in al, dx
	in al, dx
	in al, dx
	in al, dx
