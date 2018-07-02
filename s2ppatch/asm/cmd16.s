; modifies al, dx

	%if __BITS__ != 16
	%error This file is intended for 16-bit code
	%endif

	%ifndef ARG
	%define ARG 0x3F
	%endif

	mov dx, PORT2
	mov al, 0x02
	out dx, al
	dec dx
	dec dx
	mov al, ARG
	out dx, al
	inc dx
	inc dx
	mov al, 0x06
	out dx, al
	in al, dx
	in al, dx
	in al, dx
	in al, dx
	in al, dx
	in al, dx
	mov al, 0x0F
	out dx, al
	mov al, 0x07
	out dx, al
