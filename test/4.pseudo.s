.bss
.data
.text
.globl main
main: 
# x0 related pseudo instructions
	nop
	neg a0, a2
	negw a0, a1
	snez a0, a1
	sltz a0, a1
	sgtz a0, a1
	beqz a1, main_0
	bnez a1, main_0
	blez a1, main_0
	bgez a1, main_0
	bltz a1, main_0
	bgtz a1, main_0
	j main_0
	jr a1
	ret
	ld a0, foo
	ld a1, main_0
	call end
	tail end
main_0:
# non x0 related pseudo instructions
	# li a0, 42
	mv a0, a1
	not a0, a1
	seqz a0, a1
	bgt a1, a2, main_0
	ble a1, a2, main_0
	bgtu a1, a2, main_0
	bleu a1, a2, main_0
	jal main_0
	jalr a1
	call main_0
.globl end
end:
	mv a1, a0