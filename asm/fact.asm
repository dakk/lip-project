// Factorial
DEBUG REG $n
DEBUG REG $f // factorial of n
	addi $n 5 $0
	addi $f 1 $0
	addi $i 1 $0
Loop:	slt $t $n $i
	bne $t $0 End
	mul $f $f $i
	addi $i 1 $i
	jmp Loop
End:	halt