DEBUG MEM 0 160
addi $sl 1 $0
addi $sl 2 $0
store $sl $0 $0
addi $sl 4 $0
addi $2 1 $0
store $sl $0 $2
addi $sl 1 $sl
addi $2 6 $0
store $sl $0 $2
addi $sl 1 $sl
addi $2 6 $0
store $sl $0 $2
addi $sl 1 $sl
store $sl $0 $0
addi $sl 1 $sl
addi $sl 8 $0
store $sl $0 $0
addi $sl 10 $0
addi $sl 10 $0
store $sl $0 $0
addi $sl 12 $0
addi $sl 12 $0
store $sl $0 $0
addi $sl 14 $0
addi $3 0 $sl
addi $5 1 $0
store $sl $0 $5
addi $sl 1 $sl
addi $5 1 $sl
store $sl $0 $5
addi $sl 1 $sl
addi $5 0 $0
store $sl $0 $5
addi $sl 1 $sl
store $sl $0 $0
addi $sl 1 $sl
addi $2 2 $0
load $6 $3 $0
store $2 $0 $6
addi $5 1 $2
store $5 $0 $0
addi $3 1 $3
load $3 $3 $0
beq $6 $0 L28
store $5 $0 $sl
L27: load $5 $3 $0
store $sl $0 $5
addi $sl 1 $sl
addi $5 1 $sl
store $sl $0 $5
addi $sl 1 $sl
addi $3 1 $3
load $3 $3 $0
bne $3 $0 L27
L28: addi $3 1 $0
sub $sl $sl $3
store $sl $0 $0
addi $sl 1 $sl
addi $3 0 $sl
addi $5 1 $0
store $sl $0 $5
addi $sl 1 $sl
addi $5 1 $sl
store $sl $0 $5
addi $sl 1 $sl
addi $5 1 $0
store $sl $0 $5
addi $sl 1 $sl
store $sl $0 $0
addi $sl 1 $sl
addi $2 8 $0
load $6 $3 $0
store $2 $0 $6
addi $5 1 $2
store $5 $0 $0
addi $3 1 $3
load $3 $3 $0
beq $6 $0 L26
store $5 $0 $sl
L25: load $5 $3 $0
store $sl $0 $5
addi $sl 1 $sl
addi $5 1 $sl
store $sl $0 $5
addi $sl 1 $sl
addi $3 1 $3
load $3 $3 $0
bne $3 $0 L25
L26: addi $3 1 $0
sub $sl $sl $3
store $sl $0 $0
addi $sl 1 $sl
addi $3 0 $sl
addi $5 1 $0
store $sl $0 $5
addi $sl 1 $sl
addi $5 1 $sl
store $sl $0 $5
addi $sl 1 $sl
addi $5 0 $0
store $sl $0 $5
addi $sl 1 $sl
store $sl $0 $0
addi $sl 1 $sl
addi $2 12 $0
load $6 $3 $0
store $2 $0 $6
addi $5 1 $2
store $5 $0 $0
addi $3 1 $3
load $3 $3 $0
beq $6 $0 L24
store $5 $0 $sl
L23: load $5 $3 $0
store $sl $0 $5
addi $sl 1 $sl
addi $5 1 $sl
store $sl $0 $5
addi $sl 1 $sl
addi $3 1 $3
load $3 $3 $0
bne $3 $0 L23
L24: addi $3 1 $0
sub $sl $sl $3
store $sl $0 $0
addi $sl 1 $sl
L1: addi $3 12 $0
addi $4 4 $0
addi $2 0 $0
L21: load $5 $3 $0
load $6 $4 $0
bne $5 $6 L22
addi $3 1 $3
addi $4 1 $4
load $3 $3 $0
load $4 $4 $0
bne $3 $4 L21
addi $2 1 $0
L22: beq $2 $0 L4
jmp L2
jmp L3
L4: addi $4 8 $0
addi $3 10 $0
load $7 $4 $0
store $3 $0 $7
addi $6 1 $3
store $6 $0 $0
addi $4 1 $4
load $4 $4 $0
beq $7 $0 L20
store $6 $0 $sl
L19: load $6 $4 $0
store $sl $0 $6
addi $sl 1 $sl
addi $6 1 $sl
store $sl $0 $6
addi $sl 1 $sl
addi $4 1 $4
load $4 $4 $0
bne $4 $0 L19
L20: addi $4 1 $0
sub $sl $sl $4
store $sl $0 $0
addi $sl 1 $sl
addi $6 2 $0
addi $7 8 $0
addi $4 0 $sl
addi $14 0 $0
addi $6 1 $6
addi $7 1 $7
addi $8 0 $0
addi $12 100 $0
L15: load $6 $6 $0
load $7 $7 $0
beq $6 $7 L16
load $10 $6 $0
load $11 $7 $0
add $9 $8 $10
add $9 $9 $11
slt $8 $12 $9
beq $8 $0 L18
sub $9 $9 $12
L18: addi $sl 1 $sl
addi $13 1 $sl
store $sl $0 $13
addi $sl 1 $sl
store $sl $0 $9
addi $14 1 $14
addi $6 1 $6
addi $7 1 $7
jmp L15
L16: beq $8 $0 L17
addi $sl 1 $sl
addi $13 1 $sl
store $sl $0 $13
addi $sl 1 $sl
store $sl $0 $8
L17: addi $sl 1 $sl
store $sl $0 $0
addi $sl 1 $sl
store $4 $0 $14
addi $3 8 $0
load $7 $4 $0
store $3 $0 $7
addi $6 1 $3
store $6 $0 $0
addi $4 1 $4
load $4 $4 $0
beq $7 $0 L14
store $6 $0 $sl
L13: load $6 $4 $0
store $sl $0 $6
addi $sl 1 $sl
addi $6 1 $sl
store $sl $0 $6
addi $sl 1 $sl
addi $4 1 $4
load $4 $4 $0
bne $4 $0 L13
L14: addi $4 1 $0
sub $sl $sl $4
store $sl $0 $0
addi $sl 1 $sl
addi $4 10 $0
addi $3 2 $0
load $7 $4 $0
store $3 $0 $7
addi $6 1 $3
store $6 $0 $0
addi $4 1 $4
load $4 $4 $0
beq $7 $0 L12
store $6 $0 $sl
L11: load $6 $4 $0
store $sl $0 $6
addi $sl 1 $sl
addi $6 1 $sl
store $sl $0 $6
addi $sl 1 $sl
addi $4 1 $4
load $4 $4 $0
bne $4 $0 L11
L12: addi $4 1 $0
sub $sl $sl $4
store $sl $0 $0
addi $sl 1 $sl
addi $6 12 $0
addi $7 0 $sl
addi $8 1 $0
store $sl $0 $8
addi $sl 1 $sl
addi $8 1 $sl
store $sl $0 $8
addi $sl 1 $sl
addi $8 1 $0
store $sl $0 $8
addi $sl 1 $sl
store $sl $0 $0
addi $sl 1 $sl
addi $4 0 $sl
addi $14 0 $0
addi $6 1 $6
addi $7 1 $7
addi $8 0 $0
addi $12 100 $0
L7: load $6 $6 $0
load $7 $7 $0
beq $6 $7 L8
load $10 $6 $0
load $11 $7 $0
add $9 $8 $10
add $9 $9 $11
slt $8 $12 $9
beq $8 $0 L10
sub $9 $9 $12
L10: addi $sl 1 $sl
addi $13 1 $sl
store $sl $0 $13
addi $sl 1 $sl
store $sl $0 $9
addi $14 1 $14
addi $6 1 $6
addi $7 1 $7
jmp L7
L8: beq $8 $0 L9
addi $sl 1 $sl
addi $13 1 $sl
store $sl $0 $13
addi $sl 1 $sl
store $sl $0 $8
L9: addi $sl 1 $sl
store $sl $0 $0
addi $sl 1 $sl
store $4 $0 $14
addi $3 12 $0
load $7 $4 $0
store $3 $0 $7
addi $6 1 $3
store $6 $0 $0
addi $4 1 $4
load $4 $4 $0
beq $7 $0 L6
store $6 $0 $sl
L5: load $6 $4 $0
store $sl $0 $6
addi $sl 1 $sl
addi $6 1 $sl
store $sl $0 $6
addi $sl 1 $sl
addi $4 1 $4
load $4 $4 $0
bne $4 $0 L5
L6: addi $4 1 $0
sub $sl $sl $4
store $sl $0 $0
addi $sl 1 $sl
L3: jmp L1
L2: halt
