ADDI R2 R0 1
inner:
BEQ R3 R1 innerend
LW R4 R3 -1
LW R5 R3 0
BLT R5 R4 swap
afterswap:
ADDI R3 R3 1
BEQ R0 R0 inner
innerend:
BEQ R0 R0 outer
end:
HALT
swap:
SW R4 R3 0
SW R5 R3 -1
ADDI R2 R0 1
BEQ R0 R0 afterswap
