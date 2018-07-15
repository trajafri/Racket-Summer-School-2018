#lang brag

taco-program : [/"\n"] [taco-leaf] [/"\n"] [@taco-program]
taco-leaf : /"#" (taco | not-a-taco){7} /"$"
taco : /"%"
not-a-taco : /"#" /"$"

