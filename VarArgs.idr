AdderType : Num numType => (numargs : Nat) -> Type -> Type
AdderType Z numType = numType
AdderType (S k) numType = numType -> AdderType k numType

adder : Num numType => (n : Nat) -> numType -> AdderType n numType
adder Z x = x
adder (S k) x = (adder k) . (+ x)

adder2 : Num numType => (n : Nat) -> AdderType n numType
adder2 Z = 0
adder2 (S k) = \x => (adder k) x
