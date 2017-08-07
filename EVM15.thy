theory EVM15

(* This is a formalization of EIP615 *)

imports Main

begin

type_synonym bytes_len = nat
type_synonym position = nat

datatype instruction =
  UsualInstruction bytes_len -- "UsualInstruction n means an instruction that occupies n + 1 bytes on the program"
| JUMPTO position -- "JUMPTO tgt is JUMPTO instruction with an immediate argument tgt"
| JUMPIF position -- "JUMPIF tgt is JUMPIF instruction with an immeidate argument tgt"
| BEGINSUB nat nat -- "BEGINSUB n_args n_results"
| JUMPSUB position
| RETURNSUB
| BEGINDATA
| JUMPDEST
(* for later
| JUMPV "position list"
| JUMPSUBV "position list"
*)
(* for later
| PUTLOCAL
| GETLOCAL
*)

fun length_of_instruction :: "instruction \<Rightarrow> bytes_len"
where
  "length_of_instruction (UsualInstruction n) = n + 1"
| "length_of_instruction (JUMPTO _) = 5"
| "length_of_instruction (JUMPIF _) = 5"
| "length_of_instruction (BEGINSUB _ _) = 3"
| "length_of_instruction (JUMPSUB _) = 5"
| "length_of_instruction RETURNSUB = 1"
| "length_of_instruction BEGINDATA = 1"
| "length_of_instruction JUMPDEST = 1"
(* for later
| "length_of_instruction (JUMPV lst) = 5 + 4 * (length lst)"
| "length_of_instruction (JUMPSUBV lst) = 5 + 4 * (length lst)"
*)

type_synonym program = "instruction list"

datatype lookup_result =
  Hit instruction
| InTheMiddleOfInstruction
| OutOfProgram

fun lookup :: "program \<Rightarrow> position \<Rightarrow> lookup_result"
where
  "lookup [] _ = OutOfProgram"
| "lookup (BEGINDATA # _) _ = OutOfProgram"
| "lookup (i # _) 0 = Hit i"
| "lookup (i # rest) pos =
    (if length_of_instruction i > pos then
       InTheMiddleOfInstruction
     else lookup rest (pos - (length_of_instruction i)))"

(* define the linear time validation *)


(* define runtime execution *)




(* prove that, if linear time validation passes, runtime execution never goes astray *)

end