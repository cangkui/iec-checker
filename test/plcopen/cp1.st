TYPE MyStruct: 
STRUCT
    A : INT;
    B : INT;
END_STRUCT;
END_TYPE

FUNCTION FC_OffsetAccess : INT
VAR
    (* Map the STRUCTURE to a memory start address *)
    ms AT %MW500 : MyStruct;
    B_ByOffset: INT := 1;
END_VAR

(* Access B by offset(assume that %MW508 is the offset of B) *)
%MW508 := B_ByOffset;
(* Actually we should use ms.B like this: *)
(* ms.B := B_ByOffset; *)

END_FUNCTION