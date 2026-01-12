// 1 there is no init Violation
FUNCTION no_init_restart : INT
VAR
  some_var : INT;
END_VAR

(* Assuming some_var is 0 every time, which is dangerous *)
IF some_var > 5 THEN
  no_init_restart := some_var;
END_IF;

END_FUNCTION

// 2 there is no init Violation
FUNCTION Violation_DirectRead : INT
  VAR
    counter : INT := 0;
    some_var : INT; // Declared but not initialized
  END_VAR
  
  // Violation: some_var is read here for addition, but it has not been assigned
  counter := some_var + 1; 
  
  some_var := 16#42;
  Violation_DirectRead := counter;
END_FUNCTION

// 3 there is no init Violation
FUNCTION Violation_Hidden : INT
  VAR
    counter : INT := 0;
    offset : INT;    // Forgot to initialize.
    base_val : INT := 100;
  END_VAR

  // Violation: although base_val has a value, offset is uninitialized
  counter := base_val + offset; 
  
  Violation_Hidden := counter;
END_FUNCTION

// 4 all variables are initialized
FUNCTION Violation_None : INT
  VAR
    counter : INT := 0;
    offset : INT;    // Forgot to initialize.
    base_val : INT := 100;
  END_VAR

  offset := 42;
  counter := base_val + offset; 
  
  Violation_None := counter;
END_FUNCTION

// 5 there is no init Violation
FUNCTION Violation_FuncCall1 : INT
  VAR
    counter : INT := 0;
    offset : INT;    // Forgot to initialize.
    base_val : INT := 100;
  END_VAR

  counter := func(base_val, offset);
  
  Violation_FuncCall1 := counter;
END_FUNCTION

// 6 there is no init Violation
FUNCTION Violation_FuncCall2 : INT
  VAR
    counter : INT := 0;
    offset : INT;    // Forgot to initialize.
    base_val : INT := 100;
  END_VAR
  (* In fact offset should not be alarmed here because offset is assigned but not used. *)
  counter := func(x:=base_val, y=>offset);
  
  Violation_FuncCall2 := counter;
END_FUNCTION


// 7 there is no Violation
FUNCTION Violation_FuncCall3 : INT
  VAR
    counter : INT := 0;
    offset : INT;
    base_val : INT := 100;
  END_VAR
  
  Violation_FuncCall3 := counter;
END_FUNCTION

// 8 there is Violation
FUNCTION Violation_FuncCall4 : INT
  VAR
    counter : INT := 0;
    offset : INT;
    base_val : INT := 100;
  END_VAR
  func(base_val, offset);
END_FUNCTION

// 9 there is Violation
FUNCTION Violation_self : INT
  VAR
    counter : INT := 0;
    offset : INT;
    base_val : INT := 100;
  END_VAR
  // offset := 1;
  offset := offset + 1;
END_FUNCTION