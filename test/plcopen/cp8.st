FUNCTION test1 : REAL
VAR
  a : REAL;
  b: REAL := 0.0;
END_VAR

IF b > 1.0 THEN
  a := 1.0;
END_IF;
test1 := a + b;
END_FUNCTION

// ---------------------------------------------

FUNCTION test2 : REAL
VAR
  a : REAL := 1.0;
  b: REAL := 1.0;
END_VAR

IF b = 0.0 THEN
  a := 1.0;
END_IF;
test2 := a + b;
END_FUNCTION

// ---------------------------------------------

FUNCTION test3 : REAL
VAR
  a : REAL := 1.0;
  b: REAL := 1.0;
END_VAR

IF b <> 0.0 THEN
  a := 1.0;
END_IF;
test3 := a + b;
END_FUNCTION

// ---------------------------------------------

FUNCTION test4 : REAL
VAR
  a : REAL := 1.0;
  b: REAL := 1.0;
END_VAR

IF b <> 1.0 THEN
  a := 1.0;
END_IF;
test4 := a + b;
END_FUNCTION

// ---------------------------------------------

FUNCTION test5 : REAL
VAR
  a : REAL := 1.0;
  b: REAL := 1.0;
END_VAR

IF b = 1.0 THEN
  a := 1.0;
END_IF;
test5 := a + b;
END_FUNCTION

// ---------------------------------------------

FUNCTION test6 : REAL
VAR
  a : REAL := 0.0;
  b: REAL := 1.0;
END_VAR

IF a = b THEN
  a := 1.0;
END_IF;
test6 := a + b;
END_FUNCTION

// ---------------------------------------------

FUNCTION test7 : REAL
VAR
  a : REAL := 0.0;
  b: REAL := 1.0;
END_VAR

IF a <> b THEN
  a := 1.0;
END_IF;
test7 := a + b;
END_FUNCTION

// ---------------------------------------------

FUNCTION test8 : REAL
VAR
  a : REAL := 1.0;
  b: REAL := 1.0;
END_VAR

IF a = b THEN
  a := 1.0;
ELSIF a <> b THEN
  a := 2.0;
END_IF;
test8 := a + b;

END_FUNCTION

// ---------------------------------------------

FUNCTION test9 : INT
VAR
  a : INT := 1;
  b: INT := 1;
END_VAR

IF INT_TO_REAL(a) <> INT_TO_REAL(b) THEN
  a := 2;
END_IF;
test9 := a + b;
END_FUNCTION