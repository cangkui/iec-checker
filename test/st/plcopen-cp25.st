PROGRAM demo
    VAR
        I : INT := 10;
        J : REAL := 0.55;
        a1 : BOOL; a2 : BYTE; a3 : WORD; a4 : DWORD; a5 : LWORD;
        b1 : SINT; b2 : INT; b3 : DINT; b4 : LINT;
        c1 : USINT; c2 : UINT; c3 : UDINT; c4 : ULINT;
        d1 : REAL; d2 : LREAL;
        e1 : TIME; e2 : LTIME;
        f1 : DT; f2 : LDT;
        g1 : DATE; g2 : LDATE;
        h1 : TOD; h2 : LTOD;
        i1 : CHAR; i2 : WCHAR; i3 : STRING[10]; i4 : WSTRING[10];
    END_VAR

  (* allowed implicit conversions *)
  a2 := a1; (* BOOL -> BYTE *)
  a3 := a2; (* BYTE -> WORD *)
  a4 := a3; (* WORD -> DWORD *)
  a5 := a4; (* DWORD -> LWORD *)
  
  b2 := b1; (* SINT -> INT *)
  b3 := b2; (* INT -> DINT *)
  b4 := b3; (* DINT -> LINT *)
  
  d2 := d1; (* REAL -> LREAL *)
  
  c2 := c1; (* USINT -> UINT *)
  c3 := c2; (* UINT -> UDINT *)
  c4 := c3; (* UDINT -> ULINT *)
  
  e2 := e1; (* TIME -> LTIME *)
  f2 := f1; (* DT -> LDT *)
  g2 := g1; (* DATE -> LDATE *)
  h2 := h1; (* TOD -> LTOD *)
  
  i2 := i1; (* CHAR -> WCHAR *)
  i4 := i3; (* STRING -> WSTRING *)
  
  i3 := i1; (* CHAR -> STRING *)
  i4 := i2; (* WCHAR -> WSTRING *)
  
  (* Implicit conversion not allowed - triggers warning *)
  I := J; (* REAL -> INT - not allowed *)
  
  (* Other examples of disallowed conversions *)
  b1 := b3; (* DINT -> SINT - not allowed *)
  c1 := c2; (* UINT -> USINT - not allowed *)
  a1 := a2; (* BYTE -> BOOL - not allowed *)
  d1 := d2; (* LREAL -> REAL - not allowed *)
  
  (* The correct way to write requires explicit conversion *)
  I := REAL_TO_INT(J);
  J := INT_TO_REAL(I);
  b1 := DINT_TO_SINT(b3);
  c1 := UINT_TO_USINT(c2);
  a1 := BYTE_TO_BOOL(a2);
  d1 := LREAL_TO_REAL(d2);

END_PROGRAM