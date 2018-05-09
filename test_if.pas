CONST
   A = 5;

VAR
   B, C;

BEGIN
   B := 2;
   C := A;
   IF A = C THEN
      WRITELN(A)
   ELSE
      WRITELN(B);
   IF ODD B THEN
      WRITELN(B)
   ELSE
      WRITELN(A+B);
END.
