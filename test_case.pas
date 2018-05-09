CONST
   A = 5,
   B = 10,
   C = 3;

VAR D;

BEGIN
   D := 0;
   CASE D OF
      A: WRITELN(A*B);
      B: WRITELN(B*C);
      C: WRITELN(C*D);
      0: WRITELN(0);
   CEND;
   WRITELN(-1);
END.
