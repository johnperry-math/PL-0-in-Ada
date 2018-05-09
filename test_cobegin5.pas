VAR A, B, C, D;

PROCEDURE ONE;
VAR X;
BEGIN
  X := THREAD;
  WRITELN(A, B, X);
END;

BEGIN
  COBEGIN
    A := THREAD;
    B := THREAD;
    C := THREAD;
    D := B + C;
  COEND;
  WRITELN(A, B, C, D);
END;
