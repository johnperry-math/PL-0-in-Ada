VAR A, B;

PROCEDURE ONE;
VAR X, Y;
BEGIN
  X := 2;
  Y := A;
  B := X + Y;
  A := A + B;
  WRITELN(THREAD, A);
END;

BEGIN
  COBEGIN
    CALL ONE;
    CALL ONE;
    CALL ONE;
    CALL ONE;
  COEND;
  WRITELN(A, B);
END;
