VAR A, B;

PROCEDURE ONE;
VAR X, Y;
PROCEDURE TWO;
BEGIN
  A := A - 1;
END;
BEGIN
  X := 2;
  Y := A;
  B := X + Y;
  A := A + 1;
  CALL TWO;
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
