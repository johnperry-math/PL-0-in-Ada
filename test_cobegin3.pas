VAR A, B;

BEGIN
  COBEGIN
    A := 1;
    WRITELN(A);
    A := 2;
    WRITELN(A);
    A := 3;
    WRITELN(A);
    A := 4;
    WRITELN(A);
    A := 5;
    WRITELN(A);
    A := 6;
    WRITELN(A);
    A := 7;
    WRITELN(A);
  COEND;
  WRITELN(A, B);
END;
