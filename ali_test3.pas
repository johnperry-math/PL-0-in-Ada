CONST
        A=10;
VAR
        B1;

PROCEDURE ONE;
CONST
        A=3;
VAR
        J,D1;
BEGIN
        D1:=0;
        REPEAT
              D1:=D1+5;
        UNTIL D1*A=30;
        J:=A*A;
        REPEAT
               J:=J+1;
               WRITELN(J,J*A+2);
               WRITE(J);
        UNTIL J>30;



END;
BEGIN
        CALL ONE;
END.

