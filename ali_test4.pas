CONST
        A=10;
VAR
        B1,C;

PROCEDURE ONE;
CONST
        A=5;
VAR
        C1,D1;
BEGIN
        D1:=30;
        FOR B1:=10 DOWNTO A DO
         WRITE(B1);
        FOR B1:=1*C TO C DO
           WRITELN(B1,C)
END;
BEGIN
        B1:=11+A*(A-A);
        FOR C:=1-A+A TO B1+1 DO
            CASE C+1 OF
            1:IF B1=A+1 THEN
                IF A=10 THEN ELSE WRITE(A,C)
               ELSE WRITE(B1,C);
             3:WRITELN(A,B1,C);
            CEND;
        CALL ONE
END.

