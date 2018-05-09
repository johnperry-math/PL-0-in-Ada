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
             3:WRITELN(A,B1,C);
             B1:IF A+1=B THEN
                IF 10=A THEN ELSE WRITE(A,C)
               ELSE WRITE(B1,C);
            CEND;
END.