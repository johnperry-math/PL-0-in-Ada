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
        FOR F:=1*C TO C DO
           WRITELN(B1,C)
END;
BEGIN
        B1:=11+A*(A-A);
        CALL ONE;
END.