CONST
        A=10;
VAR
        B,C;

PROCEDURE TWO;
VAR
        A2,B2;
BEGIN
        A2:=1;
        REPEAT
                WRITELN(A2);
                A2:=A2+1;
        UNTIL A2>=3;
        REPEAT
              B2:=1;
                    REPEAT
                          WRITELN(A2,B2);
                          B2:=B2+2;
                    UNTIL B2>A2;
              A2:=A2+1
        UNTIL A2>5;
END;

VAR
        M;
PROCEDURE ONE;
CONST
        A=5;
VAR
        B1,C1,D1;
BEGIN
        M:=30;
        CALL TWO;
        FOR B1:=1 TO A DO
         FOR C1:=1 TO B1 DO
          WRITELN(B1,C1,B1*C1);
         FOR B1:=10 DOWNTO A DO
          WRITE(B1);
         FOR B1:=10 TO A DO
          WRITELN(B1);
         FOR B1:=1 TO C DO
          BEGIN
           C:=C+1;
           WRITELN(B1,C)
          END
END;
VAR
        STATE;
BEGIN
        B:=11+A*(A-A);
        FOR C:=1-A+A TO B+1 DO
            CASE C+1 OF
             3:WRITELN(A,B,C);
             A:IF A+1=B THEN
                IF 10=A THEN ELSE WRITE(A,C)
               ELSE WRITE(B,C);
             11:IF A+B=3 THEN
                 IF 10=A THEN WRITELN(A,C)
                 ELSE
                ELSE WRITELN(B,C);
            CEND;
        STATE :=0;
        REPEAT
              CASE STATE+1 OF
                   1:BEGIN
                          WRITE(1);
                          STATE:=2
                     END;
                   2:WRITELN(2);
                   3:BEGIN
                      WRITELN(STATE+3);
                      STATE:=4
                     END;
                   4:WRITELN(STATE*4);
              CEND
        UNTIL STATE=4;
        C:=4;
        CALL ONE;
END.