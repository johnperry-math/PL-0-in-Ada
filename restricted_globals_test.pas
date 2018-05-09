CONST
    A = 5, B = 10;
VAR
    C, D;

PROCEDURE ONE;
    VAR B, C;

    PROCEDURE TWO;
        VAR C, D;
        BEGIN
            IF C = D THEN
                CALL TWO;
            IF A <> D THEN
                CALL ONE;
        END;

    PROCEDURE THREE;
        VAR X, Y, Z, A, B, C;
        BEGIN
            IF C < X THEN
            BEGIN
                CALL THREE;
                CALL ONE;
            END;
        END;

    BEGIN
        IF B <= -(5 + C * A) THEN
            B := B + 1;
    END;

VAR
    E;

BEGIN
    C := +(C + 1);
    CALL ONE;
    WHILE A > A / A DO
        WHILE B >= B DO
            IF ODD E THEN
                E := E - 1;
END.
