#pragma list off
PROGRAM aaa(a, b, c);
VAR d, e: INTEGER;
VAR f, g: REAL;

BEGIN
    IF a+b<c[3]
    THEN
        f:=a+3
    ELSE
        f:=a*5;
    
    WHILE a>b
    DO BEGIN
        a:=b;
        b:=c;
        c:=d;
    END

END.