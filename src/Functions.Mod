MODULE Functions;
IMPORT IntStr;

PROCEDURE LIntToStr * (n: LONGINT; VAR str: ARRAY OF CHAR);
	BEGIN
		IntStr.IntToStr(n,str);
	END LIntToStr;

BEGIN
END Functions.
