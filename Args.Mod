MODULE Args;

IMPORT ProgramArgs, TextRider, Out;

CONST space = 10;

VAR	r:TextRider.Reader;
	(*str: ARRAY 256 OF CHAR;*)

PROCEDURE ArgNum * ():LONGINT;
	BEGIN
		r:= TextRider.ConnectReader(ProgramArgs.args);
		IF (r = NIL) THEN
			Out.String("Cannot read arguments!");
			Out.Ln();
		END;
		RETURN ProgramArgs.args.ArgNumber();
	END ArgNum;

PROCEDURE GetArg * (k: LONGINT; VAR str: ARRAY OF CHAR);
	VAR	i,j: LONGINT;		
		ch: CHAR;
	BEGIN		
		IF (ArgNum() = 0) THEN
			str[0]:=0X;
			RETURN;
		END;
		FOR i:= 0 TO k DO
			COPY("", str);
			j:=0;
			r.ReadChar(ch);
			WHILE (ORD(ch) # space) DO
				str[j]:= ch;
				r.ReadChar(ch);
				INC(j);
			END;
			str[j]:=0X;
			
		END;
	
	END GetArg;

BEGIN

	(*Out.Int(ArgNum(),0);Out.Ln();
	IF(GetArg(3,str)) THEN	
		Out.String("str=");Out.String(str);Out.Ln();
	END;*)

END Args.
