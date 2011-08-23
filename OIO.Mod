MODULE OIO; 

IMPORT Files, TextRider, Out, Msg, RealMath; 

TYPE	File* = RECORD 
		filename : ARRAY 23 OF CHAR; 
		filedesc : Files.File;
		r: TextRider.Reader; 
		res: Msg.Msg;
	END;
VAR	(*f : File;*) 
	(* reader is oo2c specific *)
	(*r: TextRider.Reader;  *)

PROCEDURE OpenFile* (name :ARRAY OF CHAR; VAR f: File); 
	BEGIN 
		COPY(name, f.filename);
		f.filedesc:= Files.Old (f.filename, {Files.read}, f.res);
		f.r:=TextRider.ConnectReader(f.filedesc);
		IF (f.res # Files.done) THEN
			Out.String("Cannot open file ");
			Out.String(name);
			Out.String("!"); Out.Ln;
		END;
	END OpenFile; 

PROCEDURE SetPos* (f:File; newPos: LONGINT); 
	BEGIN 
		f.r.SetPos(newPos);
	END SetPos; 

PROCEDURE Pos* (f:File):LONGINT; 
	BEGIN 
		RETURN f.r.Pos();
	END Pos; 

PROCEDURE ReadChar* (f: File; VAR ch : CHAR); 
	BEGIN
		f.r.ReadChar(ch); 
	END ReadChar; 

PROCEDURE CloseFile* (f : File); 
	BEGIN
		f.filedesc.Close();
END CloseFile;

PROCEDURE WriteString*(str: ARRAY OF CHAR);
	BEGIN
		Out.String(str);
	END WriteString;

PROCEDURE WriteChar*(ch: CHAR);
	BEGIN
		Out.Char(ch);
	END WriteChar;

PROCEDURE WriteLongInt*(int: LONGINT; n: LONGINT);
	BEGIN
		Out.LongInt(int,n);
	END WriteLongInt;

PROCEDURE WriteHex*(lint: LONGINT; n: LONGINT);
	BEGIN
		Out.Hex(lint,n);
	END WriteHex;

PROCEDURE WriteLn*();
	BEGIN
		Out.Ln;
	END WriteLn;

PROCEDURE Ten*(e:INTEGER):REAL;
	VAR r:REAL;
	BEGIN
		r:=RealMath.power(10,e);
		RETURN r;
	END Ten;
BEGIN

END OIO.
