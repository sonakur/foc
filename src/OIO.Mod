MODULE OIO; 

IMPORT Files, TextRider, Out, Msg, RealMath, BinaryRider; 

TYPE	File* = RECORD 
		filename : ARRAY 23 OF CHAR; 
		filedesc : Files.File;
		r: TextRider.Reader;
		w: TextRider.Writer; 
		res: Msg.Msg;
	END;

	BinFile* = RECORD 
		filename : ARRAY 23 OF CHAR; 
		filedesc : Files.File;
		r: BinaryRider.Reader;
		w: BinaryRider.Writer; 
		res: Msg.Msg;
	END;
	TextWriter* = TextRider.Writer;
	FileDesc* = Files.File;
VAR	(*f : File;*) 
	(* reader is oo2c specific *)
	(*r: TextRider.Reader;  *)


PROCEDURE ReturnWriter* (f: File):TextRider.Writer;
	BEGIN
		RETURN f.w;
	END ReturnWriter;

PROCEDURE SetWriter* (f: File; w:TextWriter);
	BEGIN
		f.w := w;
	END SetWriter;

PROCEDURE ReturnFileDesc* (f: BinFile):Files.File;
	BEGIN
		RETURN f.filedesc;
	END ReturnFileDesc;

PROCEDURE SetFileDesc * (VAR f: BinFile; desc: Files.File);
	BEGIN
		f.filedesc := desc; 
	END SetFileDesc;

PROCEDURE OpenFile* (name :ARRAY OF CHAR; VAR f: File):BOOLEAN; 
	BEGIN 
		COPY(name, f.filename);
		f.filedesc:= Files.Old (f.filename, {Files.read}, f.res);
		f.r:=TextRider.ConnectReader(f.filedesc);
		IF (f.res # Files.done) THEN
			Out.String("Cannot open file ");
			Out.String(name);
			Out.String("!"); Out.Ln;
			RETURN FALSE;
		END;
		RETURN TRUE;
	END OpenFile; 

PROCEDURE OpenNewFile* (name :ARRAY OF CHAR; VAR f: File):BOOLEAN; 
	BEGIN 
		COPY(name, f.filename);
		f.filedesc:= Files.New (f.filename, {Files.write}, f.res);
		f.w:=TextRider.ConnectWriter(f.filedesc);
		IF (f.res # Files.done) THEN
			Out.String("Cannot create file ");
			Out.String(name);
			Out.String("!"); Out.Ln;
			RETURN FALSE;
		END;
		RETURN TRUE;
	END OpenNewFile; 

PROCEDURE OpenBinFile* (name :ARRAY OF CHAR; VAR f: BinFile):BOOLEAN; 
	BEGIN 
		COPY(name, f.filename);
		f.filedesc:= Files.Old (f.filename, {Files.read}, f.res);
		f.r:=BinaryRider.ConnectReader(f.filedesc);
		IF (f.res # Files.done) THEN
			Out.String("Cannot open file ");
			Out.String(name);
			Out.String("!"); Out.Ln;
			RETURN FALSE;
		END;
		RETURN TRUE;
	END OpenBinFile; 

PROCEDURE OpenNewBinFile* (name :ARRAY OF CHAR; VAR f: BinFile):BOOLEAN; 
	BEGIN 
		COPY(name, f.filename);
		f.filedesc:= Files.New (f.filename, {Files.write, Files.read}, f.res);
		f.w:=BinaryRider.ConnectWriter(f.filedesc);
		IF (f.res # Files.done) THEN
			Out.String("Cannot create file ");
			Out.String(name);
			Out.String("!"); Out.Ln;
			RETURN FALSE;
		END;
		RETURN TRUE;
	END OpenNewBinFile; 

PROCEDURE ConnectBinWriter *(VAR f: BinFile; desc: FileDesc);
	BEGIN
		f.w := BinaryRider.ConnectWriter(desc);
	END ConnectBinWriter;

PROCEDURE SetPos* (VAR f:File; newPos: LONGINT); 
	BEGIN 
		f.r.SetPos(newPos);
	END SetPos; 

PROCEDURE SetBinWriterPos* (VAR f:BinFile; newPos: LONGINT); 
	BEGIN 
		f.w.SetPos(newPos);
	END SetBinWriterPos;

(*PROCEDURE BinFileDescLength *(f: BinFile):LONGINT;
	BEGIN
		RETURN f.filedesc.Length();
	END BinFileDescLength;
*)
PROCEDURE Pos* (VAR f:File):LONGINT; 
	BEGIN 
		RETURN f.r.Pos();
	END Pos; 

PROCEDURE ReadChar* (VAR f: File; VAR ch : CHAR); 
	BEGIN
		f.r.ReadChar(ch); 
	END ReadChar; 

PROCEDURE ReadBinChar* (VAR f: BinFile; VAR ch : CHAR); 
	BEGIN
		f.r.ReadChar(ch); 
	END ReadBinChar;
 
PROCEDURE ReadLInt* (VAR f: File; VAR n: LONGINT); 
	BEGIN
		f.r.ReadLInt(n); 
	END ReadLInt; 

PROCEDURE ReadBinLInt* (VAR f: BinFile; VAR n: LONGINT); 
	BEGIN
		f.r.ReadLInt(n); 
	END ReadBinLInt; 

PROCEDURE ReadSInt* (VAR f: File; VAR n: SHORTINT); 
	BEGIN
		f.r.ReadSInt(n); 
	END ReadSInt; 

PROCEDURE ReadBinSInt* (VAR f: BinFile; VAR n: SHORTINT); 
	BEGIN
		f.r.ReadSInt(n); 
	END ReadBinSInt; 

PROCEDURE ReadBinInt* (VAR f: BinFile; VAR n: INTEGER); 
	BEGIN
		f.r.ReadInt(n); 
	END ReadBinInt; 

PROCEDURE ReadBinString* (VAR f: BinFile; VAR str: ARRAY OF CHAR); 
	BEGIN
		f.r.ReadString(str); 
	END ReadBinString; 

PROCEDURE CloseFile* (VAR f : File); 
	BEGIN
		f.filedesc.Close();
END CloseFile;

PROCEDURE CloseBinFile* (VAR f : BinFile); 
	BEGIN
		f.filedesc.Close();
END CloseBinFile;

PROCEDURE WriteSet* (VAR f: File; s: SET);
	BEGIN
		f.w.WriteSet(s);		
	END WriteSet;

PROCEDURE FWriteString*(VAR f: File; str: ARRAY OF CHAR);
	BEGIN
		f.w.WriteString(str);
	END FWriteString;

PROCEDURE FWriteLn*(VAR f: File);
	BEGIN
		f.w.WriteLn()
	END FWriteLn;

PROCEDURE FWriteLInt*(VAR f: File; n: LONGINT); (* write in file *)
	BEGIN
		f.w.WriteLInt(n,0);
	END FWriteLInt;


(*PROCEDURE WriteBinLn*(f: BinFile);
	BEGIN
		f.w.WriteLn()
	END WriteBinLn;
*)
PROCEDURE WriteString*(str: ARRAY OF CHAR);
	BEGIN
		Out.String(str);
	END WriteString;

PROCEDURE WriteInt*(num: INTEGER; n: LONGINT);
	BEGIN
		Out.Int(num, n);
	END WriteInt;

PROCEDURE WriteBinString*(VAR f: BinFile; str: ARRAY OF CHAR); (*write in file *)
	BEGIN
		f.w.WriteString(str);
	END WriteBinString;

PROCEDURE WriteChar*(ch: CHAR); (* write in standard output *)
	BEGIN
		Out.Char(ch);
	END WriteChar;

PROCEDURE WriteBinChar*(VAR f: BinFile; ch: CHAR); (* write in file *)
	BEGIN
		f.w.WriteChar(ch);
	END WriteBinChar;

PROCEDURE WriteBinLInt*(VAR f: BinFile; n: LONGINT); (* write in file *)
	BEGIN
		f.w.WriteLInt(n);
	END WriteBinLInt;

PROCEDURE WriteBinSInt*(VAR f: BinFile; n: SHORTINT); (* write in file *)
	BEGIN
		f.w.WriteSInt(n);
	END WriteBinSInt;

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

PROCEDURE BinFileUpdate*(VAR f: BinFile);
	BEGIN
		f.filedesc.Flush();				
	END BinFileUpdate;

PROCEDURE FileUpdate*(VAR f: File);
	BEGIN
		f.filedesc.Flush();				
	END FileUpdate;

PROCEDURE BinFileLength*(VAR f: BinFile):LONGINT;
	VAR l: LONGINT;
	BEGIN
		l:=f.filedesc.Length();
		RETURN l;
	END BinFileLength;

PROCEDURE UpdateWriter*();
	BEGIN
		Out.Flush();
	END UpdateWriter;

PROCEDURE Available*(VAR f: BinFile):LONGINT;
	VAR l: LONGINT;
	BEGIN
		l:=f.r.Available();
		RETURN l;
	END Available;

PROCEDURE Register*(VAR f: BinFile);
	BEGIN
		f.filedesc.Register();				
	END Register;

BEGIN

END OIO.
