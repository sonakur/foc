MODULE OAVS0; (* NW 19.9.93 / 16.9.98 / 23.4.2007*)
	IMPORT OIO, SYSTEM; (*, Reals, AosIO:=Streams;*)
	
(* fof general replacements: 
	Texts.Reader => AosIO.Reader
	Texts.Writer => AosIO.Writer
	Texts.WriteString(W,  => W.String(
	Texts.WriteInt(W, => W.Int(
	Texts.Write(W, => W.Char(
	Texts.WriteLn(W => W.Ln(
	Texts.Append(Oberon.Log, W.buf) => W.Update()
	Texts.Pos(R) => R.Pos()
	
	Texts.Read(R, => R.Char(
	
	R.eot => (ch = 0X)
*) 

	
(* Oberon-SA Scanner does lexical analysis. Input is Oberon-Text, output is
	sequence of symbols, i.e identifiers, numbers, strings, and special symbols.
	Recognises all Oberon keywords and skips comments. The 36 keywords are
	recorded in a hash table. Strings are stored in the string table str.
	Get(sym) delivers next symbol from input text with Reader R.
	Mark(msg) records error and delivers error message with Writer W;
	CopyId and NotEql are to copy and compare the identifier read *)
	
	CONST IdLen* = 32; WS = 2; (*Word size*)
		KTsize = 59; maxExp = 38; stringTabSize = 1024;
		(*lexical symbols*)
		null = 0; times* = 1; rdiv* = 2; div* = 3; mod* = 4;
		and* = 5; plus* = 6; minus* = 7; or* = 8; eql* = 9;
		neq* = 10; lss* = 11; leq* = 12; gtr* = 13; geq* = 14;
		in* = 15; is* = 16; arrow* = 17; period* = 18;
		char* = 20; int* = 21; real* = 22; false* = 23; true* = 24;
		nil* = 25; string* = 26; not* = 27; lparen* = 28; lbrak* = 29;
		lbrace* = 30; ident* = 31; if* = 32; case* = 33; while* = 34;
		repeat* = 35; for* = 36;  with* = 37; assert* = 38;
		comma* = 40; colon* = 41; becomes* = 42; upto* = 43; rparen* = 44;
		rbrak* = 45; rbrace* = 46; then* = 47; of* = 48; do* = 49;
		to* = 50; by* = 51; semicolon* = 52; end* = 53; bar* = 54;
		else* = 55; elsif* = 56; until* = 57; return* = 58;
		array* = 60; record* = 61; pointer* = 62; const* = 63; type* = 64;
		var* = 65; procedure* = 66; begin* = 67; import* = 68; module* = 69;
		eof = 70;
(*BohdanT*)
		loop* =71;exit* = 72;

	TYPE Ident* = ARRAY IdLen OF CHAR;

		(** tt, fof >> *)
	Errors = POINTER TO ErrorsDesc;
	ErrorsDesc = RECORD 
		pos, err: LONGINT;  
		msg: ARRAY 64 OF CHAR;  
		next: Errors;  
	END;  
	ErrorEntry* = POINTER TO RECORD 
		pos*, err*: LONGINT;  
		msg*: ARRAY 128 OF CHAR;  
	END;  
	ErrorList* = POINTER TO RECORD (* object *)
			act, found, size: LONGINT;  
			list: POINTER TO ARRAY OF ErrorEntry;  
		END;	
		(** <<  tt, fof  *)

	VAR ival*, slen*: LONGINT;  (*results of Get*)
		rval*: REAL;
		modid*: Ident;
		error*: BOOLEAN;
		errors: Errors;
		logErrors: BOOLEAN;
		f: OIO.File;

		ch: CHAR;  (*last character read*)
		id: Ident;  (*not exported*)
		strx: INTEGER;  (*index of string table*)
		errpos, errcnt: LONGINT;
		(*R*: AosIO.Reader; W*: AosIO.Writer; *) (* exported for use by OSAG *) 
		keyTab: ARRAY KTsize OF
				RECORD sym: INTEGER; id: ARRAY 14 OF CHAR END;
		str: ARRAY stringTabSize OF LONGINT;

	(** tt, fof >> *)
	(*------------------------object's procedures-----------*)	
		PROCEDURE (obj:ErrorList) Init*( size: LONGINT );  
		BEGIN 
			obj.size := size;  
			NEW( obj.list, obj.size );  
		END Init;  
		PROCEDURE (obj:ErrorList) Erase;  
		VAR i: LONGINT;  
		BEGIN 
			FOR i := 0 TO obj.size - 1 DO obj.list[i] := NIL;  END;  
			obj.act := 0;  obj.found := 0;  
		END Erase;  

		PROCEDURE (obj:ErrorList) HasMoreErrors*( ): BOOLEAN;  
		BEGIN 
			RETURN obj.act < obj.found;  
		END HasMoreErrors;  

		PROCEDURE (obj:ErrorList) GetNext*( ): ErrorEntry;  
		VAR entry: ErrorEntry;  
		BEGIN 
			entry:=NIL;
			IF obj.HasMoreErrors() THEN 
				entry := obj.list[obj.act];  
				INC( obj.act ); 
			END;  
			RETURN entry;  
		END GetNext;  


	(*------------------------end of obj. proc.-s-----------*)

	PROCEDURE Append( VAR to (** in/out *) : ARRAY OF CHAR;  this: ARRAY OF CHAR );  
	VAR i, j, l: LONGINT;  
	BEGIN 
		i := 0;  
		WHILE to[i] # 0X DO INC( i ) END;  
		l := LEN( to ) - 1;  j := 0;  
		WHILE (i < l) & (this[j] # 0X) DO to[i] := this[j];  INC( i );  INC( j ) END;  
		to[i] := 0X
	END Append;  

	PROCEDURE MakeErrorList*( l: ErrorList );  
	VAR p: Errors;  
		str: ARRAY 128 OF CHAR; 
	
	BEGIN 
		IF l = NIL THEN RETURN END;(*??? IF l = NIL THEN RETURN END;*)  
		l.Erase;  p := errors.next;  
		WHILE (l.found < l.size) & (p # NIL ) DO 
			NEW( l.list[l.found] );  COPY( p.msg, l.list[l.found].msg );  l.list[l.found].err := p.err;  
			l.list[l.found].pos := p.pos;  
			
			IF p.err = 401 THEN Append( l.list[l.found].msg, " is no longer visible" )
			ELSIF p.err = 402 THEN Append( l.list[l.found].msg, " is redefined" )
			ELSIF p.err = 403 THEN Append( l.list[l.found].msg, " is new" )
			ELSIF (p.err >= 0) (* & (errMsg # NIL) & (LEN(errMsg) > p.err) THEN
				StringPool.GetString(errMsg[p.err], str);*)
			THEN 
				Append( l.list[l.found].msg, "  " );  Append( l.list[l.found].msg, str )
			END;  
			p := p.next;  INC( l.found );  
		END;  
		errors.next := NIL 
	END MakeErrorList;  
(** << tt, fof  *)
	
	PROCEDURE CopyId*(VAR ident: Ident);
	BEGIN COPY(id, ident)
	END CopyId;
	
	PROCEDURE NotEql*(VAR ident: ARRAY OF CHAR): BOOLEAN;
	BEGIN RETURN ident # id
	END NotEql;
(*------------------attention-----------------*)
(** fof >> *)
	(*PROCEDURE Init*( Reader: AosIO.Reader;  Writer: AosIO.Writer); *)
	
	PROCEDURE InitErrorList;
	BEGIN
		error := FALSE;
		errpos := OIO.Pos(f); 
		errcnt := 0;
		strx := 0;
		NEW( errors );
	END InitErrorList;

	PROCEDURE Init*(file: OIO.File); 
	BEGIN 
		f:=file;
		InitErrorList;
		OIO.ReadChar(f, ch );
		logErrors := TRUE;
	END Init;  
	(** << fof  *)

	PROCEDURE Mark*(msg: ARRAY OF CHAR);
	VAR pos: LONGINT;
		p, q: Errors; 
	BEGIN
		IF logErrors THEN
			pos := OIO.Pos(f) - 1;
			IF (pos > errpos) & (errcnt < 25) THEN
				OIO.WriteString( "  pos "); OIO.WriteLongInt( pos, 1);
				OIO.WriteChar( " "); OIO.WriteString( msg);
				OIO.WriteLn();
				(* W.Update() ??? *)
			END ;
			errpos := pos + 3;
			IF msg[0] # "!" THEN error := TRUE; INC(errcnt) END;

			p := errors;  q := p.next;  
			WHILE (q # NIL ) & (q.pos <= pos) DO p := q;  q := q.next END;  
			IF (pos < 0) OR (p.err = -1) OR (p.pos + 10 < pos) THEN 
				NEW( q );  q.pos := pos;  q.err := 0;  COPY( msg, q.msg );  q.next := p.next;  p.next := q
			END;  
		END;
	END Mark;

	PROCEDURE Trap*( );
	BEGIN 
		InitErrorList;		(* Ensure that no previous error is in the error list *)
		Mark("pc location");
		logErrors := FALSE;
	END Trap;
	

	(*Ling: Some procedures for debuging*)
		
	PROCEDURE DumpIntHex* (n: LONGINT);
	BEGIN
		OIO.WriteLn();
		OIO.WriteHex(n, 1);
		OIO.WriteLn();
	END DumpIntHex;

	PROCEDURE DumpString* (s: ARRAY OF CHAR);
	BEGIN
		OIO.WriteString(s);
		OIO.WriteLn()
	END DumpString;
	
	(* PROCEDURE Hex*(x, w: LONGINT; r: AosIO.Writer);has cahnged*)
	PROCEDURE Hex*(x, w: LONGINT);
		VAR i, j: INTEGER;  
			buf: ARRAY 10 OF CHAR;  
	BEGIN 
		OIO.WriteString("0x");
		j :=SHORT( w);(* SK why??? *)
		FOR i := j - 1 TO 0 BY -1 DO 
			buf[i] := CHR( x MOD 10H + 48 );  
			IF buf[i] > "9" THEN buf[i] := CHR( ORD( buf[i] ) - 48 + 65 - 10 ) END;  
			x := x DIV 10H
		END;  
		buf[j] := 0X;  OIO.WriteString( buf )
	END Hex;  
	(*Ling: End of debug procedures*)

	PROCEDURE hex(ch: CHAR): LONGINT;
		VAR n: LONGINT;
	BEGIN
		IF (ch >= "0") & (ch <= "9") THEN n := ORD(ch) - 30H
		ELSIF (ch >= "A") & (ch <= "F") THEN n := ORD(ch) - 37H
		ELSE n := 2
		END ;
		RETURN n
	END hex;

	PROCEDURE String(VAR sym: INTEGER);  (*stores string in word array*)
		VAR buf: LONGINT; i, k: INTEGER; eos: CHAR;
	BEGIN 
		eos := ch; 
		OIO.ReadChar(f, ch); 
		i := strx; ival := i; 
		k := 0;	buf := 0;
		WHILE ~(ch = 0X) & (ch # eos) DO
			IF ch >= " " THEN
				IF i >= stringTabSize-WS THEN 
					Mark("string too long"); 
					i := 0; k := 0 
				END ;
				buf := ASH(ORD(ch), k*8) + buf; 
				INC(k);
				IF k = WS THEN 
					str[i] := buf; INC(i);
					k := 0; buf := 0 
				END
			END ;
			OIO.ReadChar(f, ch)
		END ;
		OIO.ReadChar(f, ch);
		IF (i = ival) & (k = 1) THEN sym := char; ival := buf
		ELSE sym := string; str[i] := buf; slen := (i - ival)*WS + k+1; INC(i); strx := i
		END
	END String;

	PROCEDURE Identifier(VAR sym: INTEGER);
		VAR i, k: INTEGER;
	BEGIN i := 0; k := 0;
		REPEAT
			IF i < IdLen-1 THEN id[i] := ch; INC(i); k := ORD(ch) + k END ;
			OIO.ReadChar(f, ch)
		UNTIL (ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "Z") & (ch < "a") OR (ch > "z");
		id[i] := 0X; k := k MOD KTsize;  (*hash function*)
		IF (keyTab[k].sym # 0) & (keyTab[k].id = id) THEN sym := keyTab[k].sym
		ELSE k := (k+3) MOD KTsize;
			IF (keyTab[k].sym # 0) & (keyTab[k].id = id) THEN sym := keyTab[k].sym
			ELSE sym := ident
			END
		END
	END Identifier;

	PROCEDURE Number(VAR sym: INTEGER);
		CONST max = 2147483647;
		VAR i, e, n, s: INTEGER; k, h: LONGINT; x: REAL;
			d: ARRAY 16 OF LONGINT;
			negE: BOOLEAN;
	BEGIN ival := 0; i := 0; n := 0; k := 0;
		REPEAT
			IF n < 16 THEN d[n] := ORD(ch)-30H; INC(n) ELSE Mark("too many digits"); n := 0 END ;
			OIO.ReadChar(f, ch)
		UNTIL (ch < "0") OR (ch > "9") & (ch < "A") OR (ch > "F");
		IF (ch = "H") OR (ch = "R") OR (ch = "X") THEN  (*hex*)
			REPEAT h := d[i];
				IF h >= 10 THEN h := h-7 END ;
				k := k*10H + h; INC(i) (*no overflow check*)
			UNTIL i = n;
			IF ch = "X" THEN sym := char;
				IF k < 100H THEN ival := k ELSE Mark("illegal value"); ival := 0 END
			ELSIF ch = "R" THEN sym := real; rval := SYSTEM.VAL(REAL, k)
			ELSE sym := int; ival := k
			END ;
			OIO.ReadChar(f, ch)
		ELSIF ch # "." THEN  (*decimal integer*)
			REPEAT
				IF d[i] < 10 THEN
					IF k <= (max-d[i]) DIV 10 THEN k := k*10 + d[i] ELSE Mark("too large"); k := 0 END
				ELSE Mark("bad integer")
				END ;
				INC(i)
			UNTIL i = n;
			sym := int; ival := k
		ELSE OIO.ReadChar(f, ch);
			IF ch = "." THEN
				REPEAT
				IF d[i] < 10 THEN
					IF k <= (max-d[i]) DIV 10 THEN k := k*10 + d[i] ELSE Mark("too large"); k := 0 END
				ELSE Mark("bad integer")
				END ;
				INC(i)
				UNTIL i = n;
				sym := int; 
				ival := k;
				OIO.SetPos(f, OIO.Pos(f)-1)
			ELSE
				 x := 0.0; e := 0;  (*flt.pt*)
				REPEAT x := x * 10.0 + d[i]; INC(i) UNTIL i = n;  (*integer part*)
				WHILE ("0" <= ch) & (ch <= "9") DO
					x := x * 10.0 + (ORD(ch) - 30H); DEC(e); OIO.ReadChar(f, ch)  (*fraction*)
				END ;
				IF (ch = "E") OR (ch = "D") THEN  (*scale factor*)
					OIO.ReadChar(f, ch); s := 0; 
					IF ch = "-" THEN negE := TRUE; OIO.ReadChar(f, ch)
					ELSE negE := FALSE;
						IF ch = "+" THEN OIO.ReadChar(f, ch) END
					END ;
					IF ("0" <= ch) & (ch <= "9") THEN
						REPEAT s := s*10 + ORD(ch)-30H; OIO.ReadChar(f, ch)
						UNTIL (ch < "0") OR (ch >"9");
						IF negE THEN e := e-s ELSE e := e+s END
					ELSE Mark("digit?")
					END
				END ;
				IF e < 0 THEN
					IF e >= -maxExp THEN x := x /OIO.Ten(-e) ELSE x := 0.0 END
				ELSIF e > 0 THEN
					IF e <= maxExp THEN x := OIO.Ten(e) * x ELSE x := 0.0; Mark("too large") END
				END ;
				sym := real; rval := x
			END
		END
	END Number;
		
	PROCEDURE comment(VAR sym: INTEGER);
	BEGIN
		REPEAT
			REPEAT OIO.ReadChar(f, ch);
				IF ch = "(" THEN OIO.ReadChar(f, ch);
					IF ch = "*" THEN comment(sym) END
				END
			UNTIL (ch = "*") OR (ch = 0X);
			REPEAT OIO.ReadChar(f, ch) UNTIL (ch # "*") OR (ch = 0X)
		UNTIL (ch = ")") OR (ch = 0X);
		IF ~(ch = 0X) THEN OIO.ReadChar(f, ch) ELSE Mark("unterminated comment"); sym := eof END
	END comment;

	PROCEDURE Get*(VAR sym: INTEGER);
	BEGIN
		REPEAT
			WHILE ~(ch = 0X) & (ch <= " ") DO OIO.ReadChar(f, ch) END;
			IF (ch = 0X) THEN sym := eof ELSE
				CASE ch OF
				  "!": OIO.ReadChar(f, ch); sym := assert
				|  '"': String(sym)
				|  "#": OIO.ReadChar(f, ch); sym := neq
				|  "&": OIO.ReadChar(f, ch); sym := and
				|  "'": String(sym)
				|  "(": OIO.ReadChar(f, ch);
						IF ch = "*" THEN sym := null; comment(sym) ELSE sym := lparen END
				|  ")": OIO.ReadChar(f, ch); sym := rparen
				|  "*": OIO.ReadChar(f, ch); sym := times
				|  "+": OIO.ReadChar(f, ch); sym := plus
				|  ",": OIO.ReadChar(f, ch); sym := comma
				|  "-": OIO.ReadChar(f, ch); sym := minus
				|  ".": OIO.ReadChar(f, ch);
						IF ch = "." THEN OIO.ReadChar(f, ch); sym := upto ELSE sym := period END
				|  "/": OIO.ReadChar(f, ch); sym := rdiv
				|  "0" .."9": Number(sym);
				|  ":": OIO.ReadChar(f, ch);
						IF ch = "=" THEN OIO.ReadChar(f, ch); sym := becomes ELSE sym := colon END 
				|  ";": OIO.ReadChar(f, ch); sym := semicolon
				|  "<": OIO.ReadChar(f, ch);
						IF ch = "=" THEN OIO.ReadChar(f, ch); sym := leq ELSE sym := lss END
				|  "=": OIO.ReadChar(f, ch); sym := eql
				|  ">": OIO.ReadChar(f, ch);
						IF ch = "=" THEN OIO.ReadChar(f, ch); sym := geq ELSE sym := gtr END
				|  "A" .. "Z": Identifier(sym)
				|  "[": OIO.ReadChar(f, ch); sym := lbrak
				|  "]": OIO.ReadChar(f, ch); sym := rbrak
				|  "^": OIO.ReadChar(f, ch); sym := arrow 
				|  "a".."z": Identifier(sym)
				|  "{": OIO.ReadChar(f, ch); sym := lbrace
				|  "|": OIO.ReadChar(f, ch); sym := bar
				|  "}": OIO.ReadChar(f, ch); sym := rbrace
				|  "~": OIO.ReadChar(f, ch); sym := not
				ELSE OIO.ReadChar(f, ch); Mark("strange character"); sym := null
				END
			END
		UNTIL sym # null
	END Get;

	PROCEDURE MoveStrings*(VAR dst: ARRAY OF LONGINT; VAR dstx: INTEGER; dstlim: INTEGER);
	(*Move string from string buffer at position pos to dst*)
		VAR i, j: INTEGER;
	BEGIN i := dstx; j := 0;
		IF strx < dstlim - dstx THEN
			WHILE j < strx DO dst[i] := str[j]; INC(i); INC(j) END ;
		END ;
		dstx := i; strx := 0  (*reset string table*)
	END MoveStrings;
	
	PROCEDURE EnterKW(sym: INTEGER; name: ARRAY OF CHAR);
		VAR j, k: INTEGER;
	BEGIN j := 0; k := 0;
		REPEAT INC(k, ORD(name[j])); INC(j)
		UNTIL name[j] = 0X;
		k := k MOD 59;  (*hash function*)
		WHILE keyTab[k].sym # 0 DO k := (k + 3) MOD KTsize END ;
		COPY(name, keyTab[k].id); keyTab[k].sym := sym
	END EnterKW;

BEGIN
	OIO.WriteString("successful end!");
	OIO.WriteLn; 
	error := TRUE;
	EnterKW(array, "ARRAY");
	EnterKW(begin, "BEGIN");
	EnterKW(by, "BY");
	EnterKW(case, "CASE");
	EnterKW(const, "CONST");
	EnterKW(div, "DIV");
	EnterKW(do, "DO");
	EnterKW(else, "ELSE");
	EnterKW(elsif, "ELSIF");
	EnterKW(end, "END");
	EnterKW(exit, "EXIT");
	EnterKW(false, "FALSE");
	EnterKW(for, "FOR");
	EnterKW(if, "IF");
	EnterKW(import, "IMPORT");
	EnterKW(in, "IN");
	EnterKW(is, "IS");
	EnterKW(loop, "LOOP");
	EnterKW(mod, "MOD");
	EnterKW(module, "MODULE");
	EnterKW(nil, "NIL");
	EnterKW(of, "OF");
	EnterKW(or, "OR");
	EnterKW(pointer, "POINTER");
	EnterKW(procedure, "PROCEDURE");
	EnterKW(record, "RECORD");
	EnterKW(repeat, "REPEAT");
	EnterKW(return, "RETURN");
	EnterKW(then, "THEN");
	EnterKW(to, "TO");
	EnterKW(true, "TRUE");
	EnterKW(type, "TYPE");
	EnterKW(until, "UNTIL");
	EnterKW(var, "VAR");
	EnterKW(while, "WHILE");
	EnterKW(with, "WITH")

END OAVS0.

