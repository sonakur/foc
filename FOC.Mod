MODULE FOC;   (**  AUTHOR "fof"; PURPOSE "";  **)

IMPORT (*AosIO:=Streams, AosTexts:=Texts, Commands, AosTextUtilities:=TextUtilities, 
  UTF8Strings, AosFS:=Files, AosOut:=KernelLog,*) 
	OIO, BootSA0 := BootAV0, BootSAX0 := BootAVX0, OSAS0 := OAVS0, OSAP0 := OAVP0, Args; 

VAR 
  (*log: AosIO.Writer; log is output *) (* logger to AosOut *)
	is_main*: BOOLEAN;
	name*: ARRAY 255 OF CHAR;
TYPE 
  Buffer = POINTER TO ARRAY OF CHAR;  

 PROCEDURE Showhelp ;
   BEGIN

   OIO.WriteString ('usage: '); OIO.WriteLn;
   OIO.WriteString ('aoc options modulename'); OIO.WriteLn;
   OIO.WriteString ('aoc modulesuname options'); OIO.WriteLn;
   OIO.WriteLn;
   END Showhelp;

  PROCEDURE ParseOptions * (); (* it works :) *)
        VAR     i: LONGINT;
                modid, arg: ARRAY 255 OF CHAR;
        BEGIN
                is_main:= FALSE;
                IF(Args.ArgNum() = 0) THEN
                        OIO.WriteString("Enter File Name");
			OIO.WriteLn();
			(*HALT(0);*)
                ELSE
                        i:=0;
                        Args.GetArg(1, arg);
                        WHILE ((arg[i] # '.') & (arg[i] # 0X)) DO (* read filename*)
                                modid[i]:= arg[i];
                                INC(i);
                        END;
                        modid[i]:= 0X;
			COPY(modid, name);
                        FOR i:= 1 TO Args.ArgNum() DO (* read other args *)
                                Args.GetArg(i, arg);
                                IF (arg = "-M") THEN
                                        is_main:= TRUE;
                                END
                        END
                END


        END ParseOptions;


 (* PROCEDURE ParseOptions( VAR options: ARRAY OF CHAR;  breakpc: LONGINT;  
                          (*log: AosIO.Writer;*)  VAR newSF: BOOLEAN );  
  VAR i: LONGINT;  ch: CHAR; 


  BEGIN 
    (*defaults*)
    newSF := FALSE;  i := 0;  
    REPEAT 
      ch := options[i];  INC( i );  
      CASE ch OF 
      "s":       newSF := TRUE;  
      ELSE 
        IF ch > " " THEN OIO.WriteString( "Option not found:" );  
		OIO.WriteChar( ch );  OIO.WriteLn;  END;  
      END;  
    UNTIL ch = 0X;  
  END ParseOptions;  *)

  (** Compile Module *)
  PROCEDURE Module*( r: OIO.File; (*log: AosIO.Writer;*)VAR options: ARRAY OF CHAR;  (*breakpc: LONGINT;*)  VAR error,trapped: BOOLEAN );  
  VAR newSF: BOOLEAN;
  BEGIN  (*{EXCLUSIVE} *) 
    trapped := TRUE;
    ParseOptions(*( options, breakpc,  newSF)*);  OSAP0.Init( r, newSF(*, breakpc*) );  
    OSAP0.Module;  error := OSAS0.error;  
    trapped := FALSE; 
 (* FINALLY 
    IF trapped THEN 
      error := TRUE; AosOut.String("Compiler trapped"); AosOut.Ln; 
    END;*) 
  END Module;  

 (* PROCEDURE GetOptions( S: AosIO.Reader;  VAR opts: ARRAY OF CHAR );  
  VAR i: LONGINT;  ch: CHAR;  
  BEGIN 
    i := 0;  
    WHILE opts[i] # 0X DO INC( i ) END;  
    S.SkipWhitespace;  ch := S.Peek();  
    WHILE (ch = "\") DO 
      S.Char( ch );   (* skip \ *)
      S.Char( ch );  
      WHILE ch > " " DO opts[i] := ch;  INC( i );  S.Char( ch ) END;  
      opts[i] := " ";  INC( i );  S.SkipWhitespace;  ch := S.Peek()
    END;  
    opts[i] := 0X
  END GetOptions;  
*)
  
(** Compile code contained in t, beginning at position pos *)
  PROCEDURE ExpandBuf( VAR oldBuf: Buffer;  newSize: LONGINT );  
  VAR newBuf: Buffer;  i: LONGINT;  
  BEGIN 
    IF LEN( oldBuf^ ) >= newSize THEN RETURN END;  
    NEW( newBuf, newSize );  
    FOR i := 0 TO LEN( oldBuf^ ) - 1 DO newBuf[i] := oldBuf[i];  END;  
    oldBuf := newBuf;  
  END ExpandBuf;  

  (*PROCEDURE TextReader( t: AosTexts.Text;  pos: LONGINT ): AosIO.Reader;  
  VAR buffer: Buffer;  len, i, j, ch: LONGINT;  r: AosTexts.TextReader;  
  VAR s: AosIO.StringReader;  bytesPerChar: LONGINT;  
  BEGIN 
    t.AcquireRead;  len := t.GetLength() + 10;  bytesPerChar := 2;  
    NEW( buffer, len * bytesPerChar );   (* UTF8 encoded characters use up to 5 bytes *)
    NEW( r, t );  r.SetPosition( pos );  j := 0;  
    FOR i := 0 TO len - 1 DO 
      r.ReadCh( ch );  
      WHILE ~UTF8Strings.EncodeChar( ch, buffer^, j ) DO 
        (* buffer too small *)
        INC( bytesPerChar );  ExpandBuf( buffer, bytesPerChar * len );  
      END;  
    END;  
    t.ReleaseRead;  NEW( s, len );  s.Set( buffer^ );  RETURN s;  
  END TextReader;  
*)
  (*PROCEDURE CompileText*( t: AosTexts.Text;  pos, pc: LONGINT;  VAR opt: ARRAY OF CHAR;  
                          log: AosIO.Writer;  VAR error,trap: BOOLEAN );  
  BEGIN 
    IF t = NIL THEN log.String( "No text available" );  log.Ln;  log.Update;  error := TRUE;  RETURN END;  
    Module( TextReader( t, pos ), log, opt, pc, error ,trap);  
  END CompileText;  
*)
(** Compile file *)

 (* PROCEDURE CompileFile*(VAR name, opt: ARRAY OF CHAR;  pc: LONGINT;  log: AosIO.Writer;  
                            VAR error,trap: BOOLEAN );  
  VAR atu: AosTexts.Text;  format, res: LONGINT;  
  BEGIN 
    NEW( atu );  AosTextUtilities.LoadAuto( atu, name, format, res );  
    IF res # 0 THEN 
      log.String( name );  log.String( " not found" );  log.Ln;  log.Update;  error := TRUE;  
      RETURN 
    END;  
    log.String( name );  Module( TextReader( atu, 0 ), log, opt, pc, error ,trap);  
  END CompileFile;  
  *) 
(** Compile ascii file *)

  PROCEDURE CompileAsciiFile*( VAR name, opt: ARRAY OF CHAR;  
                                (*log: AosIO.Writer;*) VAR error,trap: BOOLEAN );  
  VAR f: OIO.File;  (*r: AosFS.Reader;*)
	b:BOOLEAN;  
  BEGIN 
    b := OIO.OpenFile( name, f );  
    IF b = FALSE THEN 
      OIO.WriteString( name );  OIO.WriteString( " not found" );  
	OIO.WriteLn();  OIO.UpdateWriter;  error := TRUE;  
      RETURN 
    END;  
    OIO.WriteString( name ); OIO.SetPos(f,0); (*AosFS.OpenReader( r, f, 0 );*)  
    Module( f, opt,  error,trap );  
  END CompileAsciiFile;  

  PROCEDURE Compile*(*( context: Commands.Context )*);  
  VAR 
    globalOpt, localOpt: ARRAY 32 OF CHAR;  
    count: LONGINT;  
    name: ARRAY 64 OF CHAR;  
    error,trap: BOOLEAN;  
  BEGIN 
    error := FALSE;  globalOpt := "";
   name := ''; localOpt := ''; (*nc*)
  (*GetOptions( context.arg, globalOpt );*)  count := 0;  
    WHILE ~error (*& (context.arg.res = 0)*) DO 
      (*context.arg.GetString( name );*)  
      IF name # "" THEN 
        INC( count );  COPY( globalOpt, localOpt ); 
	(* GetOptions( context.arg, localOpt ); *)  
        CompileAsciiFile( name, localOpt, (*MAX( LONGINT ), log,*) error,trap ); 
      END;  
 	error := TRUE; (* !!!remove later, or edit!!! *)
	(*OIO.WriteString("error=true");OIO.WriteLn();*)
   END
  END Compile;  


(*PROCEDURE Compile*;
  VAR
    globalOpt, localOpt: ARRAY 32 OF CHAR;
    count: LONGINT;
    name: ARRAY 64 OF CHAR;
    error,trap: BOOLEAN;
    (*r : TextRider.Reader;*)
  BEGIN
    error := FALSE;  globalOpt := "";  name := ''; localOpt := '';
    (*r := TextRider.ConnectReader(ProgramArgs.args);
    IF r = NIL THEN Out.String ("Error connecting reader to arguments"); Out.Ln; HALT(0); END;
    IF ProgramArgs.args.ArgNumber() < 1 THEN Out.String ("Error processing: wrong number of arguments"); Out.Ln; Showhelp; HALT(0) END;*)
    IF args.argscount() < 1 THEN OIO.WriteString ("Error processing: wrong number of arguments"); 
	OIO.WriteLn; Showhelp; HALT(0) END;
    args.arg(1, name);
    IF ((name[0] = '-') OR (name[0]='/') OR (name[0]='\')) THEN
    COPY(name, globalOpt); name := '';
    args.arg(2, name);
    ELSE
    args.arg(2, globalOpt);
    END;
(*r.ReadLn; IF r.res # TextRider.done THEN Out.String ('error reading argument'); Out.Ln END;*)
    (*r.ReadString(name); Out.String ('read name='); Out.String (name); Out.Ln;
    IF ((name[0] = '-') OR (name[0]='/') OR (name[0]='\')) THEN 
    COPY(name, globalOpt);
    r.ReadString(name);
    IF r.res # TextRider.done THEN Out.String ('error: no module name provided'); Out.Ln; HALT(0) END;
    ELSE
    r.ReadString(globalOpt);
    END;*)
    (*GetOptions(s, globalOpt );  count := 0;  
    WHILE ~error & (s.res = TextRider.done) DO 
      s.ReadString( name );  
      IF name # "" THEN 
        INC( count );  COPY( globalOpt, localOpt );  GetOptions(s, localOpt );  
        *)
        COPY(globalOpt, localOpt);
        (*Out.String ("localopt="); Out.String (localOpt); Out.Ln; Out.String ("globalopt="); Out.String (globalOpt); Out.Ln;
        Out.String ('name='); Out.String( name); Out.Ln;*)
        CompileAsciiFile( name, localOpt, MAX( LONGINT ),  error,trap );
      (*END;  
    END*)
  END Compile;  
*)	

  (*PROCEDURE Link*( context: Commands.Context );  
  VAR 
    name: ARRAY 64 OF CHAR; base: LONGINT;  
  BEGIN 

    context.arg.GetInteger( base, TRUE );  
    (* 
    log.String( "base=" );  log.Hex( base, 10 );  log.Ln;  
    *)
    BootSA0.Init( log );  context.arg.GetString( name );  
    IF context.arg.res = 0 THEN BootSA0.Link( base, name );  END;  
    
    (* for multiple files: 
    MinosBootLinker.StartLink( base, log );  error := FALSE;  
    WHILE (S.res = 0) & ~error DO 
      S.SkipWhitespace;  S.String( name );  
      IF name # "" THEN MinosBootLinker.LinkModule( name );  ELSE error := TRUE;  END;  
    END;  
    MinosBootLinker.EndLink();  RETURN NIL;  
    *)
  END Link;  
  
  PROCEDURE LinkX( linker: BootSAX0.Linker; addHeaderFile: BOOLEAN; context: Commands.Context );  
  VAR   fileOut,fileIn, fileHeader: ARRAY 256 OF CHAR; base: LONGINT;
    success: BOOLEAN; intRes: LONGINT;
  BEGIN
    success := TRUE;
    
    IF addHeaderFile THEN
      context.arg.GetString( fileHeader );
    ELSE
      fileHeader := "";
    END;
    context.arg.GetInteger( base, TRUE );  
    context.arg.GetString( fileOut );
    AosFS.Delete(fileOut, intRes);        (* Try to delete an existing output file *)
    linker.Begin (base, fileOut, fileHeader, success);
    WHILE (context.arg.res = AosIO.Ok) & success DO
      context.arg.GetString( fileIn );
      IF fileIn[0] # 0X THEN linker.Link (fileIn, success) END;
    END;
    IF success THEN linker.End; END;
  END LinkX; 
  
  PROCEDURE LinkXFile*( context: Commands.Context );
  VAR linker: BootSAX0.Linker;
  BEGIN NEW (linker, log, FALSE, TRUE);
    LinkX(linker,FALSE, context)
  END LinkXFile;
  
  PROCEDURE LinkXLoader*( context: Commands.Context );
  VAR linker: BootSAX0.Linker;
  BEGIN NEW (linker, log, TRUE, FALSE);
    LinkX(linker, FALSE, context)
  END LinkXLoader;
  
  PROCEDURE LinkLoad*(base: LONGINT; w: AosIO.Writer; VAR fileOut, fileIn: ARRAY OF CHAR; VAR success: BOOLEAN);
    VAR
      linker: BootSAX0.Linker;
  BEGIN
    NEW (linker, w, TRUE, FALSE);
    linker.Begin (base, fileOut, "", success);
    IF success THEN
      linker.Link (fileIn, success);
      IF success THEN
        linker.End
      END
    END
  END LinkLoad;
  
  PROCEDURE LinkXImage*( context:Commands.Context );
  VAR linker: BootSAX0.Linker;
  BEGIN NEW (linker, log, TRUE, TRUE);
    LinkX(linker, FALSE, context)
  END LinkXImage;
*)
 (* PROCEDURE SetLog*( Log: AosIO.Writer );  
  BEGIN 
    IF Log = NIL THEN NEW( log, AosOut.Send, 512 ) ELSE log := Log END;  
  END SetLog;  
*)
BEGIN 
	
	OIO.WriteString("Foc !!!!");
	COPY("", name);
	ParseOptions();
	Compile();
	OIO.WriteString("Foc !!!!");
	OIO.WriteLn();
 (* SetLog( NIL );i*)  
END FOC.
