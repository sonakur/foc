MODULE BootAV0;      (*NW 13.7.98 / 18.8.99 / 20.2.07  Boot linker for Strong Arm*)
  IMPORT OIO (*AosIO:=Streams, AosFS:=Files, AosSerials:=Serials*) (* fof removed  Files, Texts, Oberon, V24 *) ;

(* V24:
  PROCEDURE Receive (portNo: LONGINT; VAR x: SYSTEM.BYTE; VAR res: LONGINT);
  PROCEDURE Send(portNo: LONGINT; x: SYSTEM.BYTE; VAR res: LONGINT);
  PROCEDURE Start(portNo, bps, data, parity, stop: LONGINT; VAR res: LONGINT); *)
  
  (* fof replaced 
    Texts.WriteInt(W, => W.Int(
    Texts.WriteLn(W => W.Ln(
    Texts.WriteString(W, =>  W.String(
    Texts.WriteHex(W, => W.Hex(
    Texts.Write(W, => W.Char(
    Texts.Append(Oberon.Log, W.buf) => W.Update()
    Texts.Writer => AosIO.Writer 
    Files.File => AosFS.File
    Files.Rider => AosFS.Reader
    Files.Old => AosFS.Old 
    Files.Set => AosFS.OpenReader
    Files.ReadLInt(R, => R.RawLInt(
    Files.ReadString(R, => R.RawString( 

    V24.Available(port  => port.Available(
    V24.Receive(port, => port.ReceiveChar(
    V24.Send(port, => port.SendChar(
    
  *) 

  CONST portNo= 1; (* fof *)
  TYPE Name = ARRAY 32 OF CHAR;
    Module = POINTER TO ModuleDesc;
	ModuleDesc = RECORD
        name: Name;
        key:  LONGINT;
        dbase, pbase: LONGINT;
        next: Module;
        entry: ARRAY 60 OF LONGINT
      END ;

  VAR 
    (*port: AosSerials.Port;*) (* fof *) 
    root: Module;
    base, org, res: LONGINT;
    listflag: BOOLEAN;
    F: OIO.BinFile;
    code: ARRAY 8192 OF LONGINT;

  PROCEDURE ThisFile(name: ARRAY OF CHAR): OIO.FileDesc;
    VAR i: INTEGER;
	is_opened:BOOLEAN;
  BEGIN i := 0;
    WHILE name[i] # 0X DO INC(i) END ;
    name[i] := "."; name[i+1] := "a"; name[i+2] := "r"; name[i+3] := "m"; name[i+4] := 0X;
	is_opened := OIO.OpenBinFile(name, F);
    RETURN OIO.ReturnFileDesc(F)
  END ThisFile;

  (*PROCEDURE SendWord(x: LONGINT);
    VAR i: INTEGER;
  BEGIN i := 4;
    REPEAT port.SendChar( CHR(x), res); x := x DIV 100H; DEC(i) UNTIL i = 0
  END SendWord;

  PROCEDURE SendCode(len, adr, entry: LONGINT);
    VAR i, del, xsum: LONGINT; ch: CHAR;
  BEGIN
    WHILE port.Available() > 0 DO port.ReceiveChar( ch, res) END ;
    SendWord(len*4); SendWord(adr); SendWord(entry);
    xsum := len*4 + adr + entry; i := 0;
    WHILE i < len DO
      SendWord(code[i]); xsum := code[i] + xsum; INC(i)
    END ;
    SendWord(-xsum); del := 50000;
    REPEAT DEC(del) UNTIL (port.Available() > 0) OR (del = 0);
    IF del > 0 THEN port.ReceiveChar( ch, res); W.Char( ch)
    ELSE W.String( " timeout")
    END
  END SendCode; 
*)
  PROCEDURE ListCode(len, adr, entry: LONGINT);
    VAR i: LONGINT;
  BEGIN OIO.WriteLn(); i := 0;
    WHILE i < len DO
      OIO.WriteHex( adr + org,1); OIO.WriteChar( 9X); OIO.WriteHex( code[i],1);
      OIO.WriteLn(); INC(adr, 4); INC(i)
    END ;
    OIO.WriteString( "Start at"); OIO.WriteHex( entry,1); OIO.WriteLn()
  END ListCode;

  PROCEDURE Fixup(fixloc, offset, base: LONGINT; VAR entry: ARRAY OF LONGINT);
    VAR instr, next, pno, vno, reg, disp: LONGINT;
  BEGIN
    WHILE fixloc # 0 DO
      instr := code[fixloc]; next := instr MOD 10000H;
      pno := instr DIV 10000H MOD 100H;
      IF instr DIV 1000000H MOD 100H = 0EBH THEN  (* case  BL *)
        instr := instr DIV 1000000H * 1000000H + (entry[pno] + offset - fixloc - 2) MOD 1000000H
      ELSIF instr DIV 1000000H = 0 THEN (*indir. variable address*) instr := entry[pno] + base
      ELSE (*indir. proc. address*) instr := entry[pno]*4 + base
      END ;
      code[fixloc] := instr; fixloc := next
    END
  END Fixup;
  
  PROCEDURE FixSelf(fixloc, base: LONGINT);
    VAR instr, next: LONGINT;
  BEGIN
    WHILE fixloc # 0 DO
      instr := code[fixloc]; next := instr MOD 10000H;
      code[fixloc] := instr DIV 10000H + base; fixloc := next
    END
  END FixSelf;
  
  PROCEDURE ThisMod(VAR modname: ARRAY OF CHAR): Module;
    VAR mod, imp: Module;
      nofimp, nofentries, codelen, fix, fixself, i: LONGINT;
      F: OIO.BinFile;
      name: Name;
      key, offset, datasize: LONGINT;
      import: ARRAY 16 OF Module;
      fixroot: ARRAY 16 OF LONGINT;
      err: BOOLEAN;
  
  BEGIN mod := root;
    WHILE (mod # NIL) & (mod.name # modname) DO mod := mod.next END ;
    IF mod = NIL THEN  (*load*)
      OIO.SetFileDesc(F, ThisFile(modname));
      IF OIO.ReturnFileDesc(F) # NIL THEN
        (*AosFS.OpenReader(R, F, 0);*)
        NEW(mod); mod.next := root; root := mod;
        OIO.ReadBinString(F, mod.name); OIO.ReadBinLInt(F, mod.key);
        OIO.ReadBinLInt(F, fixself);
        OIO.WriteString( "module "); OIO.WriteString( mod.name); OIO.WriteHex( mod.key,1);
        OIO.WriteLn(); OIO.ReadBinString(F, name); err := FALSE; i := 0;
        WHILE (name[0] # 0X) & ~err DO
          imp := ThisMod(name);
          IF imp # NIL THEN
            IF key = imp.key THEN
              import[i] := imp; fixroot[i] := fix; INC(i)
            ELSE err := TRUE;
              OIO.WriteString( name); OIO.WriteString( " wrong version");
              OIO.WriteLn(); OIO.UpdateWriter()
            END ;
          ELSE err := TRUE;
            OIO.WriteString( name); OIO.WriteString( " not found");
            OIO.WriteLn()
          END ;
          OIO.ReadBinString(F, name); OIO.UpdateWriter()
        END ;
        nofimp := i;
        IF ~err THEN
          OIO.ReadBinString(F, name);
          WHILE name[0] # 0X DO  (*commands; not yet implemented*)
            OIO.ReadBinLInt(F, offset); OIO.ReadBinString(F, name)
          END ;
          OIO.ReadBinLInt(F, nofentries); OIO.ReadBinLInt(F, mod.entry[0]); i := 0;
          WHILE i < nofentries DO INC(i); OIO.ReadBinLInt(F, mod.entry[i]) END ;
          mod.dbase := base; OIO.ReadBinLInt(F, datasize); base := base + datasize; mod.pbase := base;
          OIO.ReadBinLInt(F, codelen); base := base + codelen*4; i := 0;
          WHILE i < codelen DO OIO.ReadBinLInt(F, code[i]); INC(i) END ;
          FixSelf(fixself, mod.pbase); i := 0;
          WHILE i < nofimp DO
            offset := import[i].pbase - mod.pbase;
            Fixup(fixroot[i], (import[i].pbase - mod.pbase) DIV 4, import[i].pbase, import[i].entry); INC(i)
          END ;
          OIO.WriteString( "    loading "); OIO.WriteString( mod.name);
          OIO.WriteLongInt( codelen*4, 6);
          OIO.WriteHex( mod.dbase,1); OIO.WriteHex( mod.pbase,1);
          OIO.WriteHex( mod.entry[0]*4 + mod.pbase,1);
          IF ~listflag THEN (*SendCode(codelen, mod.pbase, mod.entry[0]*4 + mod.pbase) *)
          ELSE ListCode(codelen, mod.pbase, mod.entry[0]*4 + mod.pbase)
          END ;
          OIO.WriteLn(); OIO.UpdateWriter()
        END
      ELSE OIO.WriteString( name); OIO.WriteString( " not found");
        OIO.WriteLn(); OIO.UpdateWriter()
      END
    END ;
    RETURN mod
  END ThisMod;

  PROCEDURE Link*(i: LONGINT; file: ARRAY OF CHAR);
  VAR main: Module;   
  BEGIN
    listflag := i < 0; base := ABS(i);
    main := ThisMod(file); root := NIL; 
  END Link;
  

  PROCEDURE Init*();(* check! (writer is Out)*)
	BEGIN
	END Init;
  (*PROCEDURE Init*(VAR w: TextRider.Writer);
  BEGIN
    W := w;
  END Init;*)

  
  (* fof moved to gui dependent modules OSACompiler and OSACompiler0  
  PROCEDURE Link*;
    VAR main: Module; S: Texts.Scanner;
  BEGIN Texts.OpenScanner(S, Oberon.Par.text, Oberon.Par.pos); Texts.Scan(S);
    IF S.class = Texts.Int THEN
      listflag := S.i < 0; base := ABS(S.i); Texts.Scan(S);
      IF S.class = Texts.Name THEN main := ThisMod(S.s); root := NIL END
    END
  END Link;
  *) 
  
  (*PROCEDURE Start*;
  BEGIN 
    port := AosSerials.GetPort(portNo);
    IF port # NIL THEN
      port.Open(19200, 8, AosSerials.ParNo, AosSerials.Stop1, res)
    END;
  END Start;*)

BEGIN (* fof removed * Texts.OpenWriter(W); *) 
	root := NIL; (*Start*)
END BootAV0.

