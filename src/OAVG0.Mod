MODULE OAVG0; (* NW 1.7.97 / 2.5.99 / 24.11.99 / 21.5.2007*)
  (*IMPORT SYSTEM, AosFS:=Files, OSAS := OAVS0, OSAB := OAVB0,Strings,KernelLog;*)
  IMPORT SYSTEM, OIO, OSAS := OAVS0, OSAB := OAVB0, Functions, Gen;
  (*Code generator for Oberon compiler for Strong-ARM processor.
     Procedural interface to Parser OSAP; result in array "code".
     Procedure Close writes code-files*)

  CONST WordSize* = 2;
    maxCode = 8192;
    FP* = 28; GB = 26; (*GB represents the base address register X for global variables; FB represents Y register used to address local variablesr*) 
    WRegLim* = 10;  maximp = 24; ITlen=256; FPUkey = SHORT(9D3F06BDH); MAUkey = 321AA771H;
    RegX = 10; CC = 11H; (*internal item mode*)
    posinfo = TRUE;
  (*AVR opcodes*)
    CPC = 01H; SBC = 02H; ADD = 03H; CPSE=10H; CP = 11H; ADC=13H; AND=20H; EOR=21H; MOV=23H; SUB=12H; MUL=93H;
    CPI = 3H; SBCI=4H; SUBI=5H; ORI=6H; ANDI=7H; CBR=7H; LDI=0EH; 
    ADIW = 96H; SBIW=97H;
    RCALL=0DH; RJMP = 0CH; 
    CBI = 98H; SBIC=99H; SBI=9AH; SBIS=9BH;
    LSL=03H; ROL=13H; TST=20H; CLR=21H;
    LD = 800H; STZ=810H; STY=818H; LDZI = 901H; STXI = 91DH;
    INCMD = 0B0H; 
    LDD = 2000H; 
    BRLO=0F00H; BREQ = 0F01H; BRNE = 0F11H; BRLT = 0F04H;
    MOVW = 01H; 
    LDS = 900H;
    CALL = 927H; JMP=926;

	(* registers *)
	
	rax = 0; 
	rbx = 1; 
	rcx = 2; 
	rdx = 3; 
	rsi = 4; 
	rdi = 5; 
	rbp = 6; 
	rsp = 7;
	rip = 8;
	r8 = 9;
	r9 = 10;
	r10 = 11;
	r11 = 12;
	r12 = 13;
	r13 = 14;
	r14 = 15;
	r15 = 16;
	(*rflags?*)    

	(* commands *)
	exit = 0;
    
  (*constants used in address operations*)
    C16 = 10000H;
    
    TYPE Item* = RECORD
      mode*: INTEGER;
      type*: OSAB.Type;
      a, b, r: LONGINT;
      rdo*: BOOLEAN ; (*read only*)
	label*:OSAS.Ident
    END ;

  (* Item forms and meaning of fields:
    mode    r      a       b
    --------------------------------
    Const   -     value (proc adr)    (immediate adr)
    Var     base   off     -               (direct adr)
    Par     base   off0     off1         (indirect adr)
    Reg    regno
    RegI   regno   off     -
    RegX   reg0   reg1   scale
    CC       cond   Fchain  Tchain  *)
  
    
  VAR 
    pc, curlev: INTEGER;
    entry, firstfixloc: INTEGER;
    codeAVR: ARRAY maxCode OF LONGINT;
    cond: ARRAY 6 OF INTEGER; (*condition codes for relations*)
    fixlistConst: LONGINT; (*fixup list header of constants*)
    fixlist: ARRAY maximp OF LONGINT; (*fixup list*)
    tdfixlist: ARRAY maximp OF LONGINT; (*TYPE descriptor fix list*)
    fixlistFP, fixlistMU: LONGINT; (*fixup lists for modules FAP AND MAU*)
    
    TSC: ARRAY ITlen OF (*table of String Constants*)
      RECORD index, adr: LONGINT END;
    TXR, TXTD, TXI : ARRAY ITlen OF (*Table of fixup elements, TXTD is the table of type descriptor elements, TXI: the table of interrupt handler*)
      RECORD ref, adr: LONGINT END; (*reference denotes the level of the module, adr denotes the offset of the instruct that needs to fix up its address part*)
    scx, xrefx, xreft, xrefi: INTEGER; (*index of TSC, TXR, TXTD*)
    RH, RL: LONGINT;
    assertLabel: LONGINT; (*the label that links all of the assert statement*)
    regs : SET;
    
    codelen, constSize, ifixup: LONGINT; (*ifixup: the header of interrupt fixup list*)


	f: OIO.File;  
	is_main*: BOOLEAN;  

  (*register management*)

(*get n consecutive registers that are greater than or equla to low. r is the smallest number of these registers*)
  PROCEDURE GetReg(VAR r: LONGINT; n: LONGINT; low: LONGINT); 
    VAR u, v: LONGINT;
  BEGIN
    u := RH;
    WHILE u IN regs DO DEC (u) END;
    v := u;
    IF u-n+1 >= low THEN
      WHILE ( ~( v  IN regs) )& (v > u-n+1) DO DEC(v) END;
      IF v = u-n+1 THEN 
        r := v;
        WHILE n > 0 DO
          INCL (regs, v);
          v := v+1;
          n := n-1
        END
      END
    ELSE
      OSAS.Mark("too few registers")
    END        
  END GetReg;
  
  PROCEDURE RelReg (r, n: LONGINT); (*Release n consecutive registers started from the smalles one r*)
  BEGIN
    WHILE n > 0 DO
      IF (r >= RL) & (r <= RH) THEN EXCL(regs, r) END;
      r := r+1;
      n := n-1
    END
  END RelReg;
  
  PROCEDURE GetReg1(VAR r: LONGINT; x: Item); (*release the registers used by items x and allocate registers for item x*)
  BEGIN
    RelReg(x.r, x.type.size);
    GetReg(r, x.type.size, 4);
  END GetReg1;
  
  PROCEDURE GetReg2(VAR r: LONGINT; x, y: Item); (*release the registers used by x, and y and allocate registers for x*)
  BEGIN
    RelReg(x.r, x.type.size);
    RelReg(y.r, y.type.size);
    GetReg(r, x.type.size, 4)
  END GetReg2;
  
  
  (*AVR instruction assemblers according to formats*)
  PROCEDURE^ PutAVR0 (op, dst, src: LONGINT);
  PROCEDURE^ PutAVR14(op, dst, src: LONGINT); (*MOVW, MULS*)

  PROCEDURE PutMul(r, r1, r2: LONGINT); (*r+1: r <_ r1+1: r1 * r2+1: r2; r should not equate to r1*)
  BEGIN
    PutAVR0(CPC+92H, r1, r2);
    PutAVR14(MOVW, r, 0);
    PutAVR0(CPC+92H, r1+1, r2);
    PutAVR0(CPC+2H, r+1, 0);
    PutAVR0(CPC+92H, r1, r2+1);
    PutAVR0(CPC+2H, r+1, 0);
  END PutMul;
  
  PROCEDURE PosInfo();  (*BohdanT*)
  VAR
   s:ARRAY 255 OF CHAR;
   pos:LONGINT;
  BEGIN
    IF posinfo THEN  
    Functions.LIntToStr(pc,s);
    pos := OIO.Pos(OSAS.f) - 1;
    (*KernelLog.String("DBG:");KernelLog.Int(pos,5);KernelLog.String("-");
	KernelLog.Int(pc,5);KernelLog.Ln; ???*)

    END;
  END PosInfo;
  
  PROCEDURE PutCode(code: LONGINT);
  BEGIN
    PosInfo();
    codeAVR[pc] := code; INC(pc)
  END PutCode;
  
  PROCEDURE PutAVR0 (op, dst, src: LONGINT);
  BEGIN
    PosInfo();
    codeAVR[pc] := (op DIV 10H)*1000H+(op MOD 10H)*400H+(src DIV 10H) * 200H + (dst DIV 10H) * 100H + (dst MOD 10H)*10H + (src MOD 10H); INC(pc)
  END PutAVR0;
  
  PROCEDURE PutAVR1 (op, dst, imm: LONGINT); (*operations with one register and one immediate*)
  BEGIN
    PosInfo();  
    dst := dst -16;
    codeAVR[pc] := op * 1000H + (imm DIV 10H)*100H + dst*10H+(imm MOD 10H); INC(pc)
    
  END PutAVR1; 
  
  PROCEDURE PutAVR2 (op, dst, imm: LONGINT); (*Add or Sub an immediate to a register pair - ADIW, SBIW*)
  BEGIN
    PosInfo();  
    dst := (dst - 24) DIV 2;
    codeAVR[pc] := op*100H+(imm DIV 10H)*40H+dst*10H+(imm MOD 10H); INC(pc)
  END PutAVR2;
  
  PROCEDURE PutAVR3(op, k: LONGINT); (*RCALL, RJMP*)
  BEGIN
    PosInfo();  
    codeAVR[pc] := op *1000H+ k MOD 1000H; INC(pc)
  END PutAVR3;
  
  PROCEDURE PutAVR5(op, a, b: LONGINT) (*CBI, SBI, SBIC, SBIS*);
  BEGIN
    PosInfo();  
    codeAVR[pc] := op *100H+a*8H+b; INC(pc)
  END PutAVR5;
  
  PROCEDURE PutAVR6 (op, dst: LONGINT); (*LSL, ROL, TST, CLR*)
  BEGIN
    PosInfo();  
    codeAVR[pc] := (op DIV 10H)*1000H+(op MOD 10H)*400H+(dst DIV 10H) * 200H + (dst DIV 10H) * 100H + (dst MOD 10H)*10H + (dst MOD 10H); INC(pc)
  END PutAVR6;
  
  PROCEDURE PutAVR7(op, dst: LONGINT); (*Load/Store one byte, COM, NEG, SWAP, INC, ASR, LSR, ROR, DEC*)
    VAR temp1, temp2, temp3: LONGINT;
  BEGIN
    PosInfo();  
    temp1 := op DIV 100H;
    temp2 := (op DIV 10H) MOD 10H;
    temp3 := op MOD 10H;
    codeAVR[pc] := temp1*1000H+temp2*200H+(dst DIV 10H)*100H+(dst MOD 10H)*10H+temp3;
    INC(pc);
  END PutAVR7;
  
  PROCEDURE PutAVR8(op, dst, a: LONGINT); (*IN, OUT, 0<=a<=63*)
  BEGIN
    PosInfo();  
    codeAVR[pc] := (op DIV 10H)*1000H+(op MOD 10H)*800H+(a DIV 10H)*200H+(dst DIV 10H)*100H+(dst MOD 10H)*10H+(a MOD 10H); INC(pc)
  END PutAVR8;
  
  PROCEDURE PutAVR9(op, dst, q: LONGINT); (*LDD Rd, Z/Y+q, STD Y/Z+Q, Rr*)
    VAR 
      temp1, temp2, temp3, temp4: LONGINT;
      q1, q2, q3: LONGINT;
      d1, d2: LONGINT;
  BEGIN
    temp1 := op DIV 1000H;
    temp2 := (op DIV 100H) MOD 10H;
    temp3 := (op DIV 10H) MOD 10H;
    temp4 := op MOD 10H;
    q1 := q DIV 20H;
    q2 := (q DIV 8H) MOD 4H;
    q3 := q MOD 8H;
    d1 := dst DIV 10H;
    d2 := dst MOD 10H;
    PosInfo();  
    codeAVR[pc] := temp1*4000H+q1*2000H+temp2*1000H+q2*400H+temp3*200H+d1*100H+d2*10H+temp4*8H+q3;
    INC(pc)    
  END PutAVR9;
  
  PROCEDURE PutAVR10(op, dst, src: LONGINT); (*MULSU, FMUL, FMULS, FMULSU*)
    VAR temp1, temp2: LONGINT;
  BEGIN
    temp1 := (op DIV 10H) MOD 10H;
    temp2 := op MOD 10H;
    dst := dst -16;
    src := src -16;
    PosInfo();  
    codeAVR[pc] := (op DIV 100H)*100H+temp1*80H+dst*10H+temp2*8H+src;
    INC(pc)
  END PutAVR10;
  
  PROCEDURE PutAVR11(op, k: LONGINT); (* BRBS, BRBC, BRCS, BRL0, BREQ, BRMI, BRVS, BRLT, BRHS, BRTS, BRIE, BRCC, BRSH, BRNE, BRPL, BRVC, BRGE, BRHC, BRTC, BRID*)
    VAR 
      temp1, temp2: LONGINT;
      k1, k2, k3: LONGINT;
  BEGIN
    temp1 := (op DIV 10H) MOD 10H;
    temp2 := op MOD 10H;
    k1 := (k DIV 2H) DIV 10H MOD 4H;
    k2 := (k DIV 2H) MOD 10H;
    k3 := k MOD 2H;
    PosInfo();  
    codeAVR[pc] := (op DIV 100H)*1000H+temp1*400H+k1*100H+k2*10H+k3*8H+temp2;
    INC(pc);
  END PutAVR11;
  
  PROCEDURE PutAVR12(op, s: LONGINT); (*BSET, BCLR*)
  BEGIN
  END PutAVR12;
  
  PROCEDURE PutAVR14(op, dst, src: LONGINT); (*MOVW, MULS*)
  BEGIN
    IF (op = 01H) THEN dst := dst DIV 2; src := src DIV 2 END;
    IF (op = 02H) THEN dst := dst - 16; src := src - 16 END;
    PosInfo();  
    codeAVR[pc] := op*100H+dst*10H+src;
    INC(pc)
  END PutAVR14;
  
  PROCEDURE PutAVR17(op, dst, imm: LONGINT); (*LDS, STS, 2 bytes instruction*) 
    VAR 
      temp1, temp2: LONGINT;
  BEGIN
    temp1 := (op DIV 10H) MOD 10H;
    temp2 := op MOD 10H;
    PosInfo();  
    codeAVR[pc] := (op DIV 100H)*1000H+temp1*200H+(dst DIV 10H)*100H+(dst MOD 10H)*10H+temp2;
    INC(pc);
    PosInfo();  
    codeAVR[pc] := imm;
    INC(pc)
  END PutAVR17;
  
  PROCEDURE PutAVR18(op, imm: LONGINT); (*CALL k; JMP k*)
    VAR
      temp1, temp2, k1, k2, k3: LONGINT;
  BEGIN
      temp1 := (op DIV 10H) MOD 10H;
      temp2 := op MOD 10H;
      k1 := (imm DIV 1000000H) MOD 2H;
      k2 := (imm DIV 100000H) MOD 10H;
      k3 := (imm DIV 10000H) MOD 2H;
      PosInfo();  
      codeAVR[pc] := (op DIV 100H)*1000H + (temp1*2H+k1)*100H+k2*10H+temp2*2H+k3;
      INC(pc);
      PosInfo();  
      codeAVR[pc] := imm MOD 10000H;
      INC(pc)
  END PutAVR18;
  
  
  PROCEDURE SetCC(VAR x: Item; n: LONGINT);
  BEGIN 
    x.mode := CC; x.a := 0; x.b := 0; x.r := n
  END SetCC;

  PROCEDURE negated(cond: LONGINT): LONGINT;
    VAR c: LONGINT;
  BEGIN
    IF (cond = 0F01H) OR (cond = 0F04H) OR (cond = 0F60H) THEN (*BREQ, BRLT, SBRC*)
      c := cond + 10H
    ELSIF (cond = 0F11H) OR (cond = 0F14H) OR (cond = 0F70H) THEN (*BRNE, BRGE, SBRS*)
      c := cond - 10H
    ELSIF (cond = 9BH) THEN
      c := 99H
    ELSIF (cond = 99H) THEN
      c := 9BH
    END;
    RETURN c
  END negated;
  
  (*handling of forward reference, fixups of branch addresses and constant tables*)

  PROCEDURE merged(L0, L1: LONGINT): LONGINT;
    VAR L2, L3: LONGINT;
  BEGIN 
    IF L0 # 0 THEN
      L3 := L0;
      REPEAT
        L2 := L3; L3 := codeAVR[L2] MOD 10000H
      UNTIL L3 = 0;
      PosInfo();
      codeAVR[L2] := codeAVR[L2] - L3 + L1;
      L1 := L0;
    END;
    RETURN L1
  END merged;


  PROCEDURE^ fix12WithCode(at, code: LONGINT);
  PROCEDURE^ fix7WithCode(at, code: LONGINT);
  PROCEDURE^ fix12With(at, code,dst: LONGINT);
  PROCEDURE^ fix7With(at, code,dst: LONGINT);

  
  PROCEDURE FixLink*(L0: LONGINT);
    VAR L1, code: LONGINT;
  BEGIN
    WHILE L0 # 0 DO
      L1 := codeAVR[L0] MOD 10000H;
      code := codeAVR[L0] DIV 10000H MOD 1000H;
      IF code = RJMP THEN
        fix12WithCode(L0, code)
      ELSE
        fix7WithCode(L0, code)
      END;
      L0 := L1
    END
  END FixLink;

  PROCEDURE FixLinkWith(L0, dst: LONGINT);
    VAR L1, code: LONGINT;
  BEGIN  
    WHILE L0 # 0 DO
      L1 := codeAVR[L0] MOD 10000H;
      code := codeAVR[L0] DIV 10000H MOD 1000H;
      IF code = RJMP THEN
        fix12With(L0, code, dst)
      ELSE
        fix7With(L0, code, dst)
      END;
      L0 := L1
    END
  END FixLinkWith;
  
  
  PROCEDURE fix12WithCode(at, code: LONGINT);
  VAR
    k: LONGINT;
  BEGIN
    k := pc-at-1;
    IF (k >= -2048) & (k<2048) THEN
      codeAVR[at] := code *1000H+ k  MOD 1000H
    ELSE
      OSAS.Mark("The conditional statement is too long!");
    END
  END fix12WithCode;
  
  PROCEDURE fix12With(at, code, dst: LONGINT);
  VAR
    k: LONGINT;
  BEGIN
    k := dst-at-1;
    IF (k >= -2048) & (k<2048) THEN
    
      codeAVR[at] := code *1000H+ k  MOD 1000H
    ELSE
      OSAS.Mark("The conditional statement is too long!");
    END
  END fix12With;
  
  PROCEDURE fix7With(at, code,dst: LONGINT);
  VAR
    temp1, temp2, k, k1, k2, k3: LONGINT;
  BEGIN
    k := dst-at-1;
    (*check if the offset is out of the scope [-64, 63]*)
    IF (k >= -64) & (k <= 63) THEN
      k := k MOD 128;
      temp1 := (code DIV 10H) MOD 10H;
      temp2 := code MOD 10H;
      k1 := (k DIV 2H) DIV 10H;
      k2 := (k DIV 2H) MOD 10H;
      k3 := k MOD 2H;
      codeAVR[at] := (code DIV 100H)*1000H+temp1*400H+k1*100H+k2*10H+k3*8H+temp2;
    ELSE
      OSAS.Mark("The conditional statement is too big!");
    END    
  END fix7With;
  
  PROCEDURE fix7WithCode(at, code: LONGINT);
  VAR
    temp1, temp2, k, k1, k2, k3: LONGINT;
  BEGIN
    k := pc-at-1;
    (*check if the offset is out of the scope [-64, 63]*)
    IF (k >= -64) & (k <= 63) THEN
      k := k MOD 128;
      temp1 := (code DIV 10H) MOD 10H;
      temp2 := code MOD 10H;
      k1 := (k DIV 2H) DIV 10H;
      k2 := (k DIV 2H) MOD 10H;
      k3 := k MOD 2H;
      codeAVR[at] := (code DIV 100H)*1000H+temp1*400H+k1*100H+k2*10H+k3*8H+temp2;
    ELSE
      OSAS.Mark("The conditional statement is too big!");
    END    
  END fix7WithCode;
  
  
  PROCEDURE enterStr(k: LONGINT);  (*enter string index in TSC*)
  BEGIN
    IF scx < ITlen THEN
      TSC[scx].index := k; TSC[scx].adr := pc; INC(scx);
      IF firstfixloc = 0 THEN firstfixloc := pc END
    ELSE
      OSAS.Mark("too many strings"); scx := 0
    END
  END enterStr;


  PROCEDURE FixupConstants; (*fixup constants and build fixup list for global and external elements*)
    VAR i, k: LONGINT;
  BEGIN 
    (*Moving string constants to the end of the code*)
    k := pc;
    OSAS.MoveStrings(codeAVR, pc, maxCode);
    constSize := pc-k;
    codelen := pc;
    (*build fixupConst list from TSC*)
    i := 0; 
    IF scx > 0 THEN
      fixlistConst := pc
    END;
    WHILE i < scx DO
      PutCode(TSC[i].adr);
      INC(i)
    END;
    PutCode(0);
    
    (*build fixuplist from TXR*)
    i := 0;
    WHILE i < xrefx DO 
      IF TXR[i].adr # 0 THEN
        k := TXR[i].ref;
        PutCode(TXR[i].adr); PutCode(fixlist[k]); fixlist[k] := pc-2
      END;
      INC(i)
    END;
    
    (*build fixuplist from TXTD*)  
    i := 0;
    WHILE i < xreft DO 
      IF TXTD[i].adr # 0 THEN
        k := TXTD[i].ref;
        PutCode(TXTD[i].adr); PutCode(tdfixlist[k]); tdfixlist[k] := pc-2
      END;
      INC(i)
    END;
    
    ifixup := pc;
    (*build interrupt handler fixup list*)
    i := 0;
    WHILE i < xrefi DO
      PutCode(TXI[i].ref); PutCode(TXI[i].adr);
      INC(i)
    END;
    PutCode(0)
          
  END FixupConstants;

  (* loading of operands and addresses into registers *)
  
  PROCEDURE GlobExtRef(VAR x: Item);
  BEGIN
    IF xrefx < ITlen THEN
      IF x.b >= 0 THEN
        TXR[xrefx].ref := 0
      ELSE
        TXR[xrefx].ref := -x.b
      END;
      TXR[xrefx].adr := pc; INC(xrefx);
      IF firstfixloc=0 THEN firstfixloc := pc END
    END    
  END GlobExtRef;
  
  PROCEDURE TDRef(ref, adr: LONGINT);
  BEGIN
    IF xreft < ITlen THEN
      TXTD[xreft].ref := ref; TXTD[xreft].adr := pc; INC(xreft);
      IF firstfixloc = 0 THEN firstfixloc := pc END
    END
  END TDRef;
  
  PROCEDURE GlobConst(lev: LONGINT);
  BEGIN
    IF xrefx < ITlen THEN
      TXR[xrefx].ref := lev; TXR[xrefx].adr := pc; INC(xrefx);
      IF firstfixloc = 0 THEN firstfixloc := pc END
    END
  END GlobConst;


 (* PROCEDURE load(VAR x: Item);
    VAR r, cd, n, i: LONGINT; 
  BEGIN
    IF x.mode # OSAB.Reg THEN
      IF x.mode = OSAB.Var THEN
        GetReg1(r, x); 
        IF (x.r = GB) THEN
          i := 0;
          WHILE i < x.type.size DO
            GlobExtRef(x);
            IF x.b >= 0 THEN
              PutAVR17(LDS, r+i, x.a+i);
            ELSE
              PutAVR17(LDS, r+i, x.a*100H+i);
            END;
            i := i+1
          END
        ELSIF x.r = FP THEN
          IF (x.a >= 0) & (x.a <=63) THEN
            i := 0;
            WHILE i < x.type.size DO
              PutAVR9(LDD+1H, r+i, x.a+i);
              INC(i)
            END
          ELSE
            n := 10000H - x.a;
            PutAVR14(MOVW, 30, x.r);
            PutAVR1(5H, 30, n MOD 100H);
            PutAVR1(4H, 31, n DIV 100H MOD 100H);
            i := 0;
            WHILE i < x.type.size DO
              PutAVR7(901H, r+i);
              INC(i)
            END  
          END
        END
      ELSIF x.mode = OSAB.RegI THEN
        RelReg(x.r, 2);
        GetReg(r, x.type.size, 0);
        n := 10000H - x.a;
        IF (x.r MOD 2 = 0) THEN
          PutAVR14(MOVW, 30, x.r);
        ELSE
          PutAVR0(23H, 30, x.r); PutAVR0(23H, 31, x.r+1)
        END;
        PutAVR1(5H, 30, n MOD 100H);
        PutAVR1(4H, 31, n DIV 100H MOD 100H);
        i := 0;
        WHILE i < x.type.size DO
          PutAVR7(901H, r+i);
          INC(i)
        END
      ELSIF x.mode = OSAB.Par THEN
        GetReg1(r, x);
        IF x.r = GB THEN
          GlobExtRef(x); PutAVR17(LDS, 30, x.a);
          GlobExtRef(x); PutAVR17(LDS, 31, x.a+1)
        ELSIF x.r = FP THEN
          PutAVR14(MOVW, 26, 28);
          n := 10000H-x.a;
          PutAVR1(5H, 26, n MOD 100H); PutAVR1(4H, 27, n DIV 100H MOD 100H);
          PutAVR7(90DH, 30); PutAVR7(90CH, 31)
        END;
        n := 10000H - x.b;
        PutAVR1(5H, 30, n MOD 100H);
        PutAVR1(4H, 31, n DIV 100H MOD 100H);
        i := 0;
        WHILE i < x.type.size DO
          PutAVR7(901H, r+i); (*LD r, Z+*)
          INC(i)
        END      
      ELSIF x.mode = OSAB.Const THEN
        IF x.type.form <= OSAB.Set THEN (*load Byte, Bool, Char, Int, Real, Set constant*)
          GetReg1(r, x);
          IF r >= 16 THEN
            cd := CPI + 0BH;
            n := x.a;
            i := 0;
            WHILE i < x.type.size DO
              PutAVR1(cd, r+i, n MOD 100H);
              n := n DIV 100H;
              INC(i)
            END
          ELSE
            OSAS.Mark("too few registers for loading constant");
          END
        ELSIF x.type.form = OSAB.Proc THEN
          GetReg(r, 2, 16);
          IF x.b >= 0 THEN
            PutAVR1(0EH, r, x.a MOD 100H);
            PutAVR1(0EH, r+1, x.a DIV 100H MOD 100H)
          ELSE
            GlobExtRef(x);
            PutAVR1(0EH, r, x.a MOD 100H);
            PutAVR1(0EH, r+1, x.a DIV 100H MOD 100H)
          END
        END
      ELSIF x.mode = CC THEN
        GetReg(r, 1, 16);
        PutAVR11(negated(x.r), 2);
        FixLink(x.b);
        PutAVR1(0EH, r, 1);
        PutAVR3(RJMP, 1);
        FixLink(x.a);
        PutAVR1(0EH, r, 0)
      END;
      x.mode := OSAB.Reg; x.r := r;
    END
  END load;*)

(* my *)
PROCEDURE MovVarToReg(VAR x:Item; r:LONGINT);
	VAR rname:OSAS.Ident;
		label: ARRAY 255 OF CHAR;
	BEGIN
		IF (x.type.size > 1 ) THEN
			Gen.RegName(r, rname);
			Gen.Mov(f);
		ELSIF (x.type.size = 1) THEN
			Gen.RegName(r, rname);
			Gen.Movsb(f);
		END;
		COPY(x.label, label);
		OIO.FWriteString(f, label);
		Gen.WriteComma(f);
		OIO.FWriteString(f, rname);
	END MovVarToReg;

PROCEDURE LoadReg(VAR x: Item; r: LONGINT);
	VAR rname: OSAS.Ident;
	BEGIN
		IF (x.type.size > 1) THEN
			Gen.RegName(r, rname);
			Gen.Mov(f);
		ELSIF (x.type.size = 1) THEN
			Gen.RegName(r, rname);
			Gen.Movsb(f);
			(* continue! *)
		END;

	END LoadReg;

PROCEDURE LoadParamToReg(base, offset, r: LONGINT; VAR x:Item);
	VAR basename, rname: OSAS.Ident;
	BEGIN
		Gen.RegName(base, basename);
		Gen.RegName(r, rname);
		IF(x.type.size = 1) THEN
			Gen.Movsb(f);
		ELSIF (x.type.size >= 4) THEN
			Gen.Mov(f);
		END;
		OIO.FWriteLInt(f, offset); (* offset for parameters is negative *)
		Gen.WriteBase(f, base);
		Gen.WriteComma(f);
		OIO.FWriteString(f, rname);
	END LoadParamToReg;

PROCEDURE LoadConstToReg(val, r:LONGINT);
	VAR rname: OSAS.Ident;
	BEGIN
		Gen.RegName(r, rname);
		Gen.Mov(f);
		Gen.Const(f);
		OIO.FWriteLInt(f, val);
		Gen.WriteComma(f);
		OIO.FWriteString(f, rname);
	END LoadConstToReg;

PROCEDURE load(VAR x: Item);
	VAR r, offset, k: LONGINT;
	BEGIN

	IF (x.mode # OSAB.Reg) THEN
		IF (x.mode = OSAB.Var) THEN
			(* IF (x.b >= 0) THEN (?)*)
			GetReg1(r, x); (* free register which contains x.r(i.e. base) and get new register to move x's value there *)
			IF (x.r = rbp) THEN (* local var or par *)
				IF (x.a < 0) THEN (* parameter *)
					k:=x.a(*??? -4 ???*);
					LoadParamToReg(rbp, k, r, x); (* movl/movsbl -offset(%rbp), %rxx *)
				ELSIF x.a > 0 THEN (* local var *)
					k:=x.a;
					MovVarToReg(x, r);(*mov x.label, %rxx*)
				END;
			ELSE LoadReg(x, r); END;
		ELSE
			GetReg1(r, x);
			LoadReg(x, r);
		END;	
	ELSIF (x.mode = OSAB.RegI) THEN	
		(* *)
	ELSIF (x.mode = RegX) THEN
		(* *)
	ELSIF (x.mode = OSAB.Const) THEN
		IF (x.type.form <= OSAB.Set) THEN (* byte, bool, char, int real, set *)
			LoadConstToReg(x.a, r);
		ELSIF (x.type.form = OSAB.Proc) THEN
			GetReg1(r, x);
			(* continue *);
		ELSIF (x.type.form = OSAB.NilTyp) THEN
			GetReg1(r,x);
			LoadConstToReg(0, r);
		END;
	ELSIF (x.mode = CC) THEN
		(* continue *)
	
	ELSE OSAS.Mark("illegal object (type?)"); END;
	

	END load;
  
  PROCEDURE loadRom(VAR x: Item); (*load a word from the ROM*)
    VAR r,  n,  i: LONGINT; 
  BEGIN
    IF x.mode # OSAB.Reg THEN
      IF x.mode = OSAB.Var THEN (*ROM address is x.a*)
        GetReg(r, x.type.size+1,4);
        PutAVR1(0EH, r, 0);
        GlobExtRef(x);
        PutAVR8(0B1H, r, 3BH); (*OUT rampz, r*)
        PutAVR1(0EH, 30, x.a MOD 100H);
        PutAVR1(0EH, 31, x.a DIV 100H MOD 100H);
        i := 0; r := r+1;
        WHILE i < x.type.size DO
          PutAVR7(907H, r+i);
          INC(i)
        END
      ELSIF x.mode = OSAB.Par THEN (*ROM address is stored in the RAM*)
        GetReg(r, x.type.size+1, 4);
        PutAVR1(0EH, r, 0);
        PutAVR8(0B1H, r, 3BH); (*OUT rampz, r*)
        PutAVR9(LDD+1H, 30, x.a); PutAVR9(LDD+1H, 31, x.a+1);
        PutAVR6(3H, 30); (*LSL 30, transform word address into byte address*)
        PutAVR6(13H, 31); (*ROL 31*)
        PutAVR11(0F10H, 2);
        PutAVR1(0EH, r, 1);
        PutAVR8(0B1H, r, 3BH); (*OUT rampz, r*)
        i := 0; r := r+1;
        WHILE i < x.type.size DO
          PutAVR7(907H, r+i);
          INC(i)
        END
      ELSIF x.mode = OSAB.RegI THEN
        GetReg(r, x.type.size+1, 4);
        PutAVR1(0EH, r, 0);
        PutAVR8(0B1H, r, 3BH); (*OUT rampz, r*)
        n := 10000H - x.a;
        IF (x.r MOD 2 = 0) THEN
          PutAVR14(MOVW, 30, x.r);
        ELSE
          PutAVR0(23H, 30, x.r); PutAVR0(23H, 31, x.r+1)
        END;
        PutAVR1(5H, 30, n MOD 100H);
        PutAVR1(4H, 31, n DIV 100H MOD 100H);
        PutAVR6(3H, 30); (*LSL 30, transform word address into byte address*)
        PutAVR6(13H, 31); (*ROL 31*)
        PutAVR11(0F10H, 2);
        PutAVR1(0EH, r, 1);
        PutAVR8(0B1H, r, 3BH); (*OUT rampz, r*)
        i := 0; r := r+1;
        WHILE i < x.type.size DO
          PutAVR7(907H, r+i);
          INC(i)
        END
      ELSE
        OSAS.Mark("bad item mode in loadRom");
      END;
      x.mode := OSAB.Reg; x.r := r; RelReg(r-1, 1);
    END
  END loadRom;

  PROCEDURE loadAdr(VAR x: Item);
    VAR r,  temp, n: LONGINT;
  BEGIN
    IF (x.mode # OSAB.RegI) THEN
      IF (x.mode = OSAB.Var) & (x.b >= 0) THEN
        GetReg(r, 2, 0);
        IF x.r = GB THEN (*load address of a global variable*)
          GlobExtRef(x);
          PutAVR1(CPI+0BH, r, x.a MOD 100H);
          PutAVR1(CPI+0BH, r+1, x.a DIV 100H);
        ELSIF x.r = FP THEN (*load address of a local variable*)
          IF r MOD 2 = 0 THEN
            PutAVR14(MOVW, r, FP)
          ELSE
            PutAVR0(23H, r, FP); PutAVR0(23H, r+1, FP+1)
          END;
          temp := 10000H - x.a;
          PutAVR1(CPI+2H, r, temp MOD 100H);
          PutAVR1(CPI+1H, r+1, temp DIV 100H)
        END
      ELSIF x.mode = OSAB.Par THEN
        RelReg(x.r, 2);
        GetReg(r, 2, 4);
        IF x.r MOD 2 = 0 THEN
          PutAVR14(MOVW, 30, x.r);
        ELSE
          PutAVR0(23H, 30, x.r); PutAVR0(23H, 31, x.r);
        END;
        n := 10000H - x.a;
        PutAVR1(5H, 30, n MOD 100H); PutAVR1(4H, 31, n DIV 100H MOD 100H);
        PutAVR7(901H, r); PutAVR7(LD, r+1)
      ELSE OSAS.Mark("bad mode in loadAdr")
      END;
      x.mode := OSAB.RegI; x.r := r
    END
  END loadAdr;
  
  PROCEDURE loadAdrRom(VAR x: Item);
    VAR r,  n: LONGINT;
  BEGIN
    IF (x.mode # OSAB.RegI) THEN
      IF (x.mode = OSAB.Var) & (x.b >= 0) THEN
        PutAVR1(0EH, 16, 0);
        GetReg(r, 2, 0);
        GlobExtRef(x);
        PutAVR8(0B1H, 16, 3BH); (*OUT rampz, r*)
        PutAVR1(0EH, r, x.a MOD 100H);
        PutAVR1(0EH, r+1, x.a DIV 100H);
      ELSIF x.mode = OSAB.Par THEN
        RelReg(x.r, 2);
        GetReg(r, 2, 4);
        IF x.r MOD 2 = 0 THEN
          PutAVR14(MOVW, 30, x.r);
        ELSE
          PutAVR0(23H, 30, x.r); PutAVR0(23H, 31, x.r);
        END;
        n := 10000H - x.a;
        PutAVR1(5H, 30, n MOD 100H); PutAVR1(4H, 31, n DIV 100H MOD 100H);
        PutAVR7(901H, r); PutAVR7(LD, r+1)
      ELSE OSAS.Mark("bad mode in loadAdrRom")
      END
    END;
    x.mode := OSAB.RegI; x.r := r  
  END loadAdrRom;

  PROCEDURE loadCC(VAR x: Item);
  BEGIN
    IF x.type.form = OSAB.Bool THEN
      IF x.mode = OSAB.Const THEN PutAVR0(CPC+10H, 0, 0); SetCC(x, cond[1-x.a])
      ELSE load(x); PutAVR1(CPI, x.r, 1); RelReg(x.r, 1); SetCC(x, cond[0])
      END
    ELSE OSAS.Mark("must be boolean"); x.a := 0; x.b := 0
    END
  END loadCC;

  PROCEDURE loadArrayLen(VAR x: Item);
  BEGIN 
  END loadArrayLen;

  PROCEDURE loadString(VAR x: Item);
    VAR r, xL: LONGINT;
  BEGIN 
    xL := x.a DIV 10000H; x.a := (x.a MOD 10000H)*2;
    IF x.mode = OSAB.Var THEN
      x.b := xL
    END;
    GetReg(r, 2, 2); x.mode := OSAB.Reg; x.r := r; 
    enterStr(x.a); PutAVR1(0EH, r, x.a MOD 100H); PutAVR1(0EH, r+1, x.a DIV 100H MOD 100H)
  END loadString;
  
  PROCEDURE AllocString*(VAR adr: LONGINT);
    (*String Constant declaration; moves string into code array*)
  BEGIN 
    adr := OSAS.slen*10000H+OSAS.ival
  END AllocString;
  
  PROCEDURE Q(T: OSAB.Type);
    VAR obj: OSAB.Object;
  BEGIN
    IF T.base # NIL THEN
      Q(T.base); obj := T.typobj;
      TDRef(-obj.lev, obj.val);
      PutCode(obj.val)
    END
  END Q;
  
  PROCEDURE AllocTD*(obj: OSAB.Object);
    VAR n: INTEGER; tp: OSAB.Type;
  BEGIN 
    obj.lev := 0;
    obj.val := pc; 
    tp := obj.type;
    IF tp.form = OSAB.Pointer THEN tp := tp.base END;
    PutCode(tp.size); (*does not include tag*)
    Q(tp); n := tp.nofpar;
    WHILE n < 2 DO PutCode(0); INC(n) END
  END AllocTD;

  (* Items: Conversion from constants or from Objects on the Heap to Items on the Stack*)

  PROCEDURE Val*(VAR x: Item): LONGINT;
  BEGIN 
    RETURN x.a
  END Val;
  
  PROCEDURE Lev*(VAR x: Item): LONGINT;
  BEGIN RETURN x.b
  END Lev;
  
  PROCEDURE MakeConstItem*(VAR x: Item; typ: OSAB.Type; val: LONGINT);
  BEGIN 
    x.mode := OSAB.Const; x.type := typ; x.a := val;
  END MakeConstItem;

  PROCEDURE MakeRealItem*(VAR x: Item; val: REAL);
  BEGIN 
  END MakeRealItem;

  PROCEDURE MakeStringItem*(VAR x: Item; inx, len: LONGINT);
  BEGIN 
    x.mode := OSAB.Const; x.type := OSAB.strType; x.a := inx; x.b := len
  END MakeStringItem;

  PROCEDURE MakeItem*(VAR x: Item; y: OSAB.Object);
    VAR cl: INTEGER; 
  BEGIN 
    cl := y.class;
    x.mode := cl; x.type := y.type; x.a := y.val; x.b := y.lev; x.rdo := y.rdo;
    IF cl = OSAB.Var THEN
      IF y.lev <= 0 THEN x.r := GB; (*global variable*)
        IF x.type.form = OSAB.String THEN x.mode := OSAB.Var END;
      ELSIF y.lev = curlev THEN x.r := FP
      ELSE OSAS.Mark ("non-local not accessible"); x.r := FP
      END
    ELSIF cl = OSAB.Par THEN x.b := 0;
      IF y.lev <= 0 THEN x.r := GB
      ELSIF y.lev =curlev THEN x.r := FP
      ELSE OSAS.Mark("non-local not accessible"); x.r := FP
      END
    ELSIF cl = OSAB.Typ THEN x.r := GB
    ELSIF cl = OSAB.Reg THEN x.r := y.val
    ELSIF cl = OSAB.RegI THEN x.r := y.val; x.a := 0
    END
  END MakeItem;

  (* Code generation for Selectors, Variables, Constants *)
  PROCEDURE^ copyItem(r: LONGINT; x: Item); (*copy the value of x into register r*)

  PROCEDURE AddConst(VAR x: Item; n: LONGINT); (*x := x-n*)
    VAR r, i: LONGINT;
  BEGIN 
    GetReg1(r, x);
    IF r >= 16 THEN
      IF x.r # r THEN
        copyItem(r, x)
      END;
      PutAVR1(CPI+2, r, n MOD 100H);
      i := 1;
      WHILE i < x.type.size DO
        n := n DIV 100H;
        PutAVR1(CPI+1, r+i, n MOD 100H);
        i := i+1;
      END;
      x.r := r
    ELSE
      OSAS.Mark("too few registers for add constant operation"); 
    END
  END AddConst;

  PROCEDURE Field*(VAR x: Item; y: OSAB.Object);   (* x := x.y *)
  BEGIN 
    x.type := y.type;
    IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN x.a := x.a+y.val
    ELSIF x.mode = OSAB.Par THEN x.b := x.b+y.val
    ELSE OSAS.Mark("bad mode in field")
    END
  END Field;

  PROCEDURE Index*(VAR x, y: Item; check: BOOLEAN);   (* x := x[y] *)
    VAR r, r0, r1, r2, s, s0, n, lim, i: LONGINT;
  BEGIN 
    s := x.type.base.size; lim := x.type.len;
    IF (y.mode = OSAB.Const) & (lim >= 0) THEN
      IF (y.a >= 0) & (y.a < lim) THEN
        IF (x.mode = OSAB.Var) OR (x.mode = OSAB.RegI) THEN x.a := y.a*s+x.a 
        ELSIF x.mode = OSAB.Par THEN x.b := y.a*s+x.b
        ELSE
          OSAS.Mark("bad mode in Index")
        END
      ELSE
        OSAS.Mark("index out of range")
      END
    ELSE load(y);
      IF check THEN (*check index bounds*)
      END;
      
      IF x.mode = OSAB.Par THEN (*r <= Mem[x.r+x.a] *)
        RelReg(x.r, 2); GetReg(r, 2, 0);
        IF x.r = GB THEN
          GlobExtRef(x);
          PutAVR17(LDS, r, x.a);
        ELSE (*x.r should be r28*)
          IF r # x.r THEN
            IF (r MOD 2 = 0) THEN
              PutAVR14(MOVW, r, x.r)
            ELSE
              PutAVR0(23H, r, x.r); PutAVR0(23H, r+1, x.r+1)
            END
          END;
          (*r := r+x.a*)
          n := 10000H - x.a; (*x.a is always positive*)
          PutAVR1(CPI+2, r, n MOD 100H);
          PutAVR1(CPI+1, r+1, n DIV 100H MOD 100H);
          IF (r MOD 2 =  0)  THEN
            PutAVR14(MOVW, 30, r); (*MOVW Z, r*)
          ELSE
            PutAVR0(23H, 30, r); PutAVR0(23H, 31, r+1)
          END;
          PutAVR7(901H, r); (*LD r, Z+*)
          PutAVR7(LD, r+1); (*LD, r, Z*)
          x.mode := OSAB.RegI; x.r := r; x.a := x.b
        END
      END;
      RelReg(y.r, 2); RelReg(x.r, 2); GetReg(r0, 2, 0); GetReg(r, 2, 0);
      IF r0 # y.r THEN
        IF (y.r MOD 2 = 0) THEN
          PutAVR14(MOVW, r0, y.r)
        ELSE
          PutAVR0(23H, r0, y.r); PutAVR0(23H, r0+1, y.r+1)
        END
      END;
      IF x.r = GB THEN (*store base address in r*)
        GlobExtRef(x);
        PutAVR1(0EH, r, 0);
        PutAVR1(0EH, r+1, 0);
      ELSIF r # x.r THEN
        IF (x.r MOD 2 = 0) THEN
          PutAVR14(MOVW, r, x.r)
        ELSE
          PutAVR0(23H, r, x.r); PutAVR0(23H, r+1, x.r+1)
        END
      END;
      s0 := s; n := 0;
      WHILE ~ODD(s0) DO s0 := s0 DIV 2; INC(n) END;
      IF x.mode IN {OSAB.RegI, OSAB.Var} THEN
        IF (s0 = 1) THEN
          i := n;
          WHILE i > 0 DO
            PutAVR6(03H, r0);
            PutAVR6(13H, r0+1);
            DEC(i)
          END;
          PutAVR0(03H, r, r0);
          PutAVR0(13H, r+1, r0+1)
        ELSE  (*x.r<=y.r * s+x.r*)   
          GetReg(r1, 2, 0); GetReg(r2, 2, 0);
          PutAVR1(0EH, r1, s MOD 100H);
          PutAVR1(0EH, r1+1, s DIV 100H MOD 100H);
          PutMul(r2, r0, r1);
          PutAVR0(03H, r, r2);
          PutAVR0(13H, r+1, r2+1);
          RelReg(r1, 2);
          RelReg(r2, 2)
        END;
        x.r :=r ; x.mode := OSAB.RegI; RelReg(r0, 2)
      ELSE OSAS.Mark("bad mode in Index")
      END
    END;
    x.type := x.type.base
  END Index;

  PROCEDURE DeRef*(VAR x: Item);
    VAR r, n: LONGINT;
  BEGIN
    IF x.mode = OSAB.Var THEN x.mode := OSAB.Par; x.b := 0;
    ELSIF (x.mode = OSAB.Par) OR (x.mode = OSAB.RegI) THEN
      RelReg(x.r, 2); GetReg(r, 2, 0);
      IF r # x.r THEN
        PutAVR14(MOVW, r, x.r);
      END;
      n := 10000H - x.a;
      PutAVR1(5H, r, n MOD 100H);
      PutAVR1(4H, r+1, n DIV 100H MOD 100H);
      PutAVR14(MOVW, 30, r);
      PutAVR7(901H, r); PutAVR7(800H, r+1); (*r := Mem[x.r+a]*)
      IF x.mode = OSAB.Par THEN
        n := 10000H - x.b; 
        PutAVR1(5H, r, n MOD 100H); PutAVR1(4H, r+1, n DIV 100H MOD 100H);
        PutAVR14(MOVW, 30, r);
        PutAVR7(901H, r); PutAVR7(800H, r+1);(* r := Mem[r+x.b]*)
      END;
      x.mode := OSAB.RegI; x.r := r; x.a := 0
    ELSIF x.mode = OSAB.Reg THEN x.mode := OSAB.RegI; x.a := 0
    ELSE OSAS.Mark("bad mode in DeRef") 
    END;
    x.type := x.type.base  
  END DeRef;

  PROCEDURE TypeTest*(VAR x: Item; T: OSAB.Type; varpar, isguard: INTEGER);
    VAR r0, r1: LONGINT; tdes: Item;
  BEGIN;
    IF varpar = 1 THEN 
    ELSE
      load(x);
      GetReg(r0, 2, 4);
      IF x.r MOD 2 = 0 THEN
        PutAVR14(MOVW, 26, x.r)
      ELSE
        PutAVR0(23H, 26, x.r); PutAVR0(23H, 27, x.r+1)
      END;
      r1 := x.r;
      PutAVR2(97H, 26, 2);
      (*load address of type descriptor into r0*)
      PutAVR7(90DH, r0);
      PutAVR7(90CH, r0+1);
      x.mode := OSAB.RegI; x.r := r0; x.a := T.nofpar;       
      loadRom(x);
      tdes.mode := OSAB.Var; tdes.a := T.typobj.val; tdes.b := T.typobj.lev;
      MakeItem(tdes, T.typobj); (*TD of y*)
      tdes.mode := OSAB.Var;
      loadAdrRom(tdes);
      PutAVR0(11H, x.r, tdes.r);
      PutAVR0(CPC, x.r+1, tdes.r+1);
      RelReg(tdes.r, 2); RelReg(r0, 2);
       RelReg(x.r, x.type.size);
      IF isguard # 1 THEN 
        RelReg(r1, 2)
      ELSE
        x.r := r1; x.mode := OSAB.Reg
      END;
      IF isguard = 0 THEN SetCC(x, 0F01H) 
      ELSE (*use INT0 interrupt as trap*) PutAVR11(BREQ, 1); PutAVR11(JMP, 2H)
      END    
    END  
  END TypeTest;
  
  (* Code generation for Boolean operators *)
  
  PROCEDURE Not*(VAR x: Item);   (* x := ~x *)
    VAR op, dst, bn, t: LONGINT;
  BEGIN
    IF x.mode # CC THEN loadCC(x) END;
    IF (x.r = 9BH) OR (x.r = 99H) OR (x.r = 0F60H) OR (x.r = 0F70H) THEN (*SBIS, SBIC, SBRS, SBRC*)
      IF (x.r = 9BH) OR (x.r = 99H) THEN
        op := codeAVR[pc-1] DIV 100H MOD 100H;
        dst := codeAVR[pc-1] DIV 8H MOD 20H;
        bn := codeAVR[pc-1] MOD 8H;
        codeAVR[pc-1] := negated(op)*100H+dst*8H+bn;
      ELSE
        op := codeAVR[pc-1] DIV 1000H MOD 10H *100H + codeAVR[pc-1] DIV 200H MOD 8H*10H + codeAVR[pc-1] DIV 8H MOD 2H;
        dst := codeAVR[pc-1] DIV 10H MOD 20H;
        bn := codeAVR[pc-1] MOD 8H;
        op := negated(op);
        codeAVR[pc-1] := op DIV 10H MOD 100H *200H+dst*10H+ op MOD 10H * 8H + bn
      END;      
    END;
    x.r := negated(x.r); t := x.a; x.a := x.b; x.b := t
  END Not;

  PROCEDURE And1*(VAR x: Item);   (* x := x & *)
  BEGIN
    IF x.mode # CC THEN loadCC(x) END;
    IF (x.r = 9BH) OR (x.r = 99H) OR (x.r = 0F60H) OR (x.r = 0F70H) THEN (*SBIS, SBIC, SBRS, SBRC*)
      PutCode(RJMP*10000H+x.a); x.a := pc-1; FixLink(x.b); x.b := 0
    ELSE
      PutAVR11(x.r, 1);
      PutCode(RJMP * 10000H+x.a); x.a := pc-1; FixLink(x.b); x.b := 0
    END
  END And1;

  PROCEDURE And2*(VAR x, y: Item);
  BEGIN
    IF y.mode # CC THEN loadCC(y) END;
    x.a := merged(y.a, x.a); x.b := y.b; x.r := y.r
  END And2;

  PROCEDURE Or1*(VAR x: Item);   (* x := x OR *)
    VAR op, dst, bn: LONGINT;
  BEGIN
    IF x.mode # CC THEN loadCC(x) END;
    IF (x.r = 9BH) OR (x.r = 99H) OR (x.r = 0F60H) OR (x.r = 0F70H) THEN (*SBIS, SBIC, SBRS, SBRC*)
      IF (x.r = 9BH) OR (x.r = 99H) THEN
        op := codeAVR[pc-1] DIV 100H MOD 100H;
        dst := codeAVR[pc-1] DIV 8H MOD 20H;
        bn := codeAVR[pc-1] MOD 8H;
        codeAVR[pc-1] := negated(op)*100H+dst*8H+bn;
      ELSE
        op := codeAVR[pc-1] DIV 1000H MOD 10H *100H + codeAVR[pc-1] DIV 200H MOD 8H*10H + codeAVR[pc-1] DIV 8H MOD 2H;
        dst := codeAVR[pc-1] DIV 10H MOD 20H;
        bn := codeAVR[pc-1] MOD 8H;
        op := negated(op);
        codeAVR[pc-1] := op DIV 10H MOD 100H *200H+dst*10H+ op MOD 10H * 8H + bn
      END;      
      PutCode(RJMP*10000H+x.b); x.b := pc-1; FixLink(x.a); x.a := 0
    ELSE
      PutCode(x.r*10000H+x.b); x.b := pc-1; FixLink(x.a); x.a := 0
    END
  END Or1;

  PROCEDURE Or2*(VAR x, y: Item);
  BEGIN
    IF y.mode # CC THEN loadCC(y) END;
    x.a := y.a; x.b := merged(y.b, x.b); x.r := y.r
  END Or2;

  (* Code generation for arithmetic operators *)

  PROCEDURE Neg*(VAR x: Item);   (* x := -x *)
  BEGIN
    IF x.type.form = OSAB.Int THEN
      IF x.mode = OSAB.Const THEN x.a := 10000H-x.a 
      ELSE load(x); PutAVR7(LD+120H, x.r+1); PutAVR7(LD+121H, x.r); PutAVR1(CPI+1H, x.r+1, 0FFH) END
    END
  END Neg;

  PROCEDURE AddOp*(op: INTEGER; VAR x, y: Item);   (* x op y *)
    VAR  r, i: LONGINT;
  BEGIN
    IF op = OSAS.plus THEN
      IF y.mode = OSAB.Const THEN
        IF x.mode = OSAB.Const THEN
          x.a := x.a + y.a;
        ELSE
          load(x);
          IF SHORT( y.a) >= 0 THEN 
            AddConst(x, 10000H-y.a)
          ELSE
            AddConst(x, y.a)
          END
        END
      ELSIF x.mode = OSAB.Const THEN
        load(y);
        IF SHORT(x.a) >=0 THEN
          AddConst(y, 10000H-x.a)
        ELSE
          AddConst(y, x.a)
        END;
        x := y
      ELSE (*x, y are all variables*)
        load(x); load(y);  GetReg2(r, x, y);
        IF x.r # r THEN
          copyItem(r, x)
        END;
        PutAVR0(CPC+2H, r, y.r);
        i := 1;
        WHILE (i < y.type.size) DO
          PutAVR0(CPC+12H, r+i, y.r+i);
          i := i+1
        END;
        x.r := r
      END
    ELSE (*op=minus*)
      IF y.mode = OSAB.Const THEN
        IF x.mode = OSAB.Const THEN x.a := x.a - y.a
        ELSE
          load(x);
          IF SHORT(y.a) >= 0 THEN
            AddConst(x, y.a)
          ELSE
            AddConst(x, 10000H-y.a)
          END
        END
      ELSE (*x, y are all variables or constant-var*)
        load(x); load(y); GetReg2(r, x, y);
        IF x.r # r THEN
          copyItem(r, x)
        END;
        PutAVR0(CPC+11H, r, y.r);
        i := 1;
        WHILE (i < y.type.size) DO
          PutAVR0(CPC+1H, r+i, y.r+i);
          i := i+1
        END;
        x.r := r 
      END        
    END    
  END AddOp;

  PROCEDURE log(m: LONGINT; VAR n: LONGINT): LONGINT;
  BEGIN
    n := 0;
    WHILE ~ODD(m) DO m := m DIV 2; INC(n) END;
    RETURN m 
  END log;

  PROCEDURE multiply(VAR x, c: Item);  (*c.a >= 2*)
    VAR i,  m, r: LONGINT;
  BEGIN 
    GetReg(r, x.type.size, 0);
    m := c.a; i := 0;
    REPEAT m := m DIV 2; INC(i) UNTIL ODD(m);
    IF m=1 THEN
      PutAVR14(MOVW, r, x.r);
      IF ODD(c.a) THEN
        WHILE i > 0 DO
          PutAVR6(LSL, r);
          PutAVR6(LSL+10H, r+1);
          DEC(i)
        END;
        PutAVR0(CPC+2H, r, x.r);
        PutAVR0(CPC+12H, r+1, x.r+1);
      ELSE
        WHILE i > 0 DO
          PutAVR6(LSL, r);
          PutAVR6(LSL+10H, r+1);
          DEC(i)
        END
      END
    ELSE
      load(c);
      PutMul(r, x.r, c.r)
    END;
    RelReg(c.r, c.type.size);
    RelReg(x.r, x.type.size);
    x.r := r;
  END multiply;


  PROCEDURE^ SaveRegs*(VAR rs: SET);

  PROCEDURE MulOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
    VAR r, i: LONGINT; res: SET;
  BEGIN
    IF (x.mode = OSAB.Const) & (y.mode = OSAB.Const) THEN
      IF op = OSAS.times THEN x.a := x.a * y.a
      ELSIF y.a = 0 THEN OSAS.Mark("Divide by 0")
      ELSIF op = OSAS.div THEN x.a := x.a DIV y.a
      ELSE x.a := x.a MOD y.a
      END
    ELSIF op = OSAS.times THEN
      IF (x.mode = OSAB.Const) & (SHORT(x.a) >= 2) THEN
        load(y); multiply(y, x); x.mode := OSAB.Reg; x.r := y.r
      ELSIF (y.mode = OSAB.Const) & (SHORT(y.a) >= 2) THEN
        load(x); multiply(x, y)
      ELSE
        load(x); load(y); GetReg(r, x.type.size, 0); PutMul(r, x.r, y.r);  RelReg(x.r, x.type.size); RelReg(y.r, y.type.size); x.r := r
      END
    ELSIF (op = OSAS.div) OR (op = OSAS.mod) THEN
      load(x); load(y);
      IF (x.mode = OSAB.Reg) OR (y.mode = OSAB.Reg) THEN (*don't save registers of x and y*)
        IF x.mode = OSAB.Reg THEN
          EXCL(regs, x.r); EXCL(regs, x.r+1)
        END;
        IF y.mode = OSAB.Reg THEN
          EXCL(regs, y.r); EXCL(regs, y.r+1)
        END;
      END;
      SaveRegs(res);
      IF op = OSAS.div THEN
        PutCode(1H); PutCode(fixlistFP);
        fixlistFP := pc-2;
      ELSE
        PutCode(2H); PutCode(fixlistFP);
        fixlistFP := pc-2
      END;
      IF res # {} THEN
        regs := res;
        GetReg(r, 2, 0);
        PutAVR14(MOVW, r, 24);
        i := 31;
        WHILE i >= 0 DO (*restore register*)
          IF i IN res THEN
            PutAVR7(90FH, i)
          END;
          DEC(i)
        END
      ELSE
        regs :={};
        INCL(regs, 24); INCL(regs, 25);
        r := 24;
      END;
      x.mode := OSAB.Reg; x.r := r
    END;  
  END MulOp;

  (* Code generation for REAL operators *)

  PROCEDURE PrepOpd*(VAR x: Item);
  BEGIN load(x)  (*for real division*)
  END PrepOpd;

  PROCEDURE RealOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
     (*x.type.form = Real*)
  BEGIN 
  END RealOp;

  (* Code generation for set operators *)

  PROCEDURE^ Shift*(fct: LONGINT; VAR x, y: Item); (*x := x shift y*)

  PROCEDURE Singleton*(VAR x, y: Item);  (* x := {y} *)
  BEGIN
    IF y.mode = OSAB.Const THEN x.mode := OSAB.Const; x.a := ASH(1, y.a)
    ELSE
       x.mode := OSAB.Const; x.a := 1; Shift(0, x, y) (*LSL x, y*)
    END  
  END Singleton;
  
  
  PROCEDURE Set*(VAR x, y, z: Item);   (* x := {y .. z} *)
    VAR  temp: Item;
  BEGIN
    IF (y.mode = OSAB.Const) & (z.mode = OSAB.Const) THEN
      x.mode := OSAB.Const;
      IF y.a <= z.a THEN
        x.a := ASH(2, z.a) - ASH(1, y.a) 
      ELSE x.a := 0
      END
    ELSE
      x.mode := OSAB.Const; x.a := 2; x.type := OSAB.intType;
      MakeConstItem(temp, OSAB.intType, 1);
      Shift(0, x, z); Shift(0, temp, y);
      AddOp(OSAS.minus, x, temp);
      x.type := OSAB.setType; x.mode := OSAB.Var      
    END
      
  END Set;

  PROCEDURE In*(VAR x, y: Item);  (* x := x IN y *)
    VAR r: LONGINT;
  BEGIN 
     load(x); load(y);
     Shift(1, y, x);
     PutAVR1(ANDI, y.r, 1);
     RelReg(x.r, x.type.size);
    RelReg(y.r, y.type.size);
     SetCC(x, BRNE)     
  END In;
  
  PROCEDURE SetOp*(op: INTEGER; VAR x, y: Item);   (* x := x op y *)
    VAR  r, r0, op0: LONGINT;  (*x.type.form = Set*)
  BEGIN
    IF op = OSAS.plus THEN op0 := 22H (*OR*)
    ELSIF op = OSAS.minus THEN op0 := 920H(*COM*)
    ELSIF op = OSAS.times THEN op0 := 20H(*AND*)
    ELSIF op = OSAS.rdiv THEN op0 := 21H (*EOR*)
    ELSE OSAS.Mark("not a set operator"); op0 := 0
    END;
    IF (y.mode = OSAB.Const) & (x.mode = OSAB.Const) THEN
      IF op = OSAS.plus THEN
        x.a := SYSTEM.VAL(LONGINT, SYSTEM.VAL(SET, x.a) + SYSTEM.VAL(SET, y.a))
      ELSIF op = OSAS.minus THEN
        x.a := SYSTEM.VAL(LONGINT, SYSTEM.VAL(SET, x.a) - SYSTEM.VAL(SET, y.a))
      ELSIF op = OSAS.times THEN
        x.a := SYSTEM.VAL(LONGINT, SYSTEM.VAL(SET, x.a) * SYSTEM.VAL(SET, y.a))
      ELSE
        x.a := SYSTEM.VAL(LONGINT, SYSTEM.VAL(SET, x.a) / SYSTEM.VAL(SET, y.a))
      END
    ELSE
      load(x); load(y);
      GetReg2(r, x, y);
      IF (r # x.r) & (x.r MOD 2 = 0) & (r MOD 2 = 0) THEN
        PutAVR14(MOVW, r, x.r)
      ELSE
        PutAVR0(23H, r, x.r); PutAVR0(23H, r+1, x.r+1)
      END;
      IF op # OSAS.minus THEN
        PutAVR0(op0, r, y.r);
        PutAVR0(op0, r+1, y.r+1)
      ELSE
        GetReg(r0, y.type.size, 0);
        PutAVR14(MOVW, r0, y.r);
        PutAVR7(op0, r0); PutAVR7(op0, r0+1);
        PutAVR0(20H, r, r0); PutAVR0(20H, r+1, r0);
        RelReg(r0, y.type.size)
      END;
      x.r := r;
    END;
    
  END SetOp;

  (* Code generation for relations *)

  PROCEDURE IntRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
    VAR i: LONGINT;
  BEGIN
    load(x); load(y);
    IF (op=OSAS.leq) OR (op = OSAS.gtr) THEN
      IF op = OSAS.leq THEN
        op := OSAS.geq
      ELSE
        op := OSAS.lss
      END;
      PutAVR0(CPC+10H, y.r, x.r);
      i := 1;
      WHILE (i < y.type.size) DO
        PutAVR0(CPC, y.r+i, x.r+i);
        INC(i)
      END
    ELSE
      PutAVR0(CPC+10H, x.r, y.r);
      i := 1;
      WHILE (i <x.type.size) DO
        PutAVR0(CPC, x.r+i, y.r+i);
        INC(i)
      END
    END;
    RelReg(x.r, x.type.size); RelReg(y.r, y.type.size); SetCC(x, cond[op-OSAS.eql])      
    
  END IntRelation;

  PROCEDURE SetRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
  VAR i: LONGINT;
  BEGIN 
    load(x); load(y);
    IF (op=OSAS.leq) OR (op = OSAS.gtr) THEN
      IF op = OSAS.leq THEN
        op := OSAS.geq
      ELSE
        op := OSAS.lss
      END;
      PutAVR0(CPC+10H, y.r, x.r);
      i := 1;
      WHILE (i < y.type.size) DO
        PutAVR0(CPC, y.r+i, x.r+i);
        INC(i)
      END
    ELSE
      PutAVR0(CPC+10H, x.r, y.r);
      i := 1;
      WHILE (i <x.type.size) DO
        PutAVR0(CPC, x.r+i, y.r+i);
        INC(i)
      END
    END;
    RelReg(x.r, x.type.size); RelReg(y.r, y.type.size); SetCC(x, cond[op-OSAS.eql])  
  END SetRelation;

  PROCEDURE RealRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
  BEGIN 
  END RealRelation;

  PROCEDURE StringRelation*(op: INTEGER; VAR x, y: Item);   (* x := x < y *)
    VAR r0, r1, pc0: LONGINT;  (*x, y are char arrays or strings*)
  BEGIN
    IF x.type.form = OSAB.String THEN loadString(x) ELSE loadAdr(x) END;
    IF y.type.form = OSAB.String THEN loadString(y) ELSE loadAdr(y) END;
    GetReg(r0, 1, 4); GetReg(r1, 1, 4);
    IF x.r MOD 2 = 0 THEN
      PutAVR14(MOVW, 26, x.r)
    ELSE
      PutAVR0(23H, 26, x.r); PutAVR0(23H, 27, x.r+1)
    END;
    IF y.r MOD 2 = 0 THEN
      PutAVR14(MOVW, 30, y.r)
    ELSE
      PutAVR0(23H, 30, y.r); PutAVR0(23H, 31, y.r+1)
    END;
    pc0 := pc;
    PutAVR7(90DH, r0); (*LD r0, x+*)
    PutAVR7(901H, r1); (*LD r1, z+*)
    IF (op=OSAS.leq) OR (op = OSAS.gtr) THEN
      IF op = OSAS.leq THEN
        op := OSAS.geq
      ELSE
        op := OSAS.lss
      END;
      PutAVR0(CPC+10H, r1, r0);
    ELSE
      PutAVR0(CPC+10H, r0, r1);
    END;
    PutAVR11(0F11H, 2);
    PutAVR1(CPI, r0, 0);
    PutAVR11(0F11H, pc0-pc-1);
    RelReg(x.r, 2); RelReg(y.r, 2); RelReg(r0, 1); RelReg(r1, 1);
    SetCC(x, cond[op-OSAS.eql])
  END StringRelation;
  
  (* Code generation of Assignments *)

  PROCEDURE PrepStore*(VAR x: Item);
  BEGIN
    
  END PrepStore;

  PROCEDURE Store*(VAR x, y: Item); (* x := y *)
    VAR cd, i, size, n, r: LONGINT;
  BEGIN 
    load(y);
    IF x.mode = OSAB.Reg THEN
      IF x.r # y.r THEN (*assignment statement in leaf procedure*)
        copyItem(x.r, y)
      END
    ELSE (*store y into a variable x*)
      size := y.type.size;
      IF x.type.size < size THEN
        size := x.type.size
      END;
      IF x.type.size > size THEN
        GetReg(r, 1, 0);
        PutAVR6(LSL+1EH, r); (*CLR r*)
      END;
      cd := LDS + 10H; (*cd = STS*)
      IF x.mode = OSAB.Var THEN 
        IF (x.r = GB) THEN
          i := 0;
          WHILE (i < size) DO
            GlobExtRef(x);
            PutAVR17(cd, y.r+i, x.a+i);
            INC(i)
          END;
          IF (i < x.type.size) THEN
            WHILE i < x.type.size DO
              PutAVR17(LDS+10H, r, x.a+i);  
              INC(i)
            END
          END
        ELSIF (x.r = FP) THEN
          IF (x.a+x.type.size <= 63) & (x.a+y.type.size <= 63) THEN
            cd := LDD+11H;
            i := 0;
            WHILE (i < size) DO
              PutAVR9(cd, y.r+i, x.a+i);
              INC(i)
            END;
            IF (i < x.type.size) THEN
              WHILE i < x.type.size DO
                PutAVR9(LDS+11H, r, x.a+i);  
                INC(i)
              END
            END
          ELSE
            n := 10000H - x.a;
            PutAVR14(MOVW, 30, x.r);
            PutAVR1(5H, 30, n MOD 100H);
            PutAVR1(4H, 31, n DIV 100H MOD 100H);
            i := 0;
            WHILE i < size DO
              PutAVR7(911H, y.r+i); (*ST Z+, y.r*)
              INC(i)
            END;
            IF (i < x.type.size) THEN
              WHILE i < x.type.size DO
                PutAVR7(911H, r);  
                INC(i)
              END;
            END
          END
        END
      ELSIF x.mode = OSAB.Par THEN
        IF (x.r = GB) THEN
          GlobExtRef(x); PutAVR17(LDS, 30, x.a);
          GlobExtRef(x); PutAVR17(LDS, 31, x.a+1)
        ELSIF x.r = FP THEN
          PutAVR14(MOVW, 26, 28);
          n := 10000H-x.a;
          PutAVR1(5H, 26, n MOD 100H); PutAVR1(4H, 27, n DIV 100H MOD 100H);
          PutAVR7(90DH, 30); PutAVR7(90CH, 31);
        END;
        n := 10000H - x.b;
        PutAVR1(5H, 30, n MOD 100H);
        PutAVR1(4H, 31, n DIV 100H MOD 100H);
        i := 0;
        WHILE i < size DO
          PutAVR7(911H, y.r+i); (*ST Z+, y.r*)
          INC(i)
        END      
      ELSIF x.mode = OSAB.RegI THEN
        n := 10000H - x.a;
        PutAVR14(MOVW, 30, x.r);
        PutAVR1(5H, 30, n MOD 100H);
        PutAVR1(4H, 31, n DIV 100H MOD 100H);
        i := 0;
        WHILE i < size DO
          PutAVR7(911H, y.r+i); (*ST Z+, y.r*)
          INC(i)
        END;
        IF (i < x.type.size) THEN
          WHILE i < x.type.size DO
            PutAVR7(911H, r);  
            INC(i)
          END;
        END;
        RelReg(x.r, 2)
      END;
      IF x.type.size > size THEN
        RelReg(r, 1)
      END;
    END;
    RelReg(y.r, y.type.size)
  END Store;

  (*PROCEDURE CopyRecord*(VAR x, y: Item);  (* x := y *)
    VAR cnt, data, s, pc0, i, elemSize, n: LONGINT;
  BEGIN 
    s := x.type.size; elemSize := x.type.base.size; n := 10000H - elemSize;
    IF s > 0 THEN
      loadAdr(x); loadAdr(y); GetReg(cnt, 2, 0); GetReg(data, x.type.base.size, 0);
      PutAVR1(0EH, cnt, s MOD 100H);
      PutAVR1(0EH, cnt+1, s DIV 100H MOD 100H);
      pc0 := pc;
      PutAVR14(MOVW, 30, y.r);
      i := 0;
      WHILE i < elemSize DO
        PutAVR7(901H, data+i);
        INC(i)
      END;
      PutAVR14(MOVW, 30, x.r);
      i := 0;
      WHILE i < elemSize DO
        PutAVR7(911H, data+i);
        INC(i)
      END;
      PutAVR1(5H, x.r, n MOD 100H);
      PutAVR1(4H, x.r+1, n DIV 100H MOD 100H);
      PutAVR1(5H, y.r, n MOD 100H);
      PutAVR1(4H, y.r+1, n DIV 100H MOD 100H);
      PutAVR1(5H, cnt, 1); (*SUBI cnt, 01*)
      PutAVR1(4H, cnt+1, 0);
      PutAVR11(0F11H, pc0-pc-1);
      RelReg(x.r, 2); RelReg(y.r, 2); RelReg(cnt, 2); RelReg(data, x.type.base.size)
    END
  END CopyRecord;*)
  
  PROCEDURE CopyRecord*(VAR x, y: Item); (* x := y *)
    VAR
      cnt, data, s, pc0, i: LONGINT;
  BEGIN
    s := x.type.size;
    IF s > 0 THEN
      loadAdr(x); loadAdr(y);
      GetReg(cnt, 2, 4); GetReg(data, 1, 4);
      (*Put address of x, y to Z, X*)
      PutAVR14(MOVW, 30, y.r);
      PutAVR14(MOVW, 26, x.r);
      (*Load size into cnt*)
      PutAVR1(LDI, cnt, s MOD 100H);
      PutAVR1(LDI, cnt+1, s DIV 100H MOD 100H);
      
      pc0 := pc;
      PutAVR7(LDZI, data);
      PutAVR7(STXI, data);
      PutAVR1(SUBI, cnt, 1);
      PutAVR1(SBCI, cnt+1, 0);
      PutAVR11(BRNE, pc0-pc-1)      
    END;
  END CopyRecord;
  
  PROCEDURE CopyArray*(VAR x, y: Item);  (* x := y *)
    VAR cnt, data, pc0, elemSize, n, i: LONGINT; xL, yL: Item;
  BEGIN
    IF (x.type.form = OSAB.Array) & (x.type.len >= 0) THEN 
      MakeConstItem(xL, OSAB.intType, x.type.len)
    ELSE
      xL.mode := OSAB.Var; xL.type := OSAB.intType; xL.r := FP; xL.a := x.a +2;
    END;
    loadAdr(x); load(xL);
    IF (y.type.form = OSAB.Array) & (y.type.len >= 0) THEN
      MakeConstItem(yL, OSAB.intType, y.type.len)
    ELSE
      yL.mode := OSAB.Var; yL.type := OSAB.intType; yL.r := FP; yL.a := y.a+2
    END;
    loadAdr(y); load(yL);
    (*length check*)
    cnt := yL.r;
    elemSize := x.type.base.size;
    n := 1000H - elemSize;
    RelReg(xL.r, 2);
    GetReg(data, elemSize, 0);
    pc0 := pc;
    PutAVR14(MOVW, 30, y.r);
    i := 0;
    WHILE i < elemSize DO
      PutAVR7(901H, data+i);
      INC(i)
    END;
    PutAVR14(MOVW, 30, x.r);
    i := 0;
    WHILE i < elemSize DO
      PutAVR7(911H, data+i);
      INC(i)
    END;
    PutAVR1(5H, x.r, n MOD 100H);
    PutAVR1(4H, x.r+1, n DIV 100H MOD 100H);
    PutAVR1(5H, y.r, n MOD 100H);
    PutAVR1(4H, y.r+1, n DIV 100H MOD 100H);
    PutAVR1(5H, cnt, 1); (*SUBI cnt, 01*)
    PutAVR1(4H, cnt+1, 0);
    PutAVR11(0F11H, pc0-pc-1);
    RelReg(data, elemSize); RelReg(x.r, 2); RelReg(y.r, 2); RelReg(yL.r, 2)
  END CopyArray;

  PROCEDURE CopyString*(VAR x, y: Item);  (* x := y *)
    VAR r: LONGINT; pc0: INTEGER; yL: Item;
  BEGIN 
    GetReg(r, 1, 16); (*get a register used to pass value from source to destination*);
    IF x.type.len = -1 THEN (*open array*)
    (*fill this later on*)
    ELSIF x.type.len < y.b THEN OSAS.Mark("String too long")
    END;
    loadAdr(x); loadString(y);
    MakeConstItem(yL, OSAB.intType, y.b);
    load(yL);
    IF y.r MOD 2 = 0 THEN
      PutAVR14(MOVW, 30, y.r)
    ELSE
      PutAVR0(23H, 30, y.r); PutAVR0(23H, 31, y.r+1)
    END;
    IF x.r MOD 2 = 0 THEN
      PutAVR14(MOVW, 26, x.r)
    ELSE
      PutAVR0(23H, 26, x.r); PutAVR0(23H, 27, x.r+1)
    END;
    pc0 := pc;
    PutAVR7(901H, r); (*LD r, Z+*)
    PutAVR7(91DH, r); (*ST X+, r*)
    PutAVR1(5H, yL.r, 1); (*SUBI yL.r, 1*)
    PutAVR1(4H, yL.r+1, 0);
    PutAVR11(0F11H, pc0-pc-1);
    RelReg(x.r, 2); RelReg(y.r, 2); RelReg(r, 1); RelReg(yL.r, 2)    
  END CopyString;

(* Code generation for parameters *)

  PROCEDURE VarParam*(VAR x: Item; ftype: OSAB.Type);
    VAR x0, tdes: Item;
  BEGIN 
    x0 := x;
    loadAdr(x);
    IF (ftype.form = OSAB.Array) & (ftype.len < 0) THEN loadArrayLen(x0)
    ELSIF ftype.form = OSAB.Record THEN
      IF x.type.typobj = NIL THEN OSAS.Mark("anonymous type for VAR-Param") END;
      MakeItem(tdes, x.type.typobj); tdes.mode := OSAB.Var; loadAdr(tdes)
    END
  END VarParam;

  PROCEDURE ValueParam*(VAR x: Item);
    VAR r: LONGINT;
  BEGIN
    IF (x.mode = OSAB.Reg) & ((x.r < RL-1) OR (x.r >= RH)) THEN
      GetReg(r, x.type.size, 0); (*register variables*)
    ELSE
      load(x)
    END
  END ValueParam;

  PROCEDURE StringParam*(VAR x: Item);
    VAR r: LONGINT;
  BEGIN (*string constant?*)
    loadString(x); GetReg(r, 2, 4); PutAVR1(0EH, r, x.b MOD 100H); PutAVR1(0EH, r+1, x.b DIV 100H MOD 100H)  (*len*)
  END StringParam;

  PROCEDURE ByteParam*(VAR x: Item);  (*formal param of type SYSTEM.BYTES*)
  BEGIN 
  END ByteParam;

  (* For and Case Statements*)
  
  PROCEDURE Case*(VAR x: Item; n: LONGINT; VAR L: LONGINT); (*only allow INTEGER type case variables*)
    VAR k, a, r: LONGINT;
  BEGIN 
    load(x);
    GetReg(r, 2, 16);
    (*range check*)
    PutAVR1(CPI+0BH, r, n MOD 100H);
    PutAVR1(CPI+0BH, r+1, n DIV 100H MOD 100H);
    PutAVR0(CPC+10H, r, x.r);
    PutAVR0(CPC, r+1, x.r+1);
    PutAVR11(0F04H, 11);
    PutAVR1(CPI+0BH, r, 0);
    PutAVR1(CPI+0BH, r+1, 0);
    PutAVR0(CPC+10H, x.r, r);
    PutAVR0(CPC, x.r+1, r+1);
    PutAVR11(0F04H, 6);
    a := pc;
    PutAVR1(CPI+0BH, 30, a MOD 100H);
    PutAVR1(CPI+0BH, 31, a DIV 100H);
    PutAVR0(CPC+2H, 30, x.r);
    PutAVR0(CPC+12H, 31, x.r+1); 
    PutAVR2(ADIW, 30, 7);
    PutCode(9409H);(*IJMP*)
    PutCode(9588H); (*SLEEP*);
    L := pc; k := 0; (*branch table*)
    WHILE k < n DO PutCode(RJMP*10000H); INC(k) END;
    RelReg(r, 2); RelReg(x.r, x.type.size)
  END Case;

  PROCEDURE For0*(VAR x, y: Item);
  BEGIN
    IF x.mode = OSAB.Reg THEN Store(x, y); y.r := x.r ELSE load(y) END
  END For0;

  PROCEDURE For1*(VAR x, y, z, w: Item; VAR L: LONGINT);
    VAR
      r: LONGINT;
  BEGIN (*FOR x := y TO z BY w DO ... END*)
    IF w.mode # OSAB.Const THEN OSAS.Mark("increment not constant"); w.a := 1 END;
    r := y.r;
    IF (r >= RL) & (r <= RH) THEN INCL(regs, r) END;
    IF w.a > 0 THEN 
      IntRelation(OSAS.geq, z, y); PutAVR11(0F14H, 1); L := pc; PutCode(RJMP*10000H) (*codeAVR[pc] := RJMP*10000H; pc := pc+1*)
    ELSIF w.a <0 THEN
      IntRelation(OSAS.geq, y, z); PutAVR11(0F14H, 1); L := pc; PutCode(RJMP*10000H)  (*codeAVR[pc] := RJMP*10000H; pc := pc+1*)
    ELSE
      OSAS.Mark("zero increment");
      PutAVR11(0F11H, 0)
    END;
    IF x.mode # OSAB.Reg THEN Store(x, y); END
  END For1;

  PROCEDURE For2*(VAR x, y, w: Item);
    VAR n: LONGINT;
  BEGIN 
    n := w.a;
    IF x.mode # OSAB.Reg THEN
      load(x)
    END;
    IF SHORT(w.a) >= 0 THEN AddConst(x, 10000H-w.a) ELSE AddConst(x, w.a) END;
    RelReg(x.r, x.type.size); RelReg(y.r, y.type.size)
  END For2;
  
  (* In-line code procedures*)

  PROCEDURE Increment*(upordown: LONGINT; VAR x, y: Item);
    VAR op, inc: LONGINT; z: Item;
  BEGIN
    op := 0;
    IF y.type.form = OSAB.NoTyp THEN 
      inc := 1
    ELSE (*increment integer constant*)
      inc := y.a;
      IF (inc < 0) OR (inc > 100H) THEN OSAS.Mark("bad increment") END
    END;
    IF (upordown = 0) & (inc <= 63)THEN
      op := ADIW
    ELSIF (upordown = 1) & (inc <= 63) THEN
      op := ADIW+1
    ELSIF (upordown = 0)  THEN
      inc := 10000H - inc(*SUBI two's complement*)
    END;
    IF x.mode = OSAB.Reg THEN 
      IF ((op = ADIW) OR (op = ADIW+1)) & (x.r MOD 2 = 0) & (x.r >= 24) THEN
        PutAVR2(op, x.r, inc)
      ELSE
        IF (x.r MOD 2 # 0) OR (x.r < 24) THEN
          inc := 10000H -inc
        END;
        PutAVR1(5, x.r, inc MOD 100H); PutAVR1(4, x.r+1, inc DIV 100H MOD 100H)
      END
    ELSE
      z := x;
      load(x);
      IF ((op = ADIW) OR (op = ADIW+1)) & (x.r MOD 2 = 0) & (x.r >= 24)  THEN
        PutAVR2(op, x.r, inc)
      ELSE
        IF (x.r MOD 2 # 0) OR (x.r < 24) THEN
          inc := 10000H - inc
        END;
        PutAVR1(5, x.r, inc MOD 100H); PutAVR1(4, x.r+1, inc DIV 100H MOD 100H);
      END;
      Store(z, x);
      RelReg(x.r, 2)
    END    
  END Increment;

  PROCEDURE^ CFJump*(VAR x: Item);
  PROCEDURE^ Fixup*(VAR x: Item);

  PROCEDURE Assert*(VAR x: Item);
  BEGIN 
    CFJump(x); PutAVR18(JMP, 02H);
    Fixup(x)
  END Assert; 

  PROCEDURE New*(VAR x, y: Item);
    VAR T: Item; 
  BEGIN;
    loadAdr(x); 
    IF x.type.base = NIL THEN OSAS.Mark("no pointer base")
    ELSE
      MakeItem(T, x.type.base.typobj); T.mode := OSAB.Var;
      loadAdrRom(T); RelReg(T.r, 2);
    END;
    PutCode(2H); PutCode(fixlistMU); fixlistMU := pc-2; RelReg(x.r, 2)
  END New;

  PROCEDURE Pack*(VAR x, y: Item);
  BEGIN 
  END Pack;

  PROCEDURE Unpk*(VAR x, y: Item);
  BEGIN 
  END Unpk;

  PROCEDURE Get*(VAR x, y: Item);
    VAR r: LONGINT;
  BEGIN
    IF (x.mode = OSAB.Const) & (x.a < 3FH) THEN
      OSAS.Mark("wrong memory address!")
    ELSIF x.mode = OSAB.Const THEN
      GetReg(r, 1, 2);
      PutAVR17(LDS, r, x.a);
      x.r := r; x.mode := OSAB.Reg;
      Store(y, x);
      RelReg(x.r, 1)
    ELSIF x.mode = OSAB.Var THEN
      load(x);
      x.a := 0; x.mode := OSAB.RegI;
      Store(y, x)
    ELSE
      OSAS.Mark("wrong mode of address variable")
    END
  END Get;
  
  PROCEDURE GetFR*(VAR x, y: Item);
    VAR
      r, n, size, i: LONGINT; 
  BEGIN
    IF x.mode = OSAB.Const THEN (*x.a is word address*)
      n := x.a * 2;
      GetReg(r, y.type.size+1,4);
      PutAVR1(0EH, r, n DIV C16);
      PutAVR8(0B1H, r, 3BH); (*OUT rampz, r*)
      PutAVR1(0EH, 30, n MOD 100H);
      PutAVR1(0EH, 31, n DIV 100H MOD 100H);
      i := 0; r := r+1;
      WHILE i < x.type.size DO
        PutAVR7(907H, r+i);
        INC(i)
      END;
      x.r :=r; x.mode := OSAB.Reg; size := x.type.size;
      x.type.size := y.type.size;
      Store(y, x);
      x.mode := OSAB.Const; x.type.size := size
    ELSIF x.mode = OSAB.Var THEN
      x.mode := OSAB.Par;
      loadRom(x); size := x.type.size; x.type.size := y.type.size;
      Store(y, x);
      x.mode := OSAB.Var; x.type.size := size
    ELSE
      OSAS.Mark("wrong mode of address variable")
    END
  END GetFR;

  PROCEDURE Put*(VAR x, y: Item);
    VAR i: LONGINT;
  BEGIN
    IF (x.mode = OSAB.Const) & (x.a < 3FH) THEN
      OSAS.Mark("wrong memory address") 
    ELSIF (y.mode = OSAB.Const) & ((y.a >=128) OR (y.a < -128)) THEN
      OSAS.Mark("constant is out of the scope");
    ELSIF x.mode = OSAB.Const THEN
      load(y);
      PutAVR17(LDS+10H, y.r, x.a);
      RelReg(y.r, y.type.size);
    ELSE
      load(x); load(y);
      IF x.r MOD 2 = 0 THEN
        PutAVR14(MOVW, 26, x.r)
      ELSE
        PutAVR0(23H, 26, x.r); PutAVR0(23H, 27, x.r+1)
      END;
      i := 0;
      WHILE i < y.type.size DO
        PutAVR7(91DH, y.r+i);
        INC(i)
      END
    END
  END Put;
  
  PROCEDURE PutFR*(VAR x, y: Item);
    VAR  i: LONGINT; m: INTEGER;
  BEGIN
    m := y.mode; y.mode := OSAB.Par;
    IF (x.mode = OSAB.Const) & (x.a < 3FH) THEN
      OSAS.Mark("wrong memory address") 
    ELSIF (y.mode = OSAB.Const) & ((y.a >=128) OR (y.a < -128)) THEN
      OSAS.Mark("constant is out of the scope");
    ELSIF x.mode = OSAB.Const THEN
      loadRom(y);
      PutAVR17(LDS+10H, y.r, x.a);
      RelReg(y.r, y.type.size);
    ELSE
      load(x); loadRom(y);
      IF x.r MOD 2 = 0 THEN
        PutAVR14(MOVW, 26, x.r)
      ELSE
        PutAVR0(23H, 26, x.r); PutAVR0(23H, 27, x.r+1)
      END;
      i := 0;
      WHILE i < x.type.size DO
        PutAVR7(91DH, y.r+i);
        INC(i)
      END;
      RelReg(x.r, x.type.size); RelReg(y.r, y.type.size)
    END;
    y.mode := m
  END PutFR;
  
  PROCEDURE SEI*;
  BEGIN
    PutCode(9478H)
  END SEI;
  
  PROCEDURE PortIn*(VAR x, y: Item); 
  VAR
    r: LONGINT;
  BEGIN
    IF (x.a < 0) OR (x.a > 3FH) THEN
      OSAS.Mark("wrong port address!")
    ELSE
      GetReg(r, 2, 2);
      PutAVR8(INCMD, r, x.a);
      PutAVR1(LDI, r+1, 0);
      x.r := r; x.mode := OSAB.Reg;
      Store(y, x);
      RelReg(x.r, 2)
    END
  END PortIn;
  
  PROCEDURE PortOut*(VAR x, y: Item);
  
  BEGIN
    IF (x.a < 0) OR (x.a > 3FH) THEN
      OSAS.Mark("wrong port address")
    ELSE
      load(y);

      PutAVR8(INCMD+1, y.r, x.a);
      RelReg(y.r, y.type.size);
    END
  END PortOut;

  PROCEDURE CLI*;  (*clear interrupt flag*)
  BEGIN
    PutCode(94F8H)
  END CLI;

  PROCEDURE CPR*(op: LONGINT; VAR cpno, cpreg, x: Item);  (*Coprocessor Register*)
    
  BEGIN
    
  END CPR;

  PROCEDURE Nop*;  (*emit nop instruction*)
  BEGIN 
    PutCode(0)
  END Nop;

  PROCEDURE AddC*(VAR x, y, z: Item); (*add with carry; x := y+z*)
  VAR
    temp: Item;
  BEGIN 
    temp := x;
    load(x); load(y); load(z);
    PutAVR14(MOVW, x.r, y.r);
    PutAVR0(13H, x.r, z.r);
    PutAVR0(13H, x.r+1, z.r+1);
    Store(temp, x);
    RelReg(x.r, 2); RelReg(y.r, 2); RelReg(z.r, 2);
  END AddC;

  PROCEDURE MulD*(VAR x, y, z: Item);
  BEGIN 
  END MulD;

  (*In-line code functions*)

  PROCEDURE Abs*(VAR x: Item);
    VAR r: LONGINT;
  BEGIN
    IF x.type.form = OSAB.Int THEN
      load(x); r := x.r;
      PutAVR6(20H, r+1); (*TST r+1*)
      PutAVR11(0F14H, 3); (*BRGE pc+4*)
      PutAVR7(LD+120H, r+1); PutAVR7(LD+121H, r); PutAVR1(CPI+1H, r+1, 0FFH); 
      x.r := r
    ELSIF x.type.form = OSAB.Real THEN
    
    ELSE (*Set*)
    END
  END Abs;

  PROCEDURE Odd*(VAR x: Item);
  BEGIN 
    load(x); PutAVR1(ANDI, x.r, 1); RelReg(x.r, x.type.size); SetCC(x, BRNE)
  END Odd;

  PROCEDURE Floor*(VAR x: Item);
  BEGIN 
  END Floor;

  PROCEDURE Float*(VAR x: Item);
  BEGIN 
  END Float;

  PROCEDURE Ord*(VAR x: Item);
    VAR r, i, n: LONGINT;
  BEGIN
    IF x.mode IN {OSAB.Var, OSAB.Par, OSAB.RegI} THEN load(x); PutAVR6(CLR, x.r+1) END
  END Ord;

  PROCEDURE Len*(VAR x: Item);
  BEGIN
    
  END Len;

  PROCEDURE^ FJump*(VAR L: LONGINT);

  PROCEDURE Shift*(fct: LONGINT; VAR x, y: Item); (*x := x shift y*)
    VAR r,r1, i, l, l0, r2: LONGINT;
  BEGIN (*LSL, LSR, ASR, ROR : 0,1,2,3*) 
    load(x); load(y);
    GetReg1(r, x); GetReg1(r1, y);
    IF r # x.r THEN
      copyItem(r, x)
    END;
    IF r1 # y.r THEN
      copyItem(r1, y)
    END;
    GetReg(r2, 1, 4);
    PutAVR6(CLR, r2);
    PutAVR0(CP, r2, y.r);
    IF y.type.size = 2 THEN
      PutAVR0(CPC,r2, y.r+1)
    END;
    PutAVR11(BRLT, 1);
    l0 := 0;
    FJump(l0);
    RelReg(r2, 1);
    l := pc;
    IF fct = 0 THEN (*LSL*)
      PutAVR6(LSL, r);
      i := 1;
      WHILE i < x.type.size DO
        PutAVR6(LSL+10H, r+i); (*ROL*)
        INC(i)
      END
    ELSIF fct = 1 THEN (*LSR*)
      i := x.type.size-1;
      PutCode(9488H); (*CLC*)
      WHILE i >= 0 DO
        PutAVR7(927H, r+i); (*ROR*)
        DEC(i)
      END;
      (*PutAVR7(926H, r) (*LSR*)*)
    ELSIF fct = 2 THEN (*ASR*)
      i := x.type.size -1;
      PutAVR7(925H, r+i); (*ASR*)
      DEC(i);
      WHILE i >= 0 DO
        PutAVR7(927H, r+i); (*ROR*)
        DEC(i)
      END
    ELSIF fct = 3  THEN (*ROR*)
      PutCode(9408H); (*SEC*);
      i := x.type.size-1;
      WHILE i >= 0 DO
        PutAVR7(927H, r+i);
        DEC(i)
      END;
      (*PutAVR7(927H, r+x.type.size-1);*)
      PutAVR11(0F00H, 1); (*BRCS 1*)
      PutAVR1(7, r+x.type.size-1, 7FH); (*CBR 80H*)
    ELSE
      OSAS.Mark("compiler error 3!");
    END;
    PutAVR7(92AH, r1); (*DEC r1, so the number of shifts cannot exceed 255. Here I cannot use CPC, CPI, since it will affect C flag*)
    PutAVR11(0F11H, l-pc-1); (*BRNE*)
    x.r := r; 
    IF r1 # y.r THEN
      RelReg(r1, y.type.size)
    ELSE
      RelReg(y.r, y.type.size)
    END;
    FixLink(l0)
  END Shift;
  
  PROCEDURE copyItem(r: LONGINT; x: Item); (*copy the value of x into register r*)
    VAR i: LONGINT;
  BEGIN
    IF x.type.size = 2 THEN
      PutAVR14(MOVW, r, x.r)
    ELSE
      i := 0;
      WHILE i<x.type.size DO
        PutAVR0(23H, r+i, x.r+i);
        INC(i)
      END
    END
  END copyItem;
  
  PROCEDURE Adr*(VAR x: Item);
    VAR
      r: LONGINT;
  BEGIN
    IF x.mode IN {OSAB.Var, OSAB.Par, OSAB.RegI} THEN loadAdr(x)
    ELSIF (x.mode = OSAB.Const) & (x.type.form = OSAB.Proc) THEN load(x)
    ELSE OSAS.Mark("not addressable")
    END
  END Adr;
  
  PROCEDURE Bit*(VAR x, y: Item);
  BEGIN
  END Bit;

  PROCEDURE BitIO*(VAR x, y: Item);
  BEGIN
    IF y.mode = OSAB.Const THEN
      PutAVR5(CBI+3H, x.a, y.a); SetCC(x, 9BH)
    ELSE
      OSAS.Mark("Bit no. must be a constant")
    END
  END BitIO;

  PROCEDURE Xor*(VAR x, y: Item);
    
  BEGIN load(x);
    
  END Xor;

  PROCEDURE Overflow*(VAR x: Item);
  BEGIN (*x.mode = Const*)
    
  END Overflow;

  PROCEDURE Null*(VAR x: Item);
    
  BEGIN 
  END Null;

  PROCEDURE CheckRegs*;
  BEGIN
    IF regs # {} THEN
      OSAS.Mark("compiler error; reg stack not empty"); OIO.WriteSet(OSAS.f, regs); 
	OIO.FileUpdate(OSAS.f); regs := {}
    END
  END CheckRegs;

  (* Branches, procedure calls, procedure prolog and epilog *)
  
  PROCEDURE Here*(VAR L: LONGINT);
  BEGIN 
    L := pc
  END Here;
  
  PROCEDURE FJump*(VAR L: LONGINT);
  BEGIN 
    PutCode(RJMP*10000H+L); L := pc-1
  END FJump;

  PROCEDURE CFJump*(VAR x: Item);
  BEGIN
    IF x.mode # CC THEN loadCC(x) END;
    IF (x.r = 9BH) OR (x.r = 99H) OR (x.r = 0F60H) OR (x.r = 0F70H) THEN (*SBIS, SBIC, SBRS, SBRC*)
      PutCode(RJMP*10000H+x.a); FixLink(x.b); x.a := pc-1
    ELSE
      PutAVR11(x.r, 1);
      PutCode(RJMP*10000H+x.a); FixLink(x.b); x.a := pc-1
    END
  END CFJump;
  
  PROCEDURE BJump*(L: LONGINT);
  BEGIN 
    PutAVR3(RJMP, L-pc-1)
  END BJump;
  
  PROCEDURE CBJump*(VAR x: Item; L: LONGINT);
  BEGIN
    IF x.mode # CC THEN loadCC(x) END;
    IF (x.r = 9BH) OR (x.r = 99H) OR (x.r = 0F60H) OR (x.r = 0F70H) THEN (*SBIS, SBIC, SBRS, SBRC*)
      PutAVR3(RJMP, L-pc-1)
    ELSIF (L-pc  >= -64) & (L-pc <= 63) THEN
      PutAVR11(negated(x.r), L-pc-1);
    ELSIF (L-pc >= -2048) & (L-pc <= 2048) THEN
      PutAVR11(x.r, 1);
      PutAVR3(RJMP, L-pc-1)
    ELSE
      OSAS.Mark("The conditional statement is too long!")
    END;
    FixLink(x.b); FixLinkWith(x.a, L);
  END CBJump;

  PROCEDURE Fixup*(VAR x: Item);
  BEGIN 
    FixLink(x.a)
  END Fixup;
  
    
  PROCEDURE SaveRegs*(VAR rs: SET);
    VAR i: INTEGER;
  BEGIN 
    rs := regs;
    IF regs # {} THEN
      i := 0;
      WHILE i <=31 DO
        IF i IN regs THEN
          PutAVR7(91FH, i)
        END;
        INC(i)
      END;
      regs := {} 
    END;
  END SaveRegs;

  PROCEDURE Call*(VAR x: Item; rs: SET);
    VAR  r, i: LONGINT;
  BEGIN 
    RL := 2;
    IF x.type.form = OSAB.Proc THEN
      IF x.mode = OSAB.Const THEN
        GlobExtRef(x);
        PutAVR18(CALL, x.a)
      ELSE (*load procedure variable*)
        load(x);
        GlobExtRef(x);
        PutAVR1(0EH,30,  0);
        PutAVR1(0EH,31,  0); (*LDI Z, 0*)
        PutAVR0(3H, 30, x.r);
        PutAVR0(13H, 31, x.r+1);
        PutCode(9509H) (*ICALL*)
      END
    END;
    IF x.type.base.form # OSAB.NoTyp THEN
      IF rs # {} THEN
        regs := rs;
        GetReg(r, x.type.size, 0);
        x.mode := OSAB.Reg;
        x.r := r;
        i := 0;
        WHILE i < x.type.size DO
          PutAVR0(23H, r+i, 26-x.type.size+i);
          INC(i)
        END;
        i := 31;
        WHILE i >= 0 DO (*restore register*)
          IF i IN rs THEN
            PutAVR7(90FH, i)
          END;
          DEC(i)
        END
      ELSE
        i := 0; regs :={};
        WHILE i < x.type.size DO
          INCL(regs, 25-i);
          INC(i)
        END;
        r := 25-i+1;
      END;
      x.mode := OSAB.Reg; x.r := r    
    ELSE regs := {}
    END
  END Call;
  
  PROCEDURE ProcReturn*(VAR x: Item);
    VAR
      i: LONGINT;
  BEGIN
    IF (x.r # FP) & (x.r+x.type.size-1 # 25) THEN
      i := 0;
      WHILE i < x.type.size DO
        PutAVR0(23H, 26-x.type.size+i, x.r+i);
        INC(i)
      END;          
      RelReg(x.r, x.type.size)
    END;
  END ProcReturn;

  PROCEDURE Header*;
    
  BEGIN 
    entry := pc;
    PutAVR7(LD+11FH, 2);
    PutAVR7(LD+11FH, 3);
    PutCode(0B7CDH); (*IN r28, 0x3D*)
    PutCode(0B7DEH); (*IN r29, 0x3E*)
  END Header;
  
  PROCEDURE Enter*(leaf, int: BOOLEAN; level, regvarno: INTEGER; parsize, varsize: LONGINT);
    VAR n, i, nofregs, size, a: LONGINT; parObj: OSAB.Object;
  BEGIN 
    curlev := level;
    RH := 25; nofregs := parsize+regvarno;
    IF ~leaf THEN 
      IF int THEN 
        TXI[xrefi].ref := OSAS.ival; TXI[xrefi].adr := pc; INC(xrefi);
        PutAVR7(91FH, 1); (*push R1*)
        PutAVR7(91FH, 0);
        FOR i := 4 TO 27 DO
          PutAVR7(91FH, i)
        END;
        PutAVR7(91FH, 30); PutAVR7(91FH, 31);
        PutAVR8(0B0H, 0, 3FH);
        PutAVR7(91FH, 0);
      END;
      (*PROCEDURE prolog*)(*save previous FP, Calculate block size, save parameters*)
      PutAVR7(LD+11FH, 28); (*push sp*)
      PutAVR7(LD+11FH, 29);
      PutAVR7(LD+11FH, 2); (*push fp*)
      PutAVR7(LD+11FH, 3);
      PutCode(0B7CDH); (*IN r28, 0x3D*)
      PutCode(0B7DEH); (*IN r29, 0x3E*)
      PutAVR14(MOVW, 2, 28);
      PutAVR1(5H, 28, (parsize+varsize) MOD 100H);
      PutAVR1(4H, 29, (parsize+varsize) DIV 100H MOD 100H);
      PutCode(0BFDEH); (*OUT 0x3E, r29*)
      PutCode(0BFCDH); (*OUT 0x3D, r28*)
      (*Simulate stack to allocate registers to parameters*)
      IF parsize > 0 THEN
        parObj := OSAB.topScope.next;
        size := 0;
        REPEAT
          IF (parObj.type.form = OSAB.Array) & (parObj.type.len < 0) THEN (*open array*)
            nofregs := 4
          ELSIF parObj.class = OSAB.Par THEN
            nofregs := 2
          ELSE
            nofregs := parObj.type.size
          END;
          size := size + nofregs;
          n := RH-size+1;
          i := 1;
          a := parObj.val;
          IF nofregs = 4 THEN (*open array parameters*)
            PutAVR9(LDD+11H, n+2, a); PutAVR9(LDD+11H, n+3, a+1); 
            PutAVR9(LDD+11H, n, a+2); PutAVR9(LDD+11H, n+1, a+3)
          ELSE
            WHILE i <= nofregs DO
              PutAVR9(LDD+11H, n, a);
              INC(n); INC(a);
              INC(i)
            END
          END;
          parObj := parObj.next
        UNTIL (size >= parsize) OR (parObj = OSAB.guard)
      END        
    ELSE (*leaf procedure*)  
        RH := RH - nofregs      
    END
  END Enter;
  
  PROCEDURE Return*(leaf, int: BOOLEAN; offset, resreg, form: INTEGER; VAR x: Item);
    VAR res: Item; i: INTEGER;
  BEGIN 
    resreg := FP-resreg;
    IF (form # OSAB.NoTyp) & ~((x.mode = OSAB.Reg) & (x.r = resreg)) THEN
      res.mode := OSAB.Reg; res.r := resreg; res.type := x.type; Store(res, x)
    END;
    
    IF ~leaf THEN
      PutAVR8(INCMD+1H, 2, 3DH); (*sp := fp*)
      PutAVR8(INCMD+1H, 3, 3EH);
      PutAVR7(LD+10FH, 3);
      PutAVR7(LD+10FH, 2);
      PutAVR7(LD+10FH, 29);
      PutAVR7(LD+10FH, 28)
    END;
    IF ~int THEN (*procedure epilog*)
      PutCode(9508H)
    ELSE (*interrupt proc*)
      PutAVR7(90FH, 0); (*pop R0*)
      PutAVR8(0B1H, 0, 3FH);
      PutAVR7(90FH, 31); PutAVR7(90FH, 30);
      FOR i := 27 TO 4 BY -1 DO
        PutAVR7(90FH, i)
      END;
      PutAVR7(90FH, 0);
      PutAVR7(90FH, 1);
      PutCode(9518H)
    END;
    RL := 2; RH := 25; regs := {}
  END Return;

(*------------------------- my functions ------------------------*)
 PROCEDURE MoveConstToReg *(const :ARRAY OF CHAR; reg :INTEGER);
	VAR regName: ARRAY 5 OF CHAR;
	BEGIN
		Gen.Mov(f);
		OIO.FWriteString(f, "$");
		OIO.FWriteString(f, const);
		OIO.FWriteString(f, ", ");
		Gen.RegName(reg, regName);
		OIO.FWriteString(f,regName);
		OIO.FWriteLn(f);
		
	END MoveConstToReg;
 
 PROCEDURE Interupt * (com :INTEGER);
	VAR comName: ARRAY 10 OF CHAR;
	BEGIN
		Gen.Int(f);
		OIO.FWriteString(f, "$");
		Gen.CommandName(com, comName);
		OIO.FWriteString(f, comName);
		OIO.FWriteLn(f);
	END Interupt;

 PROCEDURE Exit *(retVal:INTEGER);
	BEGIN
		MoveConstToReg("1", rax);
		MoveConstToReg("0", rbx);
		Interupt(exit);	
	END Exit;

(*------------------------- my functions end ---------------------- *)
 
  (*PROCEDURE Open*;
    VAR i: INTEGER;
  BEGIN 
    curlev := 0; pc := 0; scx := 0; xrefx := 0; xrefi := 0; firstfixloc := 0; ifixup := 0; fixlistFP := 0; fixlistMU := 0; fixlistConst := 0; constSize := 0;
    xreft := 0;
    FOR i := 0 TO maximp-1 DO fixlist[i] := 0; tdfixlist[i] := 0 END;
    RL := 4; RH := 25; regs := {}; assertLabel := 0
  END Open;*)

PROCEDURE GloblStart *(VAR f: OIO.File);
        BEGIN
                OIO.FWriteString(f, ".section .data");
                OIO.FWriteLn(f);
                OIO.FWriteString(f, ".section .text");
                OIO.FWriteLn(f);
                OIO.FWriteString(f, ".globl _start");
                OIO.FWriteLn(f);
                OIO.FWriteString(f, "_start:");
                OIO.FWriteLn(f);
                        
        END GloblStart;


PROCEDURE Open * (modid: OSAS.Ident);
	VAR	name: OSAS.Ident;
		is_opened: BOOLEAN;
	BEGIN 
		(* initialisation /add later/ *)
		OSAB.MakeFileName(name, modid, ".s");
		is_opened:= OIO.OpenNewFile(name, f);
		(*OIO.FWriteString(f, "brrrrrrrrrrrr");*)
		(*iOIO.WriteString(name);OIO.WriteLn();*)		
	END Open;

PROCEDURE Close *();
	VAR	name: OSAS.Ident;
		is_opened: BOOLEAN;
	BEGIN		
		IF(is_main=TRUE) THEN
			 GloblStart(f);
		END;
		Exit(0);
		OIO.CloseFile(f);
	END Close;



(*PROCEDURE Close*(VAR modid: OSAS.Ident; key, datasize: LONGINT);    
	VAR obj: OSAB.Object; i, nofentries: INTEGER;
	name: OSAS.Ident;
	(*tmp :SHORTINT;*)
	is_opened: BOOLEAN;
	F: OIO.BinFile; (*R: AosFS.Writer;*)
	h: HUGEINT;
      
	BEGIN 
	(*Generate epilog for the module under compilation*)
		PutAVR7(LD+10FH, 3);    
		PutAVR7(LD+10FH, 2);    
		PutCode(9508H);    
		FixupConstants;    
		OSAB.MakeFileName(modid, name, ".avr"); (*write code file*)     
		is_opened := OIO.OpenNewBinFile(name, F); (*AosFS.OpenWriter(R, F, 0);*)    
		OIO.WriteBinString(F, modid);
		OIO.WriteBinLInt(F, key);
		OIO.WriteBinLInt(F, SHORT(fixlistConst));(*string constant fixlist header*)     
		OIO.WriteBinLInt(F, SHORT(fixlist[0])); (*self fixlist*)    
		OIO.WriteBinLInt(F, SHORT(tdfixlist[0])); (*self type descriptor fixlist*)
		OIO.WriteBinLInt(F, SHORT(ifixup)); (*interrupt handler fixup header*)
    		obj := OSAB.topScope.next;
		WHILE obj.class = OSAB.Mod DO (*list of imported modules*)
			IF obj.name # "SYSTEM" THEN
		        	OIO.WriteBinString(F, obj(OSAB.Module).name1); 
				OIO.WriteBinLInt(F, obj.val); 
		        	OIO.WriteBinLInt(F, SHORT(fixlist[obj.lev])); 
				OIO.WriteBinLInt(F, SHORT(tdfixlist[obj.lev]))
			END;      
			obj := obj.next    
		END;   
		IF fixlistFP > 0 THEN      
			OIO.WriteBinString(F, "FPU");
			h := FPUkey; 
			OIO.WriteBinLInt(F, SHORT(h)); 
			OIO.WriteBinLInt(F, SHORT(fixlistFP));  
			OIO.WriteBinLInt(F, 0)(*No records in FPU*)    
		END;    
		IF fixlistMU > 0 THEN      
			OIO.WriteBinString(F, "MAU"); 
			OIO.WriteBinLInt(F, MAUkey); 
			OIO.WriteBinLInt(F, SHORT(fixlistMU)); 
			OIO.WriteBinLInt(F, 0) (*No records in MAU*)    
		END;    
		OIO.WriteBinSInt(F, 0);    
		obj := OSAB.topScope.next; 
		nofentries := 0;    
		WHILE obj # OSAB.guard DO (*list of commands*)      
			IF obj.expo THEN        
				IF (obj.class = OSAB.Const) & (obj.type.form = OSAB.Proc) & (obj.type.nofpar = 0  ) & (obj.type.base = OSAB.noType) THEN
         				OIO.WriteBinString(F, obj.name); 
					OIO.WriteBinLInt(F, SHORT(obj.val))        
				END;        
				IF (obj.class = OSAB.Const) & (obj.type.form = OSAB.Proc) OR (obj.class = OSAB.Typ) OR (obj.class = OSAB.Var) THEN
					INC(nofentries)
				END 
			END;      
			obj := obj.next    
		END;    
		OIO.WriteBinSInt(F, 0);    
		OIO.WriteBinLInt(F, nofentries); 
		OIO.WriteBinLInt(F, entry);    
		obj := OSAB.topScope.next;
		WHILE obj # OSAB.guard DO (* list of exported procedures (entry points), types  and variables *)
			IF obj.expo THEN        
				IF (obj.class = OSAB.Const) & (obj.type.form = OSAB.Proc) OR (obj.class = OSAB.Typ) OR (obj.class = OSAB.Var) THEN 
					OIO.WriteBinLInt(F, SHORT(obj.val))        
				END      
			END;      
			obj := obj.next    
		END;     
		OIO.WriteBinLInt(F, datasize); 
		OIO.WriteBinLInt(F, constSize); 
		OIO.WriteBinLInt(F, pc-codelen); 
		OIO.WriteBinLInt(F, pc);    
		i := 0;    
		WHILE i < pc DO OIO.WriteBinLInt(F, SHORT(codeAVR[i])); INC(i) END;    
		OIO.BinFileUpdate(F);    
		OIO.Register(F)  
	END Close;
*)
  
  PROCEDURE Init*(bkpc: HUGEINT);
  BEGIN
    
  END Init;
  
    
BEGIN
  cond[0] := BRLO+01H; (*BREQ*)
  cond[1] := BRLO+11H; (*BRNE*)
  cond[2] := BRLO+04H; (*BRLT*)
  cond[3] := 0; cond[4] := 0; cond[5] := BRLO+14H
END OAVG0.
