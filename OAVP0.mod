MODULE OAVP0; (*N. Wirth 1.7.97 / 2.5.99 / 28.5.2007  Oberon for Strong Arm*)
  IMPORT OSAB := OAVB0, OSAS := OAVS0, OSAG := OAVG0, OIO;(*, AosIO:=Streams, AosOut:=KernelLog;*)
  (*Parser of Oberon-SA compiler. Uses Scanner OSAS to obtain symbols (tokens) and
    generator OSAG to produce code. Performs type checking and data allocation.
    Parser is target-independent, except for part of the handling of declarations.*)
  (* fof replaced 
    Texts.WriteInt(W, => OSAS.W.Int(
    Texts.WriteLn(W => OSAS.W.Ln(
    Texts.WriteString(W, => OSAS.W.String(
    Texts.Append(Oberon.Log, W.buf) => OSAS.W.Update()
  *) 
  TYPE UndefRec = POINTER TO UndefRecDesc;
	UndefRecDesc = RECORD
      name: OSAS.Ident; type: OSAB.Type; next: UndefRec
    END ;
    
  VAR sym: INTEGER;   (*last symbol read*)
    dc: LONGINT;    (*data counter*)
    level: INTEGER;
    newSF, check, leaf: BOOLEAN; 
    undefRec: UndefRec;   (*undefined record types*)
    
  PROCEDURE Check(s: INTEGER; msg: ARRAY OF CHAR);
  BEGIN
    IF sym = s THEN OSAS.Get(sym) ELSE OSAS.Mark(msg) END
  END Check;

  PROCEDURE qualident(VAR obj: OSAB.Object);
  BEGIN obj := OSAB.this();  OSAS.Get(sym); 
    IF (sym = OSAS.period) & (obj.class = OSAB.Mod) THEN
      OSAS.Get(sym);
      IF sym = OSAS.ident THEN obj := OSAB.thisimport(obj); OSAS.Get(sym)
      ELSE OSAS.Mark("identifier expected"); obj := OSAB.guard
      END
    END
  END qualident;

  PROCEDURE CheckBool(VAR x: OSAG.Item);
  BEGIN
    IF x.type.form # OSAB.Bool THEN OSAS.Mark("not Boolean"); x.type := OSAB.boolType END
  END CheckBool;

  PROCEDURE CheckInt(VAR x: OSAG.Item);
  BEGIN
    IF x.type.form # OSAB.Int THEN OSAS.Mark("not Integer"); x.type := OSAB.intType END
  END CheckInt;

  PROCEDURE CheckReal(VAR x: OSAG.Item);
  BEGIN
    IF x.type.form # OSAB.Real THEN OSAS.Mark("not Real"); x.type := OSAB.realType END
  END CheckReal;

  PROCEDURE CheckSet(VAR x: OSAG.Item);
  BEGIN
    IF x.type.form # OSAB.Set THEN OSAS.Mark("not Set"); x.type := OSAB.setType END 
  END CheckSet;

  PROCEDURE CheckSetVal(VAR x: OSAG.Item);
  BEGIN
    IF x.type.form # OSAB.Int THEN OSAS.Mark("not Int"); x.type := OSAB.setType
    ELSIF x.mode = OSAB.Const THEN
      IF (OSAG.Val(x) < 0) OR (OSAG.Val(x) >= OSAG.WordSize*8) THEN OSAS.Mark("invalid set") END
    END 
  END CheckSetVal;

  PROCEDURE CheckConst(VAR x: OSAG.Item);
  BEGIN
    IF x.mode # OSAB.Const THEN OSAS.Mark("not a constant"); x.mode := OSAB.Const END
  END CheckConst;

  PROCEDURE CheckReadOnly(VAR x: OSAG.Item);
  BEGIN
    IF x.rdo THEN OSAS.Mark("read-only variable") END
  END CheckReadOnly;

  PROCEDURE CheckLeaf;
  BEGIN
    IF leaf THEN OSAS.Mark("not in leaf procedure!") END
  END CheckLeaf;

  PROCEDURE IsExtension(t0, t1: OSAB.Type): BOOLEAN;
  BEGIN (*t1 is an extension of t0*)
    RETURN (t0 = t1) OR (t1 # NIL) & IsExtension(t0, t1.base)
  END IsExtension;
  
  (* expressions *)

  PROCEDURE^ expression(VAR x: OSAG.Item);
  
  PROCEDURE TypeTest(VAR x: OSAG.Item; T: OSAB.Type; guard: INTEGER);
    (*guard = 0: type test; guard = 1: local guard; guard = 2: regional guard*)
    VAR xt: OSAB.Type;
  BEGIN xt := x.type;
    WHILE (xt # T) & (xt # NIL) DO xt := xt.base END ;
    IF xt # T THEN xt := x.type;
      IF (xt.form = OSAB.Pointer) & (T.form = OSAB.Pointer) THEN
        IF IsExtension(xt.base, T.base) THEN OSAG.TypeTest(x, T.base, 0, guard); x.type := T
        ELSE OSAS.Mark("not an extension")
        END
      ELSIF (xt.form = OSAB.Record) & (T.form = OSAB.Record) & (x.mode = OSAB.Par) THEN
        IF IsExtension(xt, T) THEN  OSAG.TypeTest(x, T, 1, guard); x.type := T
        ELSE OSAS.Mark("not an extension")
        END
      ELSE OSAS.Mark("incompatible types")
      END
    ELSIF guard = 0 THEN OSAG.MakeConstItem(x, OSAB.boolType, 1)
    END ;
    IF guard = 0 THEN x.type := OSAB.boolType END
  END TypeTest;

  PROCEDURE selector(VAR x: OSAG.Item);
    VAR y: OSAG.Item; obj: OSAB.Object;
  BEGIN
    LOOP
      IF sym = OSAS.lbrak THEN
        REPEAT OSAS.Get(sym); expression(y);
          IF x.type.form = OSAB.Array THEN CheckInt(y); OSAG.Index(x, y, ~leaf) 
          ELSE OSAS.Mark("not an array")
          END
        UNTIL sym # OSAS.comma;
        Check(OSAS.rbrak, "no ]")
      ELSIF sym = OSAS.period THEN
        OSAS.Get(sym);
        IF sym = OSAS.ident THEN
          IF x.type.form = OSAB.Pointer THEN OSAG.DeRef(x) END ;
          IF x.type.form = OSAB.Record THEN
            obj := OSAB.thisfield(x.type); OSAS.Get(sym);
            IF obj # NIL THEN OSAG.Field(x, obj) ELSE OSAS.Mark("field undefined") END
          ELSE OSAS.Mark("not a record")
          END
        ELSE OSAS.Mark("identifier expected")
        END
      ELSIF sym = OSAS.arrow THEN
        OSAS.Get(sym);
        IF x.type.form = OSAB.Pointer THEN OSAG.DeRef(x) ELSE OSAS.Mark("not a pointer") END
      ELSIF (sym = OSAS.lparen) & (x.mode # OSAB.SProc) & (x.type.form IN {OSAB.Record, OSAB.Pointer}) THEN
        OSAS.Get(sym);
        IF sym = OSAS.ident THEN
          qualident(obj);
          IF obj.class = OSAB.Typ THEN TypeTest(x, obj.type, 1)
          ELSE OSAS.Mark("not a type")
          END
        ELSE OSAS.Mark("not an identifier")
        END ;
        Check(OSAS.rparen, " ) missing")
      ELSE EXIT
      END
    END
  END selector;

  PROCEDURE CompTypes(t0, t1: OSAB.Type): BOOLEAN;

    PROCEDURE EqualSignatures(t0, t1: OSAB.Type): BOOLEAN;
      VAR p0, p1: OSAB.Object; com: BOOLEAN;
    BEGIN com := TRUE;
      IF (t0.base = t1.base) & (t0.nofpar = t1.nofpar) THEN
        p0 := t0.dsc; p1 := t1.dsc;
        WHILE p0 # OSAB.guard DO
          IF ((p0.class = p1.class) OR (p0.class = OSAB.Var) & (p1.class = OSAB.Reg)
              OR (p0.class = OSAB.Par) & (p1.class = OSAB.RegI))
              & CompTypes(p0.type, p1.type) & (p0.rdo = p1.rdo) THEN
            p0 := p0.next; p1 := p1.next
          ELSE p0 := OSAB.guard; com := FALSE
          END
        END
      ELSE com := FALSE
      END ;
      RETURN com
    END EqualSignatures;
    
  BEGIN (*Compatible Types*)
    RETURN (t0 = t1)
      OR (t0.form = OSAB.Array) & (t1.form = OSAB.Array) & CompTypes(t0.base, t1.base)
      OR (t0.form = OSAB.Pointer) & (t1.form = OSAB.Pointer) & IsExtension(t0.base, t1.base)
      OR (t0.form = OSAB.Record) & (t1.form = OSAB.Record) & IsExtension(t0, t1)
      OR (t0.form = OSAB.Proc) & (t1.form = OSAB.Proc) & EqualSignatures(t0, t1)
      OR (t0.form IN {OSAB.Pointer, OSAB.Proc}) & (t1.form = OSAB.NilTyp)
      OR (t0.form = OSAB.NilTyp) & (t1.form IN {OSAB.Pointer, OSAB.Proc})
      OR (t0.form = OSAB.Byte) & (t1.form = OSAB.Char)
  END CompTypes;

  PROCEDURE Parameter(par: OSAB.Object);
    VAR x: OSAG.Item;
  BEGIN expression(x); 
    IF CompTypes(par.type, x.type) THEN
      IF par.class IN {OSAB.Par, OSAB.RegI} THEN
        IF ~par.rdo THEN CheckReadOnly(x) END ;
        OSAG.VarParam(x, par.type)
      ELSIF x.type.form <= OSAB.Proc THEN OSAG.ValueParam(x)
      ELSE OSAS.Mark("illegal value parameter")
      END
    ELSIF (x.type.form = OSAB.String) & (par.class = OSAB.Par) & (par.type.form = OSAB.Array) & 
        (par.type.base.form = OSAB.Char) & (par.type.len < 0) THEN 
      IF par.rdo THEN OSAG.StringParam(x) ELSE OSAS.Mark("not a constant param") END
    ELSIF (par.type.form = OSAB.Array) & (par.type.base.form = OSAB.Byte) THEN
      IF ~par.rdo & x.rdo THEN OSAS.Mark("read-only") ELSE OSAG.ByteParam(x) END
    ELSE OSAS.Mark("incompatible types")
    END
  END Parameter;

  PROCEDURE ParamList(VAR x: OSAG.Item);
    VAR n: INTEGER; par: OSAB.Object;
  BEGIN par := x.type.dsc; n := 0;
    IF sym # OSAS.rparen THEN
      IF x.mode > OSAB.Par THEN OSAS.Mark("cannot compile") END ;
      Parameter(par); n := 1;
      WHILE sym <= OSAS.comma DO
        Check(sym, "comma?");
        IF par # NIL THEN par := par.next END ;
        INC(n); Parameter(par)
      END ;
      Check(OSAS.rparen, ") missing")
    ELSE OSAS.Get(sym);
    END ;
    IF n < x.type.nofpar THEN OSAS.Mark("too few parameters")
    ELSIF n > x.type.nofpar THEN OSAS.Mark("too many parameters")
    END
  END ParamList;

  PROCEDURE StandFunc(VAR x: OSAG.Item; fct: LONGINT);
    VAR y: OSAG.Item; n, n0: LONGINT;  (*in-line code functions*)
  BEGIN 
    n0 := fct MOD 4; fct := fct DIV 4; expression(x); n := 1;
    WHILE sym = OSAS.comma DO OSAS.Get(sym); expression(y); INC(n) END ;
    Check(OSAS.rparen, "no )");
    IF n = n0 THEN
      CASE fct-20 OF
           0: (*LEN*)
          IF x.type.form = OSAB.Array THEN OSAG.Len(x); x.type := OSAB.intType
          ELSE OSAS.Mark("not an array")
          END
       | 1: IF x.type.form IN {OSAB.Int, OSAB.Real, OSAB.Set} THEN OSAG.Abs(x) ELSE OSAS.Mark("bad type") END
       | 2: CheckInt(x); OSAG.Odd(x); x.type := OSAB.boolType
       | 3: CheckReal(x); OSAG.Floor(x); x.type := OSAB.intType
       | 4: CheckInt(x); OSAG.Float(x); x.type := OSAB.realType
       | 5: (*ORD*) IF x.type.form = OSAB.Char THEN x.type := OSAB.intType; OSAG.Ord(x) ELSE OSAS.Mark("bad type") END ;
          
       | 6: (*CHR*) IF (x.type.form # OSAB.Int) & (x.type.form # OSAB.Byte) THEN OSAS.Mark("bad type") END; 
               x.type := OSAB.charType
       | 7: (*BITIO*) CheckInt(x); CheckInt(y); OSAG.BitIO(x, y); x.type := OSAB.boolType
       | 8 .. 11: (*LSL, LSR, ASR, ROR*) CheckInt(x); CheckInt(y); OSAG.Shift(fct-28, x, y)
       | 12: (*BIT*) CheckInt(x); CheckInt(y); OSAG.Bit(x, y) ; x.type := OSAB.boolType
       | 13: (*VAL*) IF (x.type.form <= OSAB.Set) & (y.type.form <= OSAB.Pointer) THEN y.type := x.type; x := y
           ELSE OSAS.Mark("casting not allowed")
           END
       | 14: (*ADR*) OSAG.Adr(x); x.type := OSAB.intType
       | 15: (*SIZE*) IF x.mode = OSAB.Typ THEN OSAG.MakeConstItem(x, OSAB.intType, x.type.size)
           ELSE OSAS.Mark("must be a type")
           END
       | 16: (*NULL*) CheckInt(x); OSAG.Null(x); x.type := OSAB.boolType
       | 17: (*OVFL*) CheckInt(x); CheckConst(x); OSAG.Overflow(x); x.type := OSAB.boolType
       | 18: (*XOR*) OSAG.Xor(x, y)
      ELSE OSAS.Mark("compiler error 1")
      END
    ELSE OSAS.Mark("wrong nof params")
    END
  END StandFunc;

  PROCEDURE element(VAR x: OSAG.Item);
    VAR y, z: OSAG.Item; i: LONGINT;
  BEGIN expression(y); CheckSetVal(y);
    IF sym = OSAS.upto THEN
      OSAS.Get(sym); expression(z); CheckSetVal(z); OSAG.Set(x, y, z)
    ELSE OSAG.Singleton(x, y)
    END ;
    x.type := OSAB.setType
  END element;
    
  PROCEDURE set(VAR x: OSAG.Item);
    VAR y: OSAG.Item;
  BEGIN
    IF (sym >= OSAS.rparen) & (sym <= OSAS.rbrace) OR (sym >= OSAS.semicolon) THEN
      IF sym # OSAS.rbrace THEN OSAS.Mark(" } missing") END ;
      OSAG.MakeConstItem(x, OSAB.setType, 0) (*empty set*)
    ELSE element(x);
      WHILE (sym < OSAS.rparen) OR (sym > OSAS.rbrace) DO
        IF sym = OSAS.comma THEN OSAS.Get(sym)
        ELSIF sym # OSAS.rbrace THEN OSAS.Mark("missing comma")
        END ;
        element(y); OSAG.SetOp(OSAS.plus, x, y)
      END
    END
  END set; 
  
  PROCEDURE factor(VAR x: OSAG.Item);
    VAR obj: OSAB.Object; rset: SET;
  BEGIN (*sync*)
    IF (sym < OSAS.char) OR (sym > OSAS.ident) THEN OSAS.Mark("expression expected");
      REPEAT OSAS.Get(sym) UNTIL (sym >= OSAS.char) & (sym <= OSAS.ident)
    END ;
    IF sym = OSAS.ident THEN
      qualident(obj);
      IF obj.class = OSAB.SProc THEN
        IF sym = OSAS.lparen THEN OSAS.Get(sym); StandFunc(x, obj.val)
        ELSE OSAS.Mark("param missing"); OSAG.MakeConstItem(x, OSAB.intType, 0)
        END
      ELSE OSAG.MakeItem(x, obj); selector(x);
        IF (sym = OSAS.lparen) & (x.type.form = OSAB.Proc) THEN
          IF x.type.base.form = OSAB.NoTyp THEN OSAS.Mark("not a function") END ;
          CheckLeaf; OSAS.Get(sym); OSAG.SaveRegs(rset); ParamList(x);
          OSAG.Call(x, rset); x.type := x.type.base
        END
      END
    ELSIF sym = OSAS.int THEN OSAG.MakeConstItem(x, OSAB.intType, OSAS.ival); OSAS.Get(sym)
    ELSIF sym = OSAS.real THEN OSAG.MakeRealItem(x, OSAS.rval); OSAS.Get(sym)
    ELSIF sym = OSAS.char THEN OSAG.MakeConstItem(x, OSAB.charType, OSAS.ival); OSAS.Get(sym)
    ELSIF sym = OSAS.nil THEN OSAS.Get(sym); OSAG.MakeConstItem(x, OSAB.nilType, 0)
    ELSIF sym = OSAS.string THEN OSAG.MakeStringItem(x, OSAS.ival, OSAS.slen); OSAS.Get(sym)
    ELSIF sym = OSAS.lparen THEN OSAS.Get(sym); expression(x); Check(OSAS.rparen, "no )")
    ELSIF sym = OSAS.lbrace THEN OSAS.Get(sym); set(x); Check(OSAS.rbrace, "no }")
    ELSIF sym = OSAS.not THEN OSAS.Get(sym); factor(x); CheckBool(x); OSAG.Not(x)
    ELSIF sym = OSAS.false THEN OSAS.Get(sym); OSAG.MakeConstItem(x, OSAB.boolType, 0)
    ELSIF sym = OSAS.true THEN OSAS.Get(sym); OSAG.MakeConstItem(x, OSAB.boolType, 1)
    ELSE OSAS.Mark("not a factor"); OSAG.MakeItem(x, OSAB.guard)
    END
  END factor;
  
  PROCEDURE term(VAR x: OSAG.Item);
    VAR y: OSAG.Item; op: INTEGER;
  BEGIN factor(x);
    WHILE (sym >= OSAS.times) & (sym <= OSAS.and) DO
      op := sym; OSAS.Get(sym);
      IF op = OSAS.and THEN CheckBool(x); OSAG.And1(x); factor(y); CheckBool(y); OSAG.And2(x, y)
      ELSIF x.type.form = OSAB.Int THEN
        factor(y); CheckInt(y); OSAG.MulOp(op, x, y)
      ELSIF x.type.form = OSAB.Real THEN
        IF op = OSAS.rdiv THEN OSAG.PrepOpd(x) END ;
        CheckLeaf; factor(y); CheckReal(y); OSAG.RealOp(op, x, y)
      ELSE CheckSet(x); factor(y); CheckSet(y); OSAG.SetOp(op, x, y)
      END
    END
  END term;
  
  PROCEDURE SimpleExpression(VAR x: OSAG.Item);
    VAR y: OSAG.Item; op: INTEGER;
  BEGIN
    IF sym = OSAS.minus THEN OSAS.Get(sym); term(x);
      IF x.type.form IN {OSAB.Int, OSAB.Real, OSAB.Set} THEN OSAG.Neg(x) ELSE CheckInt(x) END
    ELSIF sym = OSAS.plus THEN OSAS.Get(sym); term(x);
    ELSE term(x)
    END ;
    WHILE (sym >= OSAS.plus) & (sym <= OSAS.or) DO
      op := sym; OSAS.Get(sym);
      IF op = OSAS.or THEN
       OSAG.Or1(x); CheckBool(x); term(y); CheckBool(y); OSAG.Or2(x, y)
      ELSIF x.type.form = OSAB.Int THEN term(y); CheckInt(y); OSAG.AddOp(op, x, y)
      ELSIF x.type.form = OSAB.Real THEN CheckLeaf; term(y); CheckReal(y); OSAG.RealOp(op, x, y)
      ELSE CheckSet(x); term(y); CheckSet(y); OSAG.SetOp(op, x, y)
      END
    END
  END SimpleExpression;

  PROCEDURE expression(VAR x: OSAG.Item);
    VAR y: OSAG.Item; obj: OSAB.Object; rel, xf, yf: INTEGER;
  BEGIN SimpleExpression(x);
    IF (sym >= OSAS.eql) & (sym <= OSAS.geq) THEN
      rel := sym; OSAS.Get(sym); SimpleExpression(y); xf := x.type.form; yf := y.type.form;
      IF CompTypes(x.type, y.type) THEN
        IF (xf IN {OSAB.Char, OSAB.Int}) THEN OSAG.IntRelation(rel, x, y)
        ELSIF xf = OSAB.Real THEN CheckLeaf; OSAG.RealRelation(rel, x, y)
        ELSIF xf = OSAB.Set THEN OSAG.SetRelation(rel, x, y)
        ELSIF (xf IN {OSAB.Bool, OSAB.Pointer, OSAB.Proc, OSAB.NilTyp}) THEN
          IF rel <= OSAS.neq THEN OSAG.IntRelation(rel, x, y) ELSE OSAS.Mark("only = or #") END
        ELSIF (xf = OSAB.Array) & (x.type.base.form = OSAB.Char) OR (xf = OSAB.String) THEN
          OSAG.StringRelation(rel, x, y)
        ELSE OSAS.Mark("illegal comparison")
        END
      ELSIF (xf = OSAB.Array) & (x.type.base.form = OSAB.Char) & (yf = OSAB.String)
          OR (yf = OSAB.Array) & (y.type.base.form = OSAB.Char) & (xf = OSAB.String) THEN
        OSAG.StringRelation(rel, x, y)
      ELSE OSAS.Mark("illegal comparison")
      END ;
      x.type := OSAB.boolType
    ELSIF sym = OSAS.in THEN
      OSAS.Get(sym); SimpleExpression(y);
      IF (x.type.form = OSAB.Int) & (y.type.form = OSAB.Set) THEN OSAG.In(x, y)
      ELSE OSAS.Mark("illegal operands of IN")
      END ;
      x.type := OSAB.boolType
    ELSIF sym = OSAS.is THEN
      OSAS.Get(sym); qualident(obj); TypeTest(x, obj.type, 0) ;
      x.type := OSAB.boolType
    END
  END expression;

  (* statements *)

  PROCEDURE StandProc(pno, nfp: LONGINT);
    VAR nap: LONGINT;
      x, y, z: OSAG.Item;
  BEGIN
    
     IF sym = OSAS.lparen THEN
      OSAS.Get(sym); expression(x);
      IF sym = OSAS.comma THEN
        OSAS.Get(sym); expression(y); nap := 2; z.type := OSAB.noType;
        WHILE sym = OSAS.comma DO OSAS.Get(sym); expression(z); INC(nap) END
      ELSE y.type := OSAB.noType; nap := 1
      END ;
      Check(OSAS.rparen, "no )");
    ELSE
      nap := 0;
    END;
    IF (nfp = nap) OR (pno < 4) THEN 
      CASE pno OF
         0, 1: CheckInt(x); CheckReadOnly(x);
             IF y.type # OSAB.noType THEN CheckInt(y); CheckConst(y) END ;
           OSAG.Increment(pno, x, y)
       | 2: CheckBool(x); OSAG.Not(x);
           OSAG.Assert(x)
       | 3: IF (x.type.form = OSAB.Pointer) THEN CheckReadOnly(x); OSAG.New(x, y)
        ELSE OSAS.Mark("not a pointer variable")
        END
       | 4: CheckReal(x); CheckInt(y); CheckReadOnly(x); (*OSAG.Pack(x, y)*)
       | 5: CheckReal(x); CheckInt(y); CheckReadOnly(x); (*OSAG.Unpk(x, y)*)
       | 6: CheckInt(x); OSAG.Get(x, y)
       | 7: CheckInt(x); OSAG.Put(x, y)
       | 8: OSAG.CLI;
       | 9: OSAG.SEI; (*SEI instruction*)
       | 10: CheckInt(x); CheckConst(x); OSAG.PortOut(x, y); 
       | 11: CheckInt(x); CheckConst(x); OSAG.PortIn(x, y);
       | 12: OSAG.Nop
       | 13: CheckInt(x); CheckInt(y); CheckInt(z);OSAG.AddC(x, y, z) (*restrict to INTEGER type!*)
       | 14: OSAG.MulD(x, y, z) (*no type checks!*)
       | 15: OSAG.PutFR(x, y);
       | 16: OSAG.GetFR(x, y);
      ELSE OSAS.Mark("compiler error 3")
      END
    ELSE OSAS.Mark("wrong nof parameters")
    END
  END StandProc;
  
  PROCEDURE CaseLabel(VAR k, n: LONGINT);
    VAR obj: OSAB.Object;
  BEGIN OSAS.Get(sym);
    IF sym = OSAS.int THEN k := OSAS.ival; OSAS.Get(sym)
    ELSIF sym = OSAS.ident THEN obj := OSAB.this(); OSAS.Get(sym);
      IF (obj.class = OSAB.Const) & (obj.type.form = OSAB.Int) THEN k := obj.val ELSE k := 0 END
    ELSE k := 0; OSAS.Mark("bad label")
    END ;
    IF (k < 0) OR (k >= n) THEN OSAS.Mark("label out of range"); k := 0 END
  END CaseLabel;

  PROCEDURE StatSequence;
    VAR obj, typ: OSAB.Object;
      oldtype: OSAB.Type;
      x, y, z, w: OSAG.Item;
      nc, k0, k, k1, inc, L0, L1: LONGINT;
  BEGIN (* StatSequence *)
    REPEAT (*sync*) obj := OSAB.guard;
      IF ~((sym = OSAS.ident) OR (sym >= OSAS.if) & (sym <= OSAS.assert) OR (sym >= OSAS.semicolon)) THEN
        OSAS.Mark("statement expected");
        REPEAT OSAS.Get(sym) UNTIL (sym = OSAS.ident) OR (sym >= OSAS.if)
      END ;
      IF sym = OSAS.ident THEN
        qualident(obj); OSAG.MakeItem(x, obj); selector(x);
        IF sym = OSAS.becomes THEN (*assignment*)
          OSAS.Get(sym); CheckReadOnly(x);
          OSAG.PrepStore(x); expression(y);
          IF (x.type.form <= OSAB.Pointer) OR (x.type.form = OSAB.Proc) THEN
            IF CompTypes(x.type, y.type)  THEN OSAG.Store(x, y)
            ELSE OSAS.Mark("incompatible assignment")
            END
          ELSIF CompTypes(x.type, y.type) THEN
            IF x.type.form = OSAB.Record THEN OSAG.CopyRecord(x, y)
            ELSIF x.type.form = OSAB.Array THEN
              IF (x.type.len >= y.type.len) & (y.type.len > 0) THEN OSAG.CopyRecord(x, y)
              ELSIF (x.type.len >= y.type.len) OR (x.type.len < 0) THEN OSAG.CopyArray(x, y)
              ELSE OSAS.Mark("incompatible arrays")
              END
            END
          ELSIF (x.type.form = OSAB.Array) & (x.type.base.form = OSAB.Char) & 
              (y.type.form = OSAB.String) THEN OSAG.CopyString(x, y)
          ELSE OSAS.Mark("illegal assignment")
          END
        ELSIF sym = OSAS.eql THEN OSAS.Mark("should be :="); OSAS.Get(sym); expression(y)
        ELSIF x.mode = OSAB.SProc THEN  (*in-line code procedures*)
          StandProc(obj.val DIV 4, obj.val MOD 4); 
        ELSIF x.mode = OSAB.Typ THEN OSAS.Mark("illegal assignment")
        ELSIF x.type.form = OSAB.Proc THEN (*procedure call*)
          CheckLeaf;
          IF sym = OSAS.lparen THEN OSAS.Get(sym); ParamList(x)
          ELSIF x.type.nofpar > 0 THEN OSAS.Mark("missing parameters")
          END ;
          OSAG.Call(x, {})
        ELSE OSAS.Mark("not a procedure")
        END
      ELSIF sym = OSAS.if THEN
        OSAS.Get(sym); expression(x); CheckBool(x); OSAG.CFJump(x);
        Check(OSAS.then, "no THEN");
        StatSequence; L0 := 0;
        WHILE sym = OSAS.elsif DO
          OSAS.Get(sym); OSAG.FJump(L0); OSAG.Fixup(x); expression(x); CheckBool(x);
          OSAG.CFJump(x); Check(OSAS.then, "no THEN"); StatSequence
        END ;
        IF sym = OSAS.else THEN
          OSAS.Get(sym); OSAG.FJump(L0); OSAG.Fixup(x); StatSequence
        ELSE OSAG.Fixup(x)
        END ;
        OSAG.FixLink(L0); Check(OSAS.end, "no END")
      ELSIF sym = OSAS.while THEN
        OSAS.Get(sym); OSAG.Here(L0); expression(x); CheckBool(x); OSAG.CFJump(x);
        Check(OSAS.do, "no DO"); StatSequence; OSAG.BJump(L0);
        WHILE sym = OSAS.elsif DO
          OSAS.Get(sym); OSAG.Fixup(x); expression(x); CheckBool(x); OSAG.CFJump(x);
          Check(OSAS.do, "no DO"); StatSequence; OSAG.BJump(L0)
        END ;
        OSAG.Fixup(x); Check(OSAS.end, "no END")
      ELSIF sym = OSAS.loop THEN
        OSAS.Get(sym); OSAG.Here(L0); 
        StatSequence;
        OSAG.BJump(L0) ;
        Check(OSAS.end, "no END")
      ELSIF sym = OSAS.repeat THEN
        OSAS.Get(sym); OSAG.Here(L0); StatSequence;
        IF sym = OSAS.until THEN
          OSAS.Get(sym); expression(x); CheckBool(x); OSAG.CBJump(x, L0)
        ELSE OSAS.Mark("missing UNTIL")
        END
      ELSIF sym = OSAS.case THEN
        OSAS.Get(sym); expression(x); CheckInt(x);
        Check(OSAS.colon, "no colon");
        IF sym = OSAS.int THEN nc := OSAS.ival ELSE nc := 0; OSAS.Mark("not an integer") END ;
        OSAS.Get(sym); Check(OSAS.of, "no OF");
        OSAG.Case(x, nc, L0); k0 := 0; L1 := 0;
        WHILE sym = OSAS.bar DO
          REPEAT OSAS.Get(sym);   (*labels*)
            IF sym = OSAS.int THEN
              k := OSAS.ival; OSAS.Get(sym);
              IF k >= nc THEN OSAS.Mark("label out of range")
              ELSIF k # k0 THEN OSAS.Mark("label out of order")
              ELSE OSAG.FixLink(L0+k); k0 := k+1
              END
            ELSE OSAS.Mark("not an integer")
            END
          UNTIL sym # OSAS.comma;
          Check(OSAS.colon, "no colon");
          StatSequence; OSAG.FJump(L1)
        END ;
        IF k+1 < nc THEN OSAS.Mark("missing cases") END;
        Check(OSAS.end, "no END"); OSAG.FixLink(L1)
      ELSIF sym = OSAS.for THEN
        OSAS.Get(sym);
        IF sym = OSAS.ident THEN
          qualident(obj); OSAG.MakeItem(x, obj); CheckInt(x); CheckReadOnly(x);
          IF sym = OSAS.becomes THEN
            OSAS.Get(sym); expression(y); CheckInt(y); OSAG.For0(x, y);
            OSAG.Here(L0);
            Check(OSAS.to, "no TO"); expression(z); CheckInt(z);
            IF sym = OSAS.by THEN OSAS.Get(sym); expression(w); CheckConst(w); CheckInt(w)
            ELSE OSAG.MakeConstItem(w, OSAB.intType, 1)
            END ;
            Check(OSAS.do, "no DO"); OSAG.For1(x, y, z, w, L1);
            StatSequence; Check(OSAS.end, "no END");
            OSAG.For2(x, y, w); IF leaf THEN OSAG.BJump(L0-1) ELSE OSAG.BJump(L0) END; OSAG.FixLink(L1)
          ELSE OSAS.Mark(":= expected")
          END
        ELSE OSAS.Mark("identifier expected")
        END
      ELSIF sym = OSAS.assert THEN
        OSAS.Get(sym); expression(x); CheckBool(x); OSAG.Not(x); 
        OSAG.Assert(x)
      ELSIF sym = OSAS.with THEN
        OSAS.Get(sym);
        IF sym = OSAS.ident THEN
          qualident(obj); OSAG.MakeItem(x, obj); Check(OSAS.colon, " : expected");
          IF sym = OSAS.ident THEN
            qualident(typ);
            IF (typ.class = OSAB.Typ) & (obj.type # NIL) THEN

              oldtype := obj.type; TypeTest(x, typ.type, 2); obj.type := typ.type

            ELSE OSAS.Mark("not a type")
            END
          ELSE OSAS.Mark("id expected")
          END
        ELSE obj := NIL; OSAS.Mark("id expected")
        END ;
        Check(OSAS.do, "DO missing"); StatSequence; Check(OSAS.end, "END expected");
        IF obj # NIL THEN obj.type := oldtype (*reset*) END
      END ;
      OSAG.CheckRegs;
      IF sym = OSAS.semicolon THEN OSAS.Get(sym)
      ELSIF sym < OSAS.semicolon THEN OSAS.Mark("missing semicolon?")
      END
    UNTIL sym > OSAS.semicolon
  END StatSequence;

  (* Types and declarations *)

  PROCEDURE^ Type(VAR type: OSAB.Type; tobj: OSAB.Object);

  PROCEDURE CheckExport(obj: OSAB.Object);
  BEGIN
    IF sym = OSAS.times THEN
      IF level = 0 THEN obj.expo := TRUE; OSAS.Get(sym)
      ELSE OSAS.Mark("!remove asterisk")
      END
    ELSE obj.expo := FALSE
    END
  END CheckExport;
  
  PROCEDURE IdentList(VAR first: OSAB.Object);
    VAR obj: OSAB.Object;
  BEGIN
    IF sym = OSAS.ident THEN
      first := OSAB.NewObj(); OSAS.Get(sym); CheckExport(first);
      WHILE sym = OSAS.comma DO
        OSAS.Get(sym);
        IF sym = OSAS.ident THEN
          obj := OSAB.NewObj(); OSAS.Get(sym); CheckExport(obj)
        ELSE OSAS.Mark("identifier expected")
        END
      END;
      IF sym = OSAS.colon THEN OSAS.Get(sym) ELSE OSAS.Mark("colon expected") END
    END
  END IdentList;

  PROCEDURE ArrayType(VAR type: OSAB.Type);
    VAR x: OSAG.Item; len: LONGINT;
  BEGIN expression(x);
    NEW(type); type.form := OSAB.NoTyp;
    IF (x.mode = OSAB.Const) & (x.type.form = OSAB.Int) & (OSAG.Val(x) > 0) THEN len := OSAG.Val(x)
    ELSE len := 1; OSAS.Mark("not a valid length")
    END ;
    (*IF len < 0 THEN OSAS.Mark("length < 0"); len := 0 ELSIF len = 0 THEN len := -1 END ;*)
    IF sym = OSAS.of THEN OSAS.Get(sym); Type(type.base, NIL)
    ELSIF sym = OSAS.comma THEN OSAS.Get(sym); ArrayType(type.base)
    ELSE OSAS.Mark("missing OF"); type.base := OSAB.intType
    END ;
    IF len >= 0 THEN
      type.size := len * type.base.size
    ELSE type.size := 4  (*array descriptor*)
    END ;
    type.form := OSAB.Array; type.len := len
  END ArrayType;

  PROCEDURE NewField(VAR obj: OSAB.Object);
    VAR new, t: OSAB.Object;
  BEGIN
    IF sym = OSAS.ident THEN
      NEW(new); OSAS.CopyId(new.name); OSAS.Get(sym);
      CheckExport(new); new.class := OSAB.Fld;
      new.next := obj; obj := new; t := new.next;
      WHILE t # NIL DO
        IF t.name = new.name THEN OSAS.Mark("multiple definition") END ;
        t := t.next
      END
    ELSE OSAS.Mark("ident missing")
    END
  END NewField;

  PROCEDURE FieldList(VAR obj: OSAB.Object; VAR size: LONGINT);
    VAR org, t: OSAB.Object; tp: OSAB.Type;
      n, offset: LONGINT;
  BEGIN org := obj; NewField(obj); n := 1;
    WHILE sym = OSAS.comma DO OSAS.Get(sym); NewField(obj); INC(n) END ;
    Check(OSAS.colon, "no :");
    Type(tp, NIL);
    size := size + n * tp.size; offset := size; t := obj;
    WHILE t # org DO
      t.class := OSAB.Fld; t.type := tp; offset := offset - tp.size; t.val := offset;
       t := t.next;
    END ;
    (*size := size + (-size) MOD 4*)
  END FieldList;

  PROCEDURE RecordType(VAR type: OSAB.Type; tobj: OSAB.Object);
    VAR fld, base: OSAB.Object; size: LONGINT;
  BEGIN NEW(type); type.form := OSAB.NoTyp; type.base := NIL;
    IF sym = OSAS.lparen THEN
      OSAS.Get(sym); (*record extension*)
      IF sym = OSAS.ident THEN
        qualident(base);
        IF base.class = OSAB.Typ THEN
          IF base.type.form = OSAB.Pointer THEN type.base := base.type.base
          ELSIF base.type.form = OSAB.Record THEN type.base := base.type
          ELSE OSAS.Mark("invalid extension type"); type.base := OSAB.intType
          END ;
          type.nofpar := type.base.nofpar + 1; (*"nofpar" here used for extension level*)
          fld := type.base.dsc; size := type.base.size
        ELSE OSAS.Mark("type expected")
        END
      ELSE OSAS.Mark("ident expected")
      END ;
      Check(OSAS.rparen, "no )")
    ELSE type.nofpar := 0; type.base := NIL; size := 0; fld := NIL
    END ;
    IF sym # OSAS.end THEN
      FieldList(fld, size);
      WHILE sym = OSAS.semicolon DO OSAS.Get(sym); FieldList(fld, size) END
    END ;
    (*size := size + (-size) MOD 4;*) type.dsc := fld; type.size := size; type.form := OSAB.Record;
    IF tobj = NIL THEN
      NEW(tobj); tobj.class := OSAB.Var; tobj.type := type; tobj.lev := 0; (*TD for anonymous type*)
    END ;
    type.typobj := tobj; OSAG.AllocTD(tobj)
  END RecordType;

  PROCEDURE FormalType(VAR typ: OSAB.Type; dim: INTEGER);
    VAR obj: OSAB.Object;
  BEGIN
    IF sym = OSAS.ident THEN
      qualident(obj);
      IF obj.class = OSAB.Typ THEN typ := obj.type ELSE OSAS.Mark("not a type"); typ := OSAB.intType END
    ELSIF sym = OSAS.array THEN
      OSAS.Get(sym); Check(OSAS.of, "OF ?");
      IF dim >= 1 THEN OSAS.Mark("multi-dimensional open arrays not implemented") END ;
      NEW(typ); typ.form := OSAB.Array; typ.len := -1; typ.size := 8; 
      FormalType(typ.base, dim+1)
    ELSE OSAS.Mark("identifier expected"); typ := OSAB.noType
    END
  END FormalType;

  PROCEDURE FPSection(VAR adr: LONGINT; VAR nofpar: INTEGER);
    VAR obj, first: OSAB.Object; tp: OSAB.Type;
      parsize: LONGINT; cl: INTEGER; rdo: BOOLEAN;
  BEGIN
    IF sym = OSAS.var THEN OSAS.Get(sym); cl := OSAB.Par; rdo := FALSE
    ELSIF sym = OSAS.const THEN OSAS.Get(sym); cl := OSAB.Const; rdo := TRUE
    ELSE cl := OSAB.Var; rdo := FALSE
    END ;
    IdentList(first); FormalType(tp, 0);
    IF cl = OSAB.Var THEN (*value param*)
      IF leaf THEN cl := OSAB.Reg END ;
      IF tp.form >= OSAB.Array THEN OSAS.Mark("no structured val params allowed") END ;
    ELSIF cl = OSAB.Par THEN (*var param*)
      IF leaf THEN cl := OSAB.RegI END
    ELSE (*const param*)
      IF leaf THEN
        IF tp.form < OSAB.Array THEN cl := OSAB.Reg ELSE cl := OSAB.RegI END
      ELSE
        IF tp.form < OSAB.Array THEN cl := OSAB.Var ELSE cl := OSAB.Par END
      END
    END ;
    IF (tp.form = OSAB.Array) & (tp.len < 0) OR (tp.form = OSAB.Record) THEN
      parsize := 2*OSAG.WordSize  (*open array or record, needs second word for length or type tag*)
    ELSIF cl = OSAB.Par THEN
      parsize := 2
    ELSE parsize := tp.size
    END ;
    obj := first;
    WHILE obj # OSAB.guard DO
      INC(nofpar); obj.class := cl; obj.type := tp; obj.rdo := rdo; obj.lev := level;
      IF leaf THEN obj.val := 24-adr ELSE obj.val := adr+1 END ;
      adr := adr + parsize;
      obj := obj.next
    END ;
    IF adr >= 48 THEN OSAS.Mark("too many parameters") END
  END FPSection;

  PROCEDURE ProcedureType(ptype: OSAB.Type; VAR parblksize: LONGINT);
    VAR obj: OSAB.Object; size: LONGINT; nofpar: INTEGER;
  BEGIN ptype.base := OSAB.noType; size := -parblksize; nofpar := 0; ptype.dsc := OSAB.guard;
    IF sym = OSAS.lparen THEN
      OSAS.Get(sym);
      IF sym = OSAS.rparen THEN OSAS.Get(sym)
      ELSE FPSection(size, nofpar);
        WHILE sym = OSAS.semicolon DO OSAS.Get(sym); FPSection(size, nofpar) END ;
        Check(OSAS.rparen, "no )")
      END ;
      ptype.nofpar := nofpar; parblksize := size;
      IF sym = OSAS.colon THEN  (*function*)
        OSAS.Get(sym);
        IF sym = OSAS.ident THEN qualident(obj);
          IF (obj.class = OSAB.Typ) & (obj.type.form <= OSAB.Pointer) THEN ptype.base := obj.type
          ELSE OSAS.Mark("illegal function type")
          END
        ELSE OSAS.Mark("type identifier expected")
        END
      END
    END
  END ProcedureType;
    
  PROCEDURE Type(VAR type: OSAB.Type; tobj: OSAB.Object);
    VAR dmy: LONGINT; obj: OSAB.Object; und: UndefRec;
  BEGIN type := OSAB.intType; (*sync*)
    IF (sym # OSAS.ident) & (sym < OSAS.array) THEN OSAS.Mark("not a type");
      REPEAT OSAS.Get(sym) UNTIL (sym = OSAS.ident) OR (sym >= OSAS.array)
    END ;
    IF sym = OSAS.ident THEN
      qualident(obj);
      IF (obj.class = OSAB.Typ) & (obj.type.form # OSAB.NoTyp) THEN 
        type := obj.type
      ELSE OSAS.Mark("not a type or undefined")
      END
    ELSIF sym = OSAS.array THEN OSAS.Get(sym); ArrayType(type)
    ELSIF sym = OSAS.record THEN
      OSAS.Get(sym); RecordType(type, tobj); Check(OSAS.end, "no END")
    ELSIF sym = OSAS.pointer THEN
      OSAS.Get(sym); Check(OSAS.to, "no TO");
      NEW(type);  type.form := OSAB.Pointer; type.size := OSAG.WordSize; type.base := OSAB.intType;
      IF sym = OSAS.ident THEN
        obj := OSAB.topScope.next;  (*search in top scope only for pointer base*)
        WHILE (obj # OSAB.guard) & OSAS.NotEql(obj.name) DO obj := obj.next END ;
        IF obj # OSAB.guard THEN  (*base found*)
          IF (obj.class = OSAB.Typ) & (obj.type.form IN {OSAB.Record, OSAB.NoTyp}) THEN type.base := obj.type
          ELSE OSAS.Mark("no valid base type")
          END
        ELSE (*base type still undefined, fixup in "declarations"*)
          NEW(und); OSAS.CopyId(und.name); und.type := type;
          und.next := undefRec; undefRec := und
        END ;
        OSAS.Get(sym)
      ELSE Type(type.base, tobj);
        IF type.base.form # OSAB.Record THEN OSAS.Mark("pointer base not record") END
      END ;
    ELSIF sym = OSAS.procedure THEN
      OSAS.Get(sym); OSAB.OpenScope;
      NEW(type); type.form := OSAB.Proc; type.size := OSAG.WordSize; dmy := 0;
      ProcedureType(type, dmy); type.dsc := OSAB.topScope.next; OSAB.CloseScope
    ELSE OSAS.Mark("illegal type")
    END
  END Type;

  PROCEDURE Declarations(reglim: INTEGER; VAR varsize: LONGINT; VAR regno: INTEGER); (*Updated by ling to set up the base address rigister for global variables*)
    VAR obj, first: OSAB.Object;
      x: OSAG.Item; tp: OSAB.Type; und: UndefRec;
  BEGIN (*sync*)  undefRec := NIL;
    IF (sym < OSAS.const) & (sym # OSAS.end) & (sym # OSAS.return) THEN
      OSAS.Mark("declaration expected");
      REPEAT OSAS.Get(sym) UNTIL (sym >= OSAS.const) OR (sym = OSAS.end)
    END ;
    IF sym = OSAS.const THEN
      OSAS.Get(sym);
      WHILE sym = OSAS.ident DO
        obj := OSAB.NewObj(); OSAS.Get(sym);
        CheckExport(obj); Check(OSAS.eql, "no =");
        IF sym = OSAS.string THEN
          obj.class := OSAB.Var; obj.lev := 0; obj.type := OSAB.strType;
          OSAG.AllocString(obj.val); OSAS.Get(sym);
        ELSE expression(x); obj.class := x.mode; obj.type := x.type;
          IF (x.mode = OSAB.Const) OR (x.type.form = OSAB.String) THEN
            obj.val := OSAG.Val(x); obj.lev := SHORT(OSAG.Lev(x))
          ELSE OSAS.Mark("not a constant")
          END
        END ;
        Check(OSAS.semicolon, "no ;")
      END
    END ;
    IF sym = OSAS.type THEN
      OSAS.Get(sym);
      WHILE sym = OSAS.ident DO
        obj := OSAB.NewObj(); OSAS.Get(sym);
        CheckExport(obj); Check(OSAS.eql, "no =");
        IF sym = OSAS.ident THEN OSAS.Mark("no alias allowed") END ;
        obj.class := OSAB.Typ; Type(obj.type, obj); obj.type.typobj := obj; obj.anc := NIL;
        IF obj.type.form = OSAB.Record THEN
          obj.lev := 0 (*TDs are always global*); und := undefRec;
          (*check whether this is base of a pointer type; search and fixup*)
          WHILE und # NIL DO
            IF obj.name = und.name THEN und.type.base := obj.type; und.name[0] := 0X END ;
            und := und.next
          END
        END ;
        Check(OSAS.semicolon, "no ;")
      END
    END ;
    IF sym = OSAS.var THEN
      OSAS.Get(sym);
      WHILE sym = OSAS.ident DO
        IdentList(first); Type(tp, NIL); obj := first;
        WHILE obj # OSAB.guard DO
          obj.type := tp;
          IF leaf & (tp.form IN {OSAB.Int, OSAB.Set}) & (regno < reglim) THEN
            obj.class := OSAB.Reg; obj.val := 24-regno; INC(regno, 2) (*register variable*)
          ELSE obj.class := OSAB.Var; obj.type := tp; obj.lev := level; (* varsize := varsize + tp.size; commented by ling*)
            (*IF tp.size >= 4 THEN varsize := varsize + (-varsize) MOD 4 END ;commented by ling*)
            (*obj.val := -varsize;commented by ling*)
            obj.val := varsize;
            varsize := varsize + tp.size;
            IF (tp.form = OSAB.Array) & (tp.len < 0) THEN obj.class := OSAB.Par; INC(obj.val, 4) END ;
            IF obj.expo & ~(obj.type.form IN {OSAB.Bool .. OSAB.Pointer}) THEN
              OSAS.Mark("export not allowed")
            END ;
          END ;
          obj := obj.next
        END ;
        Check(OSAS.semicolon, "no ;")
      END
    END ;
    (*varsize := varsize + (-varsize) MOD 4;*) und := undefRec;
    WHILE und # NIL DO
      IF und.name[0] > 0X THEN OSAS.Mark("undefined pointer base type") END ;
      und := und.next
    END ;
    IF (sym >= OSAS.const) & (sym <= OSAS.var) THEN OSAS.Mark("decl in bad order") END
  END Declarations;

  PROCEDURE ProcedureDecl;
    VAR proc: OSAB.Object;
      type: OSAB.Type;
      procid: OSAS.Ident;
      x: OSAG.Item;
      locblksize, parblksize, L: LONGINT;
      nofpar, regvarno, resregs, retoffset: INTEGER;
      int: BOOLEAN; intDex: LONGINT;
  BEGIN OSAS.Get(sym); CheckLeaf; parblksize := 0; resregs := 0;
    IF sym = OSAS.times THEN (*leaf procedure*) OSAS.Get(sym); leaf := TRUE;
      WHILE sym = OSAS.times DO OSAS.Get(sym); INC(resregs) END
    END ;
    IF sym = OSAS.ident THEN
      OSAS.CopyId(procid); OSAS.Get(sym);
      proc := OSAB.NewObj(); proc.class := OSAB.Const; regvarno := 0; parblksize := resregs;
      NEW(type); type.form := OSAB.Proc; type.size := OSAG.WordSize; proc.type := type;
      CheckExport(proc);
      OSAB.OpenScope; INC(level); proc.val := -1; type.base := OSAB.noType;
      IF sym = OSAS.lbrak THEN (*interrupt handler*)
        OSAS.Get(sym); int := TRUE; Check(OSAS.int, "number?");
        IF leaf THEN regvarno := 8 (*FIQ*) END ;
        retoffset := SHORT(OSAS.ival MOD 10H); Check(OSAS.rbrak, "] ")
      ELSE int := FALSE; ProcedureType(type, parblksize)      
      END ;            
      Check(OSAS.semicolon, "no ;"); locblksize := parblksize+1;
      Declarations(SHORT(OSAG.WRegLim - parblksize), locblksize, regvarno);      
      IF int & leaf & (locblksize > 0) THEN OSAS.Mark("too many vars") END ;
      OSAG.Here(proc.val); proc.type.dsc := OSAB.topScope.next;
      IF sym = OSAS.procedure THEN
        L := 0; OSAG.FJump(L);
        REPEAT ProcedureDecl; Check(OSAS.semicolon, "no ;") UNTIL sym # OSAS.procedure;
        OSAG.FixLink(L)
      END ;
      OSAG.Here(proc.val); proc.type.dsc := OSAB.topScope.next;
      OSAG.Enter(leaf, int, level, regvarno, parblksize, locblksize - parblksize-1);
      IF sym = OSAS.begin THEN OSAS.Get(sym); StatSequence END ;
      IF sym = OSAS.return THEN
        OSAS.Get(sym); expression(x);OSAG.ProcReturn(x);
        IF type.base = OSAB.noType THEN OSAS.Mark("this is not a function")
        ELSIF ~CompTypes(type.base, x.type) THEN OSAS.Mark("wrong result type")
        END
      ELSIF type.base.form # OSAB.NoTyp THEN
        OSAS.Mark("function without result"); type.base := OSAB.noType
      END ;
      OSAG.Return(leaf, int, retoffset, resregs, type.base.form, x);
      OSAB.CloseScope; DEC(level); Check(OSAS.end, "no END");
      IF sym = OSAS.ident THEN
        IF OSAS.NotEql(procid) THEN OSAS.Mark("no match") END ;
        OSAS.Get(sym)
      ELSE OSAS.Mark("no proc id")
      END
    END ;
    leaf := FALSE
  END ProcedureDecl;

  (* Modules *)

  PROCEDURE Module*;
    CONST Ttablen = 100;
    VAR dummy: INTEGER; key, i: LONGINT;
      impid, impid1: OSAS.Ident;
      obj, obj1: OSAB.Object;
      
  BEGIN 
	OIO.WriteString("  compiling "); 
	OSAS.Get(sym);    
	IF sym = OSAS.module THEN      
		OSAS.Get(sym); 
		OSAB.Init; 
		OSAB.OpenScope; 
		leaf := FALSE;      
		IF sym = OSAS.ident THEN        
			OSAS.CopyId(OSAS.modid); 
			OSAS.Get(sym);        
			OIO.WriteString( OSAS.modid); 
			OIO.WriteLn(); 
			OIO.FileUpdate(OSAS.f)      
		ELSE 
			OSAS.Mark("identifier expected")      
		END ;      
		Check(OSAS.semicolon, "no ;"); 
		level := 0; 
		dc := 0;      
		IF sym = OSAS.import THEN        
			OSAS.Get(sym);        
			WHILE sym = OSAS.ident DO              
				OSAS.CopyId(impid); 
				OSAS.Get(sym);          
				IF sym = OSAS.becomes THEN            
					OSAS.Get(sym);            
					IF sym = OSAS.ident THEN 
						OSAS.CopyId(impid1); 
						OSAS.Get(sym)            
					ELSE 
						OSAS.Mark("id expected")            
					END          
				ELSE 
					impid1 := impid          
				END ;          
				OSAB.Import(impid, impid1);          
				IF sym = OSAS.comma THEN 
					OSAS.Get(sym)          
				ELSIF sym = OSAS.ident THEN 
					OSAS.Mark("comma missing")          
				END        
			END ;        
			Check(OSAS.semicolon, "no ;")        
		END ;
      
      
      OSAG.Open; Declarations(10, dc, dummy);
      WHILE sym = OSAS.procedure DO ProcedureDecl; Check(OSAS.semicolon, "no ;") END ;
      OSAG.Header;
      IF sym = OSAS.begin THEN OSAS.Get(sym); StatSequence END ;
      Check(OSAS.end, "no END");
      IF sym = OSAS.ident THEN
        IF OSAS.NotEql(OSAS.modid) THEN OSAS.Mark("no match") END ;
        OSAS.Get(sym)
      ELSE OSAS.Mark("identifier missing")
      END ;
      Check(OSAS.period, "period missing");
      IF ~OSAS.error THEN
          OSAB.Export(OSAS.modid, newSF, key);
          IF newSF THEN OIO.WriteString( "new symbol file; ") END
      END ;
      IF ~OSAS.error THEN
        OSAG.Close(OSAS.modid, key, dc); OIO.WriteString( " code generated, len =");
        OSAG.Here(key); OIO.WriteLongInt( key, 6); OIO.WriteLongInt( dc, 6);
        OIO.WriteLn(); OIO.FileUpdate(OSAS.f)
      END ;
      OSAB.CloseScope()
    ELSE OSAS.Mark("must start with MODULE")
    END
  END Module;
  
  PROCEDURE Init*(VAR F: OIO.File;  newSymFile: BOOLEAN; breakpc: LONGINT);  
	BEGIN    
		newSF := newSymFile; 
		OSAS.Init(F); 
		OSAG.Init(breakpc);  
	END Init;



BEGIN 
	OIO.WriteString("FOC Compiler  22.12.2012"); 
	OIO.WriteLn();
END OAVP0.

