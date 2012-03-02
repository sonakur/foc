MODULE Gen;
IMPORT OIO;

VAR

PROCEDURE CommandName * (c: LONGINT; VAR xName: ARRAY OF CHAR);
	BEGIN
		CASE (c) OF
			0: COPY("0x80", xName)
		END
	END CommandName;

PROCEDURE RegName * (r: LONGINT; VAR rName: ARRAY OF CHAR);
	BEGIN
		CASE (r) OF
			0: COPY("%rax", rName)
			|1: COPY("%rbx", rName)
			|2: COPY("%rcx", rName)
			|3: COPY("%rdx", rName)
			|4: COPY("%rsi", rName)
			|5: COPY("%rdi", rName)
			|6: COPY("%rbp", rName)
			|7: COPY("%rsp", rName)
			|8: COPY("%rip", rName)
			|9: COPY("%r8", rName)
			|10: COPY("%r9", rName)
			|11: COPY("%r10", rName)
			|12: COPY("%r11", rName)
			|13: COPY("%r12", rName)
			|14: COPY("%r13", rName)
			|15: COPY("%r14", rName)
			|16: COPY("%r15", rName)
		END
	END RegName; 

(*PROCEDURE GloblStart *(VAR f: OIO.File);
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
*)
PROCEDURE Mov * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "movq ");
	END Mov;

PROCEDURE Movsb * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "movsbq ");
	END Movsb;
PROCEDURE WriteBase * (VAR f: OIO.File; base: LONGINT);
	BEGIN
		OIO.FWriteString(f, "(");
		OIO.FWriteLInt(f, base);
		OIO.FWriteString(f, ")");
	END WriteBase;

PROCEDURE Int * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "int ");
	END Int;

PROCEDURE WriteComma*(VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, ", ");
	END WriteComma;

PROCEDURE Const *(VAR f: OIO.File);
	BEGIN		
		OIO.FWriteString(f, "$");
	END Const;
BEGIN
END Gen.
