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
			|12: COPY("%rsi", rName)
			|13: COPY("%rdi", rName)
			|14: COPY("%rbp", rName)
			|15: COPY("%rsp", rName)
			|16: COPY("%rip", rName)
			|4: COPY("%r8", rName)
			|5: COPY("%r9", rName)
			|6: COPY("%r10", rName)
			|7: COPY("%r11", rName)
			|8: COPY("%r12", rName)
			|9: COPY("%r13", rName)
			|10: COPY("%r14", rName)
			|11: COPY("%r15", rName)
			
			|17: COPY("%xmm0", rName);
			|18: COPY("%xmm1", rName);
			|19: COPY("%xmm2", rName);
			|20: COPY("%xmm3", rName);
			|21: COPY("%xmm4", rName);
			|22: COPY("%xmm5", rName);
			|23: COPY("%xmm6", rName);
			|24: COPY("%xmm7", rName);
			|25: COPY("%xmm8", rName);
			|26: COPY("%xmm9", rName);
			|27: COPY("%xmm10", rName);
			|28: COPY("%xmm11", rName);
			|29: COPY("%xmm12", rName);
			|30: COPY("%xmm13", rName);
			|31: COPY("%xmm14", rName);
			|32: COPY("%xmm15", rName);
			
			|33: COPY("%al", rName);
			|34: COPY("%bl", rName);
			|35: COPY("%cl", rName);
			|36: COPY("%dl", rName);
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

PROCEDURE Cmp * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "cmpq ");
	END Cmp;

PROCEDURE Jne * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "jne ");
	END Jne;


PROCEDURE Inc * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "incq ");
	END Inc;


PROCEDURE Movss * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "movss ");
	END Movss;

PROCEDURE Movsb * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "movsbq ");
	END Movsb;

PROCEDURE Movb * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "movb ");
	END Movb;

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

PROCEDURE Const *(VAR f: OIO.File; const: LONGINT);
	BEGIN		
		OIO.FWriteString(f, "$");
		OIO.FWriteLInt(f, const);
	END Const;

PROCEDURE SectionText *(VAR f: OIO.File);
	BEGIN		
		OIO.FWriteString(f, ".section .text");
		OIO.FWriteLn(f);
	END SectionText;


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

PROCEDURE Init * (VAR f: OIO.File; modname: ARRAY OF CHAR);
	BEGIN
		OIO.FWriteString(f, ".section .text");
		OIO.FWriteLn(f);
		OIO.FWriteString(f, ".globl ");
		OIO.FWriteString(f, modname); OIO.FWriteString(f, "_init");
		OIO.FWriteLn(f);		
		OIO.FWriteString(f, modname);OIO.FWriteString(f, "_init:");
		OIO.FWriteLn(f);
	END Init;

PROCEDURE Add * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "addq ");
	END Add;

PROCEDURE Sub * (VAR f: OIO.File);
	BEGIN
		OIO.FWriteString(f, "subq ");
	END Sub;

BEGIN
END Gen.
