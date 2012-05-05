MODULE FOCC;

IMPORT OIO, OSAS := OAVS0, OSAP := OAVP0, Args;

VAR
	is_main: BOOLEAN;
	name: ARRAY 255 OF CHAR;

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
                        WHILE (*((arg[i] # '.') &*) (arg[i] # 0X) DO (* read filename*)
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

PROCEDURE Compile();
	VAR 	is_opened, error: BOOLEAN; 
		f: OIO.File;
BEGIN
	is_opened:=OIO.OpenFile(name, f);
	error:=FALSE;

	IF (is_opened = FALSE) THEN
		OIO.WriteString(name); 
		OIO.WriteString(" not found."); OIO.WriteLn();
		OIO.UpdateWriter;
		error:= TRUE;
	END;
	OSAP.Compile(f, is_main, error);

	(* continue? *)
	OIO.WriteString("compileing from focc");
	END Compile;
BEGIN
	ParseOptions();
	OIO.WriteString("name="); OIO.WriteString(name); OIO.WriteLn();
	Compile();
END FOCC.
