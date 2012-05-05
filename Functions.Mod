MODULE Functions;
IMPORT IntStr, Strings;

PROCEDURE LIntToStr * (n: LONGINT; VAR str: ARRAY OF CHAR);
	BEGIN
		IntStr.IntToStr(n,str);
	END LIntToStr;

PROCEDURE StrAppend * (source: ARRAY OF CHAR; VAR destinastion: ARRAY OF CHAR);
	BEGIN
		Strings.Append(source, destinastion);
	END StrAppend;

PROCEDURE Reverse (VAR str : ARRAY OF CHAR; start, end : INTEGER); (* (c) Norayr *)
(* Reverses order of characters in the interval [start..end]. *)
	VAR
 	h : CHAR;
  	BEGIN
    		WHILE start < end DO
        		h:= str[start]; 
			str[start]:= str[end]; 
			str[end] := h;
	    		INC(start); 
			DEC(end)
	      	END
	END Reverse;

PROCEDURE IntToStr*(int: LONGINT; VAR str: ARRAY OF CHAR); (* (c) Norayr *)
(**Converts the value of @oparam{int} to string form and copies the possibly
   truncated result to @oparam{str}.  *)
	CONST
     		maxLength = 11; (* maximum number of digits representing a LONGINT value *)
     	VAR
       		b : ARRAY maxLength+1 OF CHAR;
         	s, e: INTEGER;
	BEGIN
	   (* build representation in string 'b' *)
		IF int = MIN(LONGINT) THEN      (* smallest LONGINT, -int is an overflow *)
	        	b := "-2147483648";
		     	e := 11
		ELSE
			IF int < 0 THEN               (* negative sign *)
				b[0] := "-"; 
				int := -int; 
				s := 1
			ELSE  (* no sign *)
				s := 0
			END;
			        e := s;                       (* 's' holds starting position of string *)
			REPEAT
			        b[e] := CHR(int MOD 10+ORD("0"));
			        int := int DIV 10;
			        INC(e)
			UNTIL int = 0;
			b[e] := 0X;
			Reverse(b, s, e-1)
		END;
	        COPY(b, str) (* truncate output if necessary *)
	 END IntToStr;
BEGIN
END Functions.
