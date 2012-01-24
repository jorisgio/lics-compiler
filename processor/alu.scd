Alu1bit <-(a, b,less,ainvert, binvert, carryIn, operation[3])
	array out[32];
	a1 = mux(ainvert, a, not a);   
	b1 = mux(binvert, b, not b);
 	/* ALUOp */
	op0 = a1 and b1;
	op1 = a1 || b1;
	array op2[2];
	op2[0..1] = Fulladder<-(a1,b1,carryIn);
        f = false;
	out[0..0] = Mux8to1<-(operation[0..2],op0,op1,op2[0],f,f,f,f,less);
-> (out[0],op2[1])

Alulastbit <- (a, b,less,ainvert, binvert, carryIn, operation[3])
	array out[32];
	a1 = mux(ainvert, a, not a);   
	b1 = mux(binvert, b, not b);
 	/* ALUOp */
	op0 = a1 and b1;
	op1 = a1 || b1;
	array op2[2];
	op2[0..1] = Fulladder<-(a1,b1,carryIn);
        f = false;
	out[0..0] = Mux8to1<-(operation[0..2],op0,op1,op2[0],f,f,f,f,less);
->(out[0],op2[0])

Alu32bits <- (a[32],b[32],ainvert,binvert,carryIn,operation[3])
	  array carry[32];
	  array s[32];
	  array out0[2];
	  array out31[2];
	  
	  out0[0..1] = Alu1bit <-
	  (a[0],b[0],out31[1],ainvert,binvert,carryIn,operation[0..2]);

	  f = false;
	  for(i,1,30){
		array out[2];
		out[0..1] = Alu1bit <- (a[i],b[i],f,ainvert,binvert,carry[i-1],operation[0..2]);
		carry[i] = out[1] and true;
		s[i] = out[0] and true;
		}
	  out31[0..1] = Alulastbit <-
		(a[31],a[31],f,ainvert,binvert,carry[30],operation[0..2]);

->(out0[0], s[1..30], out31[0])
