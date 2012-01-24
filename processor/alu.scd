Alu1bit <-(a, b,less,ainvert, binvert, carryIn, operation[2])
	array out[32];
	a1 = mux(ainvert, a, not a);   
	b1 = mux(binvert, b, not b);
 	/* ALUOp */
	op0 = a1 and b1;
	op1 = a1 || b1;
	array op2[2];
	op2[0..1] = Fulladder<-(a1,b1,carryIn);
	out[0..0] = Mux8to1<-(operation[0..2],op0,op1,op2[0],false, false,false,less);
-> (out[0],op2[1])

Alulastbit <- (a, b,less,ainvert, binvert, carryIn, operation[2])
	array out[32];
	a1 = mux(ainvert, a, not a);   
	b1 = mux(binvert, b, not b);
 	/* ALUOp */
	op0 = a1 and b1;
	op1 = a1 || b1;
	array op2[2];
	op2[0..1] = Fulladder<-(a1,b1,carryIn);
	out[0..0] = Mux8to1<-(operation[0..3],op0,op1,op2[0],false, false,false,less);
->(out[0],op2[0])

Alu32bits <- (a[32],b[32],ainvert,binvert,carryIn,operation[2])
	  array carry[32];
	  array s[32];
	  array out0[2];
	  array out31[2];
	  
	  out0[0..1] = Alu1bit <- (a[0],b[0],out31[1],ainvert,binvert,carryIn,operation[0..2]);
	  
	  for(i,1,30){
		array out[2];
		out[0..1] = Alu1bit <- (a[i],b[i],false,ainvert,binvert,carry[i-1],operation[0..2]);
		carry[i] = out[1] and true;
		s[i] = out[0];
		}
	  out31[0..1] = Alulastbit <- (a[31],a[31],false,ainvert,binvert,carry[30],operation[0..2]);
	  s[0] = out0[0] and true;
	  s[31] = out31[0] and true;
->(s[0..31])
