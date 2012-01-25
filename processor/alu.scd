Alu1bit <-(a, b,less,ainvert, binvert, carryIn, operation[3])
        na = not a;
        nb = not b;
	a1 = mux(ainvert, a, na);   
	b1 = mux(binvert, b, nb);
 	/* ALUOp */
	op0 = a1 and b1;
	op1 = a1 || b1;
	array op2[2];
	op2[0..1] = Fulladder<-(a1,b1,carryIn);
        array f[4];
        for (i,0,3) {
            f[i] = false;
            }
        l = less and true;
        array out[1];
	out[0..0] = Mux8to1<-(operation[0..2],op0,op1,op2[0],f[0],f[1],f[2],f[3],l);
-> (out[0],op2[1])
/*
Alulastbit <- (a, b,less,ainvert, binvert, carryIn, operation[3])
        na = not a;
        nb = not b;
	a1 = mux(ainvert, a, na);   
	b1 = mux(binvert, b, nb);
 	COMMENTAIRE * ALUOp *
	op0 = a1 and b1;
	op1 = a1 || b1;
	array op2[2];
	op2[0..1] = Fulladder<-(a1,b1,carryIn);
        array f[4];
        for (i,0,3) {
            f[i] = false;
            }
        l = less and true;
        array out[1];
	out[0..0] = Mux8to1<-(operation[0..2],op0,op1,op2[0],f[0],f[1],f[2],f[3],l);
->(out[0],op2[0])
*/
Alu32bits <- (a[32],b[32],ainvert,binvert,carryIn,operation[3])
	  array carry[32];
	  array s[32];
	  array out0[2];
          array op2[2];

          /* Adaptation pour régler les prolongements du pb de l'identite */

          out31bis = op2[0] && true;

          array operationbis[3];
          for (i,0,2) {
              operationbis[i] = operation[i] && true;
          }
          
	  
	  out0[0..1] = Alu1bit <-
	  (a[0],b[0],out31bis,ainvert,binvert,carryIn,operationbis[0..2]);
          carry[0] = out0[1] and true;
          
	  f = false;
	  for(i,1,30){
		array out[2];
		out[0..1] = Alu1bit <- (a[i],b[i],f,ainvert,binvert,carry[i-1],operationbis[0..2]);
		carry[i] = out[1] and true;
		s[i] = out[0] and true;
		}
	  

          /* On intègre à la min Alulastbit dans le code pour s'en tirer
          avec le pb de l'identité */

          na = not a[31];
          nb = not b[31];
          a1 = mux(ainvert, a[31], na);   
          b1 = mux(binvert, b[31], nb);
          /* ALUOp */
          op0 = a1 and b1;
          op1 = a1 || b1;
          op2[0..1] = Fulladder<-(a1,b1,carry[30]);
          array f[4];
          for (i,0,3) {
              f[i] = false;
          }

          array out31[1];
          out31[0..0] = Mux8to1<-(operationbis[0..2],op0,op1,op2[0],f[0],f[1],f[2],f[3],f);


->(out0[0], s[1..30], out31[0])
