/* Partie contrôl de l'alu. Prend un opcode et un bout d'instruction, et renvoit le bon code de function à l'ALU */
/* pour simplifier, on considère que la partie basse des instructions de type R contient déjà le code defonction de l'ALU */
ALUControl <- (aluop,negate,func[3]) 
	   array aluctl[3];
	   array truth1[3];
	   /* dans le cas d'un beq, on doit faire une soustraction */
	   truth1[0] = false;
	   truth1[1] = false;
	   truth1[2] = false;
	   t = true;
	   n = mux(aluop,t, negate);
	   for(i,0,2) {
	   	aluctl[i] = mux(aluop,truth1[i],func[i]);
		}
-> (n,aluctl[0..2])

