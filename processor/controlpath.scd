/* Partie contrôl de l'alu. Prend un opcode et un bout d'instruction, et renvoit le bon code de function à l'ALU */
/* pour simplifier, on considère que la partie basse des instructions de type R contient déjà le code defonction de l'ALU */
ALUControl <- (aluop,func[3]) 
	   array aluctl[3];
	   array truth1[3];
	   truth1[0] = false && true;
	   truth1[1] = true  && true;
	   truth1[2] = true && true;
	   for(i,0,2) {
	   	aluctl[i] = MUX(aluop,func[i],truth1[i]);
		}
-> (aluctl[0..2])

