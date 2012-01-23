Start <- ()
         
/* Définition des registres 32 bits */
         array pc[32];

         /* memory unit */
         array instr[32];
         instr[0..31] = LW(pc[0..31]);

	 /* l'instruction contient 6 bits d'opcode au début, seuls comptent les deux derniers */

         /* multiplexeur */
         array writeReg[5];
         for(i,0,4) {
                    writeReg[i] = MUX(NOT instr[31], instr[i+11], instr[i+16]);
         }

         array regs[64];
         regs[0..63] = Regs <- (alu[0..31], writeReg[0..4], instr[21..25],instr[16..20], NOT instr[31]);

	 array alufunc[3] 
	 alufunc[0..2] = ALUControl <- (instr[31],instr[0..2]);
	 
         array alu[32];
         alu[0..31] = Alu32bits <- (regs[0..31], regs[32..63],alufunc[0..2]);

         /* calcul de l'adresse de l'instruction suivante */

         /* par défaut c'est zéro */
         array quatre[32];
         quatre[5] = TRUE;

         array addressplus[32];
         addressplus[0..31] = Fulladder <- (quatre[0..31], pc[0..31]);

         array sign_extend[32];
         for(i,0,15) {
                     sign_extend[i] = instr[i] && true;
         }
         for(i,16,31) {
                      sign_extend[i] = instr[15] && true;
         }

         array branch[32];
         branch([0..31]) = Fulladder <-(addressplus[0..31],sign_extend[32]);

         array zero[1];
         zero[0] = Zero <- (alu[32]);
	 
	 array jump[32] ;
	 for(i,0,29) {
         	     pc[i] = MUX(instr[30],instr[i],( MUX((zero[0] AND instr[31]),branch[i],addressplus[i]) ));
	}
	pc[30] = MUX(instr[30],false,( MUX((zero[0] AND instr[31]), branch[30], addressplus[30])));
	pc[31] = MUX(instr[31],false,( MUX((zero[0] AND instr[31]), branch[31], addressplus[31])));

	 
-> ()

