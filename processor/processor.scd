Start <- ()
         
/* Définition des registres 32 bits */
         array pc[32];

         /* memory unit */
         array instr[32];
         instr[0..31] = LW(pc[0..31]);

	 datasrc = instr[29] && true;
	 regWrite = NOT instr[31];
	 jump = instr[30] && true;
	 br = instr[31] && true;
	 
      	 array sign_extend[32];
         for(i,0,15) {
                     sign_extend[i] = instr[i] && true;
         }
         for(i,16,31) {
                      sign_extend[i] = instr[15] && true;
         }

	 /* l'instruction contient 6 bits d'opcode au début, seuls comptent les deux derniers */

         /* multiplexeur choisissant l'addresse du rehistre à écrire */
         array writeReg[5];
         for(i,0,4) {
                    writeReg[i] = MUX(NOT instr[31], instr[i+11], instr[i+16]);
         }
	 array alu[32];
	 
	 /* choix de l'entrée des données à écrire dans le registre, soit le résultat de l'alu, soit les données directes pour li */	
	 array regdataentry[32];
	 for(i,0,31) {
	 	     regdataentry[i] = MUX(datasrc,sign_extend[i],alu[i])
		     }

         array regs[64];
         regs[0..63] = Regs <- (alu[0..31], writeReg[0..4], instr[21..25],instr[16..20], regWrite);

	 array alufunc[3] ;
	 alufunc[0..2] = ALUControl <- (instr[31],instr[0..2]);

   	
		     
         alu[0..31] = Alu32bits <- (regs[0..31],regs[32..63],false,instr[3],alufunc[0..2]);

         /* calcul de l'adresse de l'instruction suivante */

         /* par défaut c'est zéro */
         array quatre[32];
         quatre[5] = TRUE;

         array addressplus[32];
         addressplus[0..31] = Fulladder <- (quatre[0..31], pc[0..31]);



         array branch[32];
         branch([0..31]) = Fulladder <-(addressplus[0..31],sign_extend[32]);

         array zero[1];
         zero[0] = Zero <- (alu[32]);
	 
	 /* Choix du nouveau PC en fonction de l'instruction (jump, branch, li ou R-type) */
	 array jump[32] ;
	 for(i,0,29) {
         	     pc[i] = REG MUX(jump,instr[i],( MUX((zero[0] AND br),branch[i],addressplus[i]) ));
	}
	pc[29] = REG MUX(jump,false,( MUX((zero[0] AND instr[31]), br, addressplus[29])));
	pc[30] = REG MUX(jump,false,( MUX((zero[0] AND instr[31]), br, addressplus[30])));
	pc[31] = REG MUX(jump,false,( MUX((zero[0] AND instr[31]), br, addressplus[31])));

	 
-> ()

