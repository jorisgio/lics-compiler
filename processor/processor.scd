Start <- ()
         
/* Définition des registres 32 bits */
         array pc[32];

         /* memory unit */
         array instr[32];
         instr[0..31] = lw(pc[0..31]);

	 datasrc = instr[29] && true;
	 regWrite = not instr[31];
	 jump = instr[30] && true;
	 
      	 array signextend[32];
         for(i,0,15) {
                     signextend[i] = instr[i] && true;
         }
         for(i,16,31) {
                      signextend[i] = instr[15] && true;
         }

	 /* l'instruction contient 6 bits d'opcode au début, seuls comptent les deux derniers */

         /* multiplexeur choisissant l'addresse du rehistre à écrire. Pour les instructions R-types c'est dans rd, sinon c'est dans rt pour load */
         array writeReg[5];
         for(i,0,4) {
                    writeReg[i] = mux(not instr[29], instr[i+11], instr[i+16]);
         }
	 array alu[32];
	 
	 /* choix de l'entrée des données à écrire dans le registre, soit le résultat de l'alu, soit les données directes pour li */	
	 array regdataentry[32];
	 for(i,0,31) {
	 	     regdataentry[i] = mux(datasrc,signextend[i],alu[i]);
		     }

         array regs[64];
         regs[0..63] = Regs <- (alu[0..31], writeReg[0..4], instr[21..25],instr[16..20], regWrite);

	 array alufunc[3] ;
	 alufunc[0..2] = ALUControl <- (instr[31],instr[0..2]);

         f = false;   	
		     
         alu[0..31] = Alu32bits <- (regs[0..31],regs[32..63],f,instr[3],alufunc[0..2]);

         /* calcul de l'adresse de l'instruction suivante */

         /* Taille de 1 mot */
         array quatre[32];
         for(i,0,31) {
                     quatre[i] = false;
         }
         quatre[5] = true;

         array addressplus[32];
         addressplus[0..31] = Adder32 <- (quatre[0..31], pc[0..31]);

         f0 = false;
         f1 = false;

         array branch[32];
         /* signextend est shifté préalablement de 2 */
         branch[0..31] = Adder32 <-(addressplus[0..31],f0,f1,signextend[0..29]);

         array zero[1];
         zero[0..0] = Zero <- (alu[0..31]);
	 
	 /* Choix du nouveau PC en fonction de l'instruction (jump, branch,
	 li ou R-type) */
         /* rem : br = instr[31] */


         /* on traite à part le 0 et le 1 pour simuler un shift */

         pc[0] = reg mux(jump,f,( mux((zero[0] and instr[31]),branch[0],addressplus[0]) ));
         pc[1] = reg mux(jump,f,( mux((zero[0] and instr[31]),branch[1],addressplus[1]) ));
	 
	 for(i,2,27) {
         	     pc[i] = reg mux(jump,instr[i-2],( mux((zero[0] and instr[31]),branch[i],addressplus[i]) ));
	}
	pc[28] = reg mux(jump,addressplus[28],( mux((zero[0] and instr[31]), branch[28], addressplus[28])));
	pc[29] = reg mux(jump,addressplus[29],( mux((zero[0] and instr[31]), branch[29], addressplus[29])));
	pc[30] = reg mux(jump,addressplus[30],( mux((zero[0] and instr[31]), branch[30], addressplus[30])));
	pc[31] = reg mux(jump,addressplus[31],( mux((zero[0] and instr[31]), branch[31], addressplus[31])));

	 
-> ()

