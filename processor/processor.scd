/* Le processeur reçoit une entrée qui vaut 1 à chaque nouvelle seconde et
doit alors incrémenter un registre spécial $31 */

Start <- (top)
         
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
                    writeReg[i] = mux(instr[29], instr[i+16], instr[i+11]);
         }
	 array alu[32];
	 
	 /* choix de l'entrée des données à écrire dans le registre, soit le résultat de l'alu, soit les données directes pour li */	
	 array regdataentry[32];
	 for(i,0,31) {
	 	     regdataentry[i] = mux(datasrc,signextend[i],alu[i]);
		     }
         
         /* on reçoit deux registres utiles dans le datapath + 14 registres
         supplémentaires qui vont directement en sortie du microprocesseur
         pour affichage */
         /* on donne un argument supplémentaire qui dit s'il faut
         incrémenter le registre $31 qui stocke le timestamp */

         array regs[512];
         regs[0..511] = Regs <- (alu[0..31], writeReg[0..4],
         instr[21..25],instr[16..20], regWrite, top);

	 array alufunc[3] ;
	 alufunc[0..2] = ALUControl <- (instr[31],instr[0..2]);

         f = false;   	
		     
         alu[0..31] = Alu32bits <- (regs[0..31],regs[32..63],f,instr[3],alufunc[0..2]);

         /* calcul de l'adresse de l'instruction suivante */

         /* Taille de 1 mot */
         array un[32];
         for(i,1,31) {
                     un[i] = false;
         }
         un[0] = true;
	 f42 = false;
         array addressplus[32];
         addressplus[0..31] = Adder32 <- (un[0..31], pc[0..31],f42);

	 /* Calcul de l'addresse après branchement, qui est PC + partie basse de l'instruction */
         array branch[32];
         branch[0..31] = Adder32 <-(addressplus[0..31],signextend[0..31],f42);

         array zero[1];
         zero[0..0] = Zero <- (alu[0..31]);
	 
	 /* Choix du nouveau PC en fonction de l'instruction (jump, branch,
	 li ou R-type) */

         array pctmp[32]; /* on developpe pour éviter les bugs du compilo */

         /* calcul pour savoir si on branch */
         br = zero[0] and instr[31];

       	 
	 for(i,0,31) {
         	     pctmp[i] = mux(jump,instr[i],( mux(br,branch[i],addressplus[i]) ));
	}
                     
        pc[0..31] = Reg32 <- (pctmp[0..31]);

	 
-> (regs[64..511]) /* on renvoit les valeurs des registres à afficher */

