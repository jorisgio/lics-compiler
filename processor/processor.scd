/* Choisit l'opération effectuée par l'ALU */
Opmultiplexor <-(op[4], inputs[4])
	result = MUX(


Datapath <- (regwrite, alusrc, alucontrol[6], branch, regdst)
         
/* Définition des registres 32 bits */
         array pc[32];

         /* memory unit */
         array instr[32];
         instr[0..31] = LW(rom,pc[0..31]);

         /* multiplexeur */
         array writeReg[5];
         for(i,0,4) {
                    writeReg[i] = MUX(regdst, instr[i+11], instr[i+16]);
         }

         array regs[64];
         regs[0..63] = Regs <- (alu[0..31], writeReg[0..4], instr[21..25],
         instr[16..20], regwrite);

         /* multiplexeur pour le subi */
         array mult[32];
         for(i,0,15) {
                     mult[i] = MUX(alusrc, instr[i], regs[i+32]) ;
         }
         for(i,16,31) {
                     mult[i] = MUX(alusrc, instr[15], regs[i+32]) ;
         }

         array alu[32];
         alu[0..31] = Alu32bits <- (regs[0..31], mult[0..31],
         alucontrol[0..5]);

         /* calcul de l'adresse de l'instruction suivante */

         /* par défaut c'est zéro */
         array quatre[32];
         quatre[5] = TRUE;

         array addressplus[32];
         addressplus[0..31] = Fulladder <- (quatre[0..31], pc[0..31]);

         array sign_extend[32];
         for(i,0,15) {
                     sign_extend[i] = instr[i];
         }
         for(i,16,31) {
                      sign_extend[i] = instr[15];
         }

         array branch[32];
         branch([0..31]) = Fulladder <-
         (addressplus[0..31],sign_extend[32]);

         pc = MUX(zero[0],branch[0..31],addressplus[0..31]);

-> (zero[0], instr[0..5], instr[26..31])

