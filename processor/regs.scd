/* registre 32 bits */
Reg32 <-(inputs[32])
      array result[32];
      for(i, 0, 31) {
      	     result[i] = reg inputs[i];
	}
-> (result[0..31])

/* Unité registres */

Regs <-(datasIn[32],writeReg[5],readReg1[5],readReg2[5],controlWrite, top) 
     array regs[1024];
     array control[32];
     /* Ecriture des registres */ 
     control[0..31] = Demux5to32s1bit <- (writeReg[0..4]);

     /* on fait en sorte d'éviter les for imbriqués */
     for(i,0,991) {
     		 /* on vérifie si on doit écrire dans ce registre */
     		 w = controlWrite and control[i/32];
		 /* Actualisation du registre,
		 si on doit écrire, on actualise le registre, sinon on garde son ancienne valeur */
		 
                 /* calcul du mod pour datasIn */
		 dat = mux(w,datasIn[i-(i/32)*32], regs[i]);
		 regs[i] = reg dat;
                 
     }
     
     /* on traite séparément le registre $31 qui contient le timestamp */
     array zero[32];
     for(i,0,31) {
                 zero[i] = false;
     }
     array regtmp[32];
     regtmp[0..31] = Adder32 <- (regs[992..1023],zero[0..31], top);
     regs[992..1023] = Reg32 <- (regtmp[0..31]);
		 
     /* Lecture des registres */
     array out1[32];
     array out2[32];
     out1[0..31] = Mux32to1s32 <- (readReg1[0..4],regs[0..1023]);
     out2[0..31] = Mux32to1s32 <- (readReg2[0..4],regs[0..1023]);

     /* pour les sorties */
     array sorties[448];
     for (i,0,31) {
     	  sorties[i] = out1[i] && true;
	  sorties[i+32] = out2[i] && true;
}	 

     for (i,0,63) {
         /*sorties[i] = regs[i+448] && true; */
         sorties[i+64] = regs[i + 544] && true;
         sorties[i+128] = regs[i + 608] && true;
         sorties[i+192] = regs[i + 352] && true;
         sorties[i+256] = regs[i + 224] && true;
         sorties[i+320] = regs[i + 64] && true;
         sorties[i+384] = regs[i + 128] && true;
     }
     
     
->(out1[0..31],out2[0..31],sorties[0..447])

/* on renvoit dans cet ordre $14, $15, $17, $18, $19, $20, $11, $12,
   $7, $8, $2, $3, $4, $5
   qui correspondent du chiffre de poids fort à celui de poids faible (en
   décimal) aux heures, minutes, secondes, jours, mois, années. */
