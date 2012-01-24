/* registre 32 bits */
Reg32 <-(inputs[32])
      array result[32];
      for(i, 0, 31) {
      	     result[i] = REG inputs[i];
	}
-> (result)

/* Unité registres */

Regs <-(datasIn[32],writeReg[5],readReg1[5],readReg2[5],controlWrite) 
     array regs[1024];
     array control[32];
     array dat[32];
     /* Ecriture des registres */ 
     control[0..31] = Demux5to32s1bit <- (writeReg[0..4]);
     for(i,0,31) {
     		 /* on vérifie si on doit écrire dans ce registre */
     		 w = controlWrite AND control[i];
		 /* Actualisation du registre,
		 si on doit écrire, on actualise le registre, sinon on garde son ancienne valeur */
		   for(j,0,31) {
		   	       dat[j] = MUX(w,datasIn[j], regs[i+j]);
			       regs[j+i] = REG dat[j];
		 }
		 
     /* Lecture des registres */
     array out1[32];
     array out2[32];
     out1[0..31] = Mux32to1s32(readReg1[5],regs[0..1023]);
     out2[0..31] = Mux32to1s32(readReg2[5],regs[0..1023]);
-> (out1[0..31],out2[0..31])