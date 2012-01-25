/* inverseur */
Invert <- (a,b,ainvert,binvert)
     na = not a;
     nb = not b;
     a1 = mux(ainvert, a, na);
     b1 = mux(binvert, b, nb);
->(a1,b1)

/* 8 to 1 multiplexer, contrôle sur 3 bits, 8 entrées 1bit */ 

Mux8to1 <- (control[3],inputs[8])
	n0 = mux(control[0],inputs[1],inputs[0]);
	n1 = mux(control[0],inputs[3],inputs[2]);
	n2 = mux(control[0],inputs[5],inputs[4]);
	n3 = mux(control[0],inputs[7],inputs[6]);
	s = mux(control[2],mux(control[1],n3,n2),mux(control[1],n1,n0));
-> (s) 
 

Mux4to1 <- (control[2],inputs[4])
       s = mux(control[1],mux(control[0],inputs[3],inputs[2]),mux(control[0],inputs[1],inputs[0]));
-> (s)

Mux32to1 <- (control[5],inputs[32]) 
	 array mux1[1];
	 array mux2[1];
	 array mux3[1];
	 array mux4[1];
	 array s[1];
	 mux1[0..0] = Mux8to1<-(control[0..2],inputs[0..7]);
	 mux2[0..0] = Mux8to1<-(control[0..2],inputs[8..15]);
	 mux3[0..0] = Mux8to1<-(control[0..2],inputs[16..23]);
	 mux4[0..0] = Mux8to1<-(control[0..2],inputs[24..31]);
	 s[0..0] = Mux4to1<-(control[3..4],mux4[0],mux3[0],mux2[0],mux1[0]);
-> (s[0])

/* il faut maintenant des entrées 32 bits */

Mux32to1s32 <- (control[5],inputs[1024]) 
	    array s[32];
	    for(i,0,31) {
	    	
	        s[i..i] =
                 Mux32to1 <- (control[0..4],inputs[i],inputs[i+32],inputs[i+64],inputs[i+96],inputs[i+128],inputs[i+160],inputs[i+192],inputs[i+224],inputs[i+256],inputs[i+288],inputs[i+320],inputs[i+352],inputs[i+384],inputs[i+416],inputs[i+448],inputs[i+480],inputs[i+512],inputs[i+544],inputs[i+576],inputs[i+608],inputs[i+640],inputs[i+672],inputs[i+704],inputs[i+736],inputs[i+768],inputs[i+800],inputs[i+832],inputs[i+864],inputs[i+896],inputs[i+928],inputs[i+960],inputs[i+992]);
                 }
-> (s[0..31])

/* décodeurs */

Demux1to2 <-(a)
	  s1 = not a;
          s2 = a && true;
-> (s1,s2)

Demux2to4 <-(a,b) 
	  a1 = not a;
	  b1 = not b;
	  d0 = a1 and b1;
	  d1 = a and b1;
	  d2 = a1 and b;
	  d3 = a and b ;
-> (d0,d1,d2,d3)

Demux3to8 <-(a[3])
          array b[4];
          b[0..3]=Demux2to4<-(a[0..1]);
          c = not a[2];
          array d[8];
          for(i,0,3) {
          d[i] = b[i] && c;
          }
          for(i,0,3) {
          d[i+4] = b[i] && a[2];
          }
->(d[0..7])

Demux4to16 <-(a[4])
           array b[8];
           b[0..7]=Demux3to8<-(a[0..2]);
           c = not a[3];
           array d[16];
           for(i,0,7) {
           d[i] = b[i] && c;
           }
           for(i,0,7) {
           d[i+8] = b[i] && a[3];
           }
->(d[0..15])

Demux5to32s1bit <-(a[5])
           array b[16];
           b[0..15]=Demux4to16<-(a[0..3]);
           c = not a[4];
           array d[32];
           for(i,0,15) {
           d[i] = b[i] && c;
           }
           for(i,0,15) {
           d[i+16] = b[i] && a[4];
           }
->(d[0..31])
