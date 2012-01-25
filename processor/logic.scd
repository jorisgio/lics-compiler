/* inverseur */
Invert <- (a,b,ainvert,binvert)
     na = not a;
     nb = not b;
     a1 = mux(ainvert, a, na);
     b1 = mux(binvert, b, nb);
->(a1,b1)

/* 8 to 1 multiplexer, contrôle sur 3 bits, 8 entrées 1bit */ 

Mux8to1 <- (control[3],inputs8[8])
	n0 = mux(control[0],inputs8[1],inputs8[0]);
	n1 = mux(control[0],inputs8[3],inputs8[2]);
	n2 = mux(control[0],inputs8[5],inputs8[4]);
	n3 = mux(control[0],inputs8[7],inputs8[6]);
        m0 = mux(control[1],n1,n0);
        m1 = mux(control[1],n3,n2);
	s = mux(control[2],m1,m0);
-> (s) 
 

Mux4to1 <- (control[2],inputs4[4])
       m0 = mux(control[0],inputs4[1],inputs4[0]);
       m1 = mux(control[0],inputs4[3],inputs4[2]);
       s = mux(control[1],m1,m0);
-> (s)

Mux32to1 <- (control[5],inputs32[32]) 
	 array mux1[1];
	 array mux2[1];
	 array mux3[1];
	 array mux4[1];
	 array s[1];
	 mux1[0..0] = Mux8to1<-(control[0..2],inputs32[0..7]);
	 mux2[0..0] = Mux8to1<-(control[0..2],inputs32[8..15]);
	 mux3[0..0] = Mux8to1<-(control[0..2],inputs32[16..23]);
	 mux4[0..0] = Mux8to1<-(control[0..2],inputs32[24..31]);
	 s[0..0] = Mux4to1<-(control[3..4],mux4[0],mux3[0],mux2[0],mux1[0]);
-> (s[0])

/* il faut maintenant des entrées 32 bits */

Mux32to1s32 <- (control[5],inp[1024]) 
	    array s[32];
	    for(i,0,31) {
	    	
	        s[i..i] =
                 Mux32to1 <- (control[0..4],inp[i],inp[i+32],inp[i+64],inp[i+96],inp[i+128],inp[i+160],inp[i+192],inp[i+224],inp[i+256],inp[i+288],inp[i+320],inp[i+352],inp[i+384],inp[i+416],inp[i+448],inp[i+480],inp[i+512],inp[i+544],inp[i+576],inp[i+608],inp[i+640],inp[i+672],inp[i+704],inp[i+736],inp[i+768],inp[i+800],inp[i+832],inp[i+864],inp[i+896],inp[i+928],inp[i+960],inp[i+992]);
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
