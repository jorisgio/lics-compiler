/* inverseur */
Invert <- (a,b,ainvert,binvert)
     a1 = MUX(ainvert, a, NOT a);
     b1 = MUX(binvert, b, NOT b);
     ->(a1,b1)

/* 8 to 1 multiplexer, contrôle sur 3 bits, 8 entrées 1bit */ 

Mux8to1 <- (control[3],inputs[8])
	n0 = MUX(control[0],inputs[0],inputs[1]);
	n1 = MUX(control[0],inputs[2],inputs[3]);
	n2 = MUX(control[0],inputs[4],inputs[5]);
	n3 = MUX(control[0],inputs[6],inputs[7]);
	s = MUX(control[2],MUX(control[1],n0,n1),MUX(control[1],n2,n3));
-> (s) 
 

Mux4t1 <- (control[2],inputs[4])
       s = MUX(control[1],MUX(control[0],inputs[0],inputs[1]),MUX(control[0],inputs[2],inputs[3]));
-> (s)

Mux32to1 <- (control[5],inputs[32]) 
	 array mux1[1];
	 array mux2[1];
	 array mux3[1];
	 array mux4[];
	 array s[1];
	 mux1[0..0] = Mux8to1<-(control[0..2],inputs[0..7]);
	 mux2[0..0] = Mux8to1<-(control[0..2],inputs[8..15]);
	 mux3[0..0] = Mux8to1<-(control[0..2],inputs[16..23]);
	 mux4[0..0] = Mux8to1<-(control[0..2],inputs[24..31]);
	 s[0..0] = Mux4to1<-(control[3..4],mux1[0],mux2[0],mux3[0],mux4[0]);
-> (s[0])

/* il faut maintenant des entrées 32 bits */

Mux32to1s32 <- (control[5],inputs[1024]) 
	    array s[32];
	    for(i,0,31) {
	    	
	        s[i..i] = Mux32to1(control[0..4],inputs[)..(31 * i+1)]);
		}
-> (s[0..31])

/* décodeurs */

Demux1to2 <-(a)
	  s1 = NOT a;
	  s2 = a;
-> (s1,s2)

Demux2to4 <-(a,b) 
	  a1 = NOT a;
	  b1 = NOT b;
	  d0 = a1 AND b1;
	  d1 = a AND b1;
	  d2 = a1 AND b;
	  d3 = a AND b ;
-> (d0,d1,d2,d3)

Demux3to8decoder
