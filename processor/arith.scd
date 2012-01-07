/* Additionneurs */

Fulladder <-(a,b,c)
	t1 = a xor b;
	s = t1 xor c;
	t2 = a && b;
	t3 = t1 && c;
	r = t2 || t3;
->(s,r)

/* Additionneur 32 bits série */
   
Adder32 <- (a[32],b[32],carryIn) 
	array s[32];
	array r[33];
	r[0] = carryIn AND true;
	for(i,0,31) {
	    array out[2];
	    out[0..1] = Fulladder(a[i],b[i],r[j]);
	    s[i] = out[0] AND true;
	    r[i+1] = out[1] AND true;
	    }
/* on gère pas les overflow */
-> (s,r[32])
