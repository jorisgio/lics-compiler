/* Additionneurs */

Fulladder <-(a,b,c)
	t1 = a xor b;
	s = t1 xor c;
	t2 = a && b;
	t3 = t1 && c;
	r = t2 || t3;
->(s,r)

/* Zero */

Zero <-(a[32])
     array t[32];
     t[0] = a[0] && true;
     for(i,1,31) {
                 t[i] = a[i] or t[i-1];
                 }
     out = not t[31];
-> (out)

/* Additionneur 32 bits série */
   
Adder32 <- (a[32],b[32],carryIn) 
	array s[32];
	array r[33];
	r[0] = carryIn and true;
	for(i,0,31) {
	    array out[2];
	    out[0..1] = Fulladder <- (a[i],b[i],r[i]);
	    s[i] = out[0] and true;
	    r[i+1] = out[1] and true;
	    }
/* on gère pas les overflow */
-> (s[0..31])
