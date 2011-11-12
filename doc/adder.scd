def
Input <-(a,b,c,d,e,f)
->(a,b,c,d,e,f)

Adder <-(a,b,c)
	t1 = a xor b;
	s = t1 xor c;
	co = (t1 and c) or (a and b);
->(s,co)

Output <-(a,b,c,d)
->(a,b,c,d)

end

Input inp <-(i.1,i.2,i.3,i.4,i.5,i.6)

Adder b1 <-(inp.0,inp.1,inp.5)

Adder b2 <-(inp.2,inp.3,b1.1)

Adder b3 <-(inp.4,inp.5,b2.1)

Output out <-(b1.0,b2.0,b3.0,b3.1)
