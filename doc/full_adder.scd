def
Input <-(a,b,c)
->(a,b,c)

Adder <-(a,b,c)
	t1 = a xor b;
	s = t1 xor c;
	t2 = a && b;
	t3 = t1 && c;
	r = t2 || t3;
->(s,r)

Output <-(a,b)
->(a,b)

end

Input inp <-(i.0,i.1,i.2)

Adder b <-(inp.0,inp.1,inp.2)

Output out <-(b.0,b.1)