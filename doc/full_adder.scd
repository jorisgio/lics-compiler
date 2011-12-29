def

Adder <-(a,b,c)
	t1 = a xor b;
	s = t1 xor c;
	t2 = a && b;
	t3 = t1 && c;
	r = t2 || t3;
->(s,r)

end

<-(a,b,c)

Adder b <-(a,b,c)

->(b[0..1])
