def
Input <-(a)
->(a)

Cm2 <-(a)
	t0 = a xor s;
	s = reg t0;
	r = x && s;
->(s,r)

Output <-(a,b)
->(a,b)

end

Input inp <-(i.0)

Cm2 b <-(inp.0)

Output out <-(b[0..1])
