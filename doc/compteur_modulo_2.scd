def

Cm2 <-(a)
	t0 = a xor s;
	s = reg t0;
	r = a && s;
->(s,r)

end

<-(a)

Cm2 b <-(a)

->(b[0..1])
