def
Input <-(a)
->(a)

Minus <-(x)
	y = x xor c;
	t0 = x || y;
	c = reg t0;
->(y)

Output <-(a)
->(a)

end

Input inp <-(i.0)

Minus b <-(inp.0)

Output out <-(b.0)