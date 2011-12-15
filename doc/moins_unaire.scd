def
Minus <-(x)
	y = x xor c;
	t0 = x || y;
	c = reg t0;
->(y)
end

Minus b <-()
