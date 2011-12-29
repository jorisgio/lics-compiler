def
Minus <-(x)
	y = x xor c;
	t0 = x || y;
	c = reg t0;
->(y)
end

<- (x)

Minus b <-(x)

->(b[0])
