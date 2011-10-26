start block1;
def
   Adder <- (i1,i2)
	s1 = i1 && i2;
        -> (s1)

   Bidule1 <- (z1, z2, z3, z4)
	s1 = z1 && z2 || (z3 xor z4);
	-> (s1)
end

block1
