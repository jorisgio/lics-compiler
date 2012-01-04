def

Cm30 <-(a)
     t1 = a xor s1;
     t2 = r1 xor s2;
     t3 = r2 xor s3;
     t4 = r3 xor s4;
     t5 = r4 xor s5;

     r1 = a && s1;
     r2 = r1 && s2;
     r3 = r2 && s3;
     r4 = r3 && s4;
     r5 = r4 && s5;

     ns2 = not s2;

     x = s1 && (ns2 && (s3 && (s4 && s5)));
     y = not x;

     s1 = reg t1;
     s2 = reg t2;
     s3 = reg t3;
     s4 = reg t4;
     s5 = reg t5;

     r = a && x;
->(s1,s2,s3,s4,s5,r)     
     
Cm60 <-(a)
     t1 = a xor s1;
     t2 = r1 xor s2;
     t3 = r2 xor s3;
     t4 = r3 xor s4;
     t5 = r4 xor s5;
     t6 = r5 xor s6;
     
     r1 = a && s1;
     r2 = r1 && s2;
     r3 = r2 && s3;
     r4 = r3 && s4;
     r5 = r4 && s5;
     r6 = r5 && s6;
     
     ns3 = not s3;
     
     x = s1 && (s2 && (ns3 && (s4 && (s5 && s6))));
     y = not x;
     
     tt1 = reg t1;
     tt2 = reg t2;
     tt3 = reg t3;
     tt4 = reg t4;
     tt5 = reg t5;
     tt6 = reg t6;
     
     s1 = y && tt1;
     s2 = y && tt2;
     s3 = y && tt3;
     s4 = y && tt4;
     s5 = y && tt5;
     s6 = y && tt6;

     r = a && x;
->(s1,s2,s3,s4,s5,s6,r)

end
