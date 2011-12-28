def

Cm3 <-(a)
        t0 = a xor s;
        s = reg t0;
        t1 = a && s;
        t2 = r xor t1;
        r = reg t2;        
        t = a && r;
->(s,r,t)        

Cm2 <-(a)
        t0 = a xor s;
        s = reg t0;
        r = a && s;
->(s,r)

Output <-(a,b,c,d,e,f)
       a1 = a and true ;
       b1 = b and true ;
       c1 = c and true ;
       d1 = d and true ;
       e1 = e and true ;
       f1 = f and true ;
->(a1,b1,c1,d1,e1,f1)

end

Cm2 b <-()

Cm2 c <-(b[1])

Cm2 d <-(c[1])

Cm3 e <-(c[1])

Output out <-(b[0],c[0],d[0],e[0],e[1],e[2])
