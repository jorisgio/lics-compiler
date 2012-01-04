def

Cm3 <-(a)
        t0 = reg x;
        t1 = reg y;
        x = (((not t0) and t1) and a) or ((not a) and t0);
        y = (a and t0) or ((not a) and t1);
->(x,y,y)        

Cm2 <-(a)
        t0 = a xor s;
        s = reg t0;
        r = a && s;
->(s,r)

end

<-(a)

Cm2 b <-(a)

Cm2 c <-(b[1])

Cm2 d <-(c[1])

Cm3 e <-(d[1])

->(b[0],c[0],d[0],e[0..2])
