def

Cm3 <-(a)
        t0 = reg x;
        t1 = reg y;
        t2 = t0 || t1;
        t3 = not t2;
        x = mux(a, t0, t3);
        y = mux(a, t1, t0);
->(x,y,t1)        

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
