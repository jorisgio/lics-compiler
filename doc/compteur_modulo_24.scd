def

Cm3 <-(a)
        b = not a;

        x = reg t0;
        c = not x ;

        y = reg t1;
        d = not y;
        
        t2 = (c and d) and a ;
        t3 = b and x ;
        t0 = t2 or t3;
        t1 = (a and x) or (b and y);
        z = y and a;
->(x,y,z)

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
