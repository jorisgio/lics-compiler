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

Start <-(a)
      
      array b[2];
      b[0..1] = Cm2 <- (a);

      array c[2];
      c[0..1] = Cm2 <-(b[1]);

      array d[2];
      d[0..1] = Cm2 <-(c[1]);

      array e[3];
      e[0..2] = Cm3 <-(d[1]);

->(b[0],c[0],d[0],e[0..2])
