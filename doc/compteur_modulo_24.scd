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

Start <-(a)

      array b[2];
      b[0..1] = Cm2 <-(a);

      array c[2];
      c[0..1] = Cm2 <-(b[1]);

      array d[2];
      d[0..1] = Cm2 <-(c[1]);

      array e[3];
      e[0..2] = Cm3 <-(d[1]);

->(b[0],c[0],d[0],e[0..2])
