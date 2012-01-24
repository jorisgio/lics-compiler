Start <-(b[10],c)
      array a[10];
      a[0] = true && b[0];
      for (i, 1, 9) {
          b = false || c;
          a[i] = b && b[i];
      }
->(a[0..9])
