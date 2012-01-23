Test <- ()
     array b[10];
     b[0] = false;
     a = true;
-> (b[2..4])

Start <- ()
      array b[3];
      b[0..2] = Test <-();
-> (b[0])
