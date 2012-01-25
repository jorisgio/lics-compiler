        li      $1,    1        ;
resetcentaineannees:    
        li      $3,    0        ;
        add     $2,    $2,      $1 ;
        j       main               ;
resetdizainesannees:    
        li      $4,     0         ;
        add     $3,     $3,     $1 ;
        li      $25,    10         ;
        beq     $3,     $25,    resetcentaineannees ;
        j main                                      ;
resetannees:
        li      $5,     0           ;
        add     $4,     $4,     $1  ;
        li      $25,    10          ;
        beq     $4,     $25,    resetdizainesannees ;
        