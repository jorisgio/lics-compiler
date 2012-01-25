main:   
        li      $1,    1        ;
resetcentaineannees:    
        li      $3,    0        ;
        add     $2,    $2,      $1 ;
        j       affiche            ;
resetdizainesannees:    
        li      $4,     0         ;
        add     $3,     $3,     $1 ;
        li      $25,    10         ;
        beq     $3,     $25,    resetcentaineannees ;
        j       affiche                             ;
resetannees:
        li      $5,     0           ;
        add     $4,     $4,     $1  ;
        li      $25,    10          ;
        beq     $4,     $25,    resetdizainesannees ;
        j       affiche                             ;
resetmois:      
        li      $7,     0                                 ;
        li      $8,     1                                 ;
        li      $9,     1                                 ;
        add     $6,     $6,     $1                        ;
        li      $25,    4                                 ;
        beq     $6,     $25     bissextile                ;
        li      $21,    0                                 ;
        j       apresbissextile                           ;
bissextile:
        li      $21,    1       ;
        li      $6,     0        ;
apresbissextile:
        add     $5,     $5,     $1      ;
        li      $25,    10              ;
        beq     $5,     $25,    resetannees ;
        j       affiche                     ;
resetunitesmois:
        li      $7,     0       ;
        add     $8,     $8,     $1 ;
        j       affiche            ;
resetjours:
        li      $11,    0       ;
        li      $12,    1       ;
        li      $13,    1       ;
        add     $9,     $9,     $1 ;
        add     $8,     $8,     $1 ;
        li      $25,    2          ;
        beq     $9,     $25,    fevrier ;
        li      $25,    4               ;
        beq     $9,     $25,    moiscourt ;
        li      $25,    6                 ;
        beq     $9,     $25,    moiscourt ;
        li      $25,    9                 ;
        beq     $9,     $25,    moiscourt ;
        li      $25,    11                ;
        beq     $9,     $25,    moiscourt ;
        li      $10,    31                ;
        j       updatedjours              ;
fevrier:
        li      $10,    28      ;
        add     $10,    $10,    $21 ;
        j       updatedjours        ;
moiscourt:
        li      $10,    30      ;
updatedjours:
        li      $25,    13      ;
        beq     $9,     $25,    resetmois ;
        li      $25,    10                ;
        beq     $8,     $25,    resetunitesmois ;
        j       affiche                         ;
resetunitesjours:
        li      $12,    0       ;
        add     $11,    $11,    $1 ;
        j       affiche            ;
resetheures:
        li      $14,    0       ;
        li      $15,    0       ;
        li      $16,    0       ;
        add     $13,    $13,    $1 ;
        add     $12,    $12,    $1 ;
        beq     $13,    $10,    resetjours ;
        li      $25,    10                 ;
        beq     $12,    $25,    resetunitesjours ;
        j       affiche                          ;
resetunitesheures:
        li      $15,    0       ;
        add     $14,    $14,    $1 ;
        j       affiche            ;
resetminutes:
        li      $17,    0       ;
        add     $16,    $16,    $1 ;
        add     $15,    $15,    $1 ;
        li      $25,    24         ;
        beq     $16,    $25,    resetheures ;
        li      $25,    10                  ;
        beq     $15,    $25,    resetunitesheures ;
        j       affiche                           ;
resetunitesminutes:
        li      $18,    0       ;
        add     $17,    $17,    $1 ;
        li      $25,    6          ;
        beq     $17,    $25,    resetminutes ;
        j       affiche                      ;
resetsecondes:
        li      $19,    0       ;
        add     $18,    $18,    $1 ;
        li      $25,    10         ;
        beq     $18,    $25,    resetunitesminutes ;
        j       affiche                            ;
resetunitessecondes:
        li      $20,    0       ;
        add     $19,    $19,    $1 ;
        li      $25,    6          ;
        beq     $19,    $25,    resetsecondes ;
        j       affiche                       ;
incremente:
        add     $20,    $20,    $1 ;
        li      $25,    10         ;
        beq     $20,    $25,    resetunitessecondes ;
affiche:
        beq     $30,    $31,    affiche ;
        li      $25,    0               ;
        add     $30,    $31,    $25     ;
        j       incremente      ;