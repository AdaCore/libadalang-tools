
PACKAGE BODY C83025C_PACK IS
     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

     FUNCTION F IS NEW GEN_FUN (INTEGER, OBJ);

     FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

     PROCEDURE INNER (X : IN OUT INTEGER) IS SEPARATE;

     PROCEDURE INNER2 (X : IN INTEGER := C83025C_PACK.A;
                       A : IN OUT INTEGER) IS SEPARATE;

     FUNCTION INNER3 (X : INTEGER;
                      Z : ENUM := Y) RETURN INTEGER IS SEPARATE;

     FUNCTION INNER4 (X : INTEGER;
                      Z : ENUM := Y) RETURN INTEGER IS SEPARATE;

     PROCEDURE INNER5 (X : IN OUT INTEGER; F : IN FLOAT;
                       Z : CHARACTER := Y) IS SEPARATE;
END C83025C_PACK;