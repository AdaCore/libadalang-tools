PRAGMA Ada_2022;

PROCEDURE test IS

   TYPE Matrix IS ARRAY (Integer RANGE <>, Integer RANGE <>) OF Real;
   TYPE Table IS ARRAY (1 .. 10) OF Integer;

   Empty_Matrix : CONSTANT Matrix := [];

   A    : Table := (7, 9, 5, 1, 3, 2, 4, 8, 6, 0);
   Abis : Table := [7, 9, 5, 1, 3, 2, 4, 8, 6, 0];

   B : Table := (2 | 4 | 10 => 1, OTHERS => 0);
   B : Table := [2 | 4 | 10 => 1, OTHERS => 0];

   C     : CONSTANT Matrix := (1 .. 5 => (1 .. 8 => 0.0));
   Cbis1 : CONSTANT Matrix := [1 .. 5 => (1 .. 8 => 0.0)];
   Cbis2 : CONSTANT Matrix := (1 .. 5 => [1 .. 8 => 0.0]);
   Cbis1 : CONSTANT Matrix := [1 .. 5 => [1 .. 8 => 0.0]];

   Text : String := ['a', 'b', 'c'];

   TYPE Mix_Code IS (ADD, SUB, MUL, LDA, STA, STZ);
   TYPE Mix_Code_bis IS (ADD, SUB, MUL, LDA, STA, STZ);

   FOR Mix_Code USE
     (ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24, STZ => 33);
   FOR Mix_Code_bis USE [ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24,
     STZ                     => 33];

BEGIN
   NULL;

END test;
