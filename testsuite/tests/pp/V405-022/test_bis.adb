pragma Ada_2022;

procedure test_bis is

 type Matrix is array(Integer  range <>, Integer range <>) of Real;
 type Table is array(1 .. 10) of Integer;

 Empty_Matrix : constant Matrix := [];

 A : Table := (7, 9, 5, 1, 3, 2, 4, 8, 6, 0);
 Abis : Table := [7, 9, 5, 1, 3, 2, 4, 8, 6, 0];

 B : Table := (2 | 4 | 10 => 1, others => 0);
 B : Table := [2 | 4 | 10 => 1, others => 0];

 C : constant Matrix := (1 .. 5 => (1 .. 8 => 0.0));
 Cbis1 : constant Matrix := [1 .. 5 => (1 .. 8 => 0.0)];
 Cbis2 : constant Matrix := (1 .. 5 => [1 .. 8 => 0.0]);
 Cbis1 : constant Matrix := [1 .. 5 => [1 .. 8 => 0.0]];

 Text : String := ['a', 'b', 'c'];

 type Mix_Code is (ADD, SUB, MUL, LDA, STA, STZ);
 type Mix_Code_bis is (ADD, SUB, MUL, LDA, STA, STZ);

for Mix_Code use
   (ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24, STZ =>33);
for Mix_Code_bis use
   [ADD => 1, SUB => 2, MUL => 3, LDA => 8, STA => 24, STZ =>33];




begin
null; 

end test_bis;
