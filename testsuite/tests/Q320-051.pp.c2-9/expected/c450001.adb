------------------------------------------------------------------- C450001

with Report;
with C450001_0;
with Tctouch;
procedure C450001 is
   use C450001_0;

   Br : constant String := " produced the wrong result";

   procedure Is_T (B : Boolean; S : String) renames Tctouch.Assert;
   procedure Is_F (B : Boolean; S : String) renames Tctouch.Assert_Not;

   Whole_8_A, Whole_8_B, Whole_8_C : C450001_0.Unsigned_8_Bit;

   Short_8_A, Short_8_B, Short_8_C : C450001_0.Unsigned_Edge_8;

   Over_8_A, Over_8_B, Over_8_C : C450001_0.Unsigned_Over_8;

begin  -- Main test procedure. C450001

   Report.Test
     ("C450001",
      "Check that operations on modular types " & "perform correctly.");

   -- the cases for the whole 8 bit type are pretty simple

   Whole_8_A := 2#0000_0000#;
   Whole_8_B := 2#1111_1111#;

   Is_T ((Id (Whole_8_A) and Id (Whole_8_B)) = 2#0000_0000#, "8 bit and" & Br);
   Is_T ((Id (Whole_8_A) or Id (Whole_8_B)) = 2#1111_1111#, "8 bit  or" & Br);
   Is_T ((Id (Whole_8_A) xor Id (Whole_8_B)) = 2#1111_1111#, "8 bit xor" & Br);

   Whole_8_A := 2#0000_1111#;
   Whole_8_B := 2#1111_1111#;

   Is_T ((Id (Whole_8_A) and Id (Whole_8_B)) = 2#0000_1111#, "8 bit and" & Br);
   Is_T ((Id (Whole_8_A) or Id (Whole_8_B)) = 2#1111_1111#, "8 bit  or" & Br);
   Is_T ((Id (Whole_8_A) xor Id (Whole_8_B)) = 2#1111_0000#, "8 bit xor" & Br);

   Whole_8_A := 2#1010_1010#;
   Whole_8_B := 2#1111_0000#;

   Is_T ((Id (Whole_8_A) and Id (Whole_8_B)) = 2#1010_0000#, "8 bit and" & Br);
   Is_T ((Id (Whole_8_A) or Id (Whole_8_B)) = 2#1111_1010#, "8 bit  or" & Br);
   Is_T ((Id (Whole_8_A) xor Id (Whole_8_B)) = 2#0101_1010#, "8 bit xor" & Br);

   -- the cases for the partial 8 bit type involve subtracting the modulus
   -- from results that exceed the modulus.
   -- hence, any of the following operations that exceed 2#11111110# must
   -- have 2#11111111# subtracted from the result; i.e. where you would
   -- expect to see 2#11111111# as in the above operations, the correct
   -- result will be 2#00000000#.  Note that 2#11111111# is not a legal
   -- value of type C450001_0.Unsigned_Edge_8.

   Short_8_A := 2#1110_0101#;
   Short_8_B := 2#0001_1111#;

   Is_T
     ((Id (Short_8_A) and Id (Short_8_B)) = 2#0000_0101#,
      "8 short and 1" & Br);
   Is_T
     ((Id (Short_8_A) or Id (Short_8_B)) = 2#0000_0000#,
      "8 short  or 1" & Br);
   Is_T
     ((Id (Short_8_A) xor Id (Short_8_B)) = 2#1111_1010#,
      "8 short xor 1" & Br);

   Short_8_A := 2#1111_0000#;
   Short_8_B := 2#1111_1110#;

   Is_T
     ((Id (Short_8_A) and Id (Short_8_B)) = 2#1111_0000#,
      "8 short and 2" & Br);
   Is_T
     ((Id (Short_8_A) or Id (Short_8_B)) = 2#1111_1110#,
      "8 short  or 2" & Br);
   Is_T
     ((Id (Short_8_A) xor Id (Short_8_B)) = 2#0000_1110#,
      "8 short xor 2" & Br);

   Short_8_A := 2#1010_1010#;
   Short_8_B := 2#0101_0101#;

   Is_T
     ((Id (Short_8_A) and Id (Short_8_B)) = 2#0000_0000#,
      "8 short and 3" & Br);
   Is_T
     ((Id (Short_8_A) or Id (Short_8_B)) = 2#0000_0000#,
      "8 short  or 3" & Br);
   Is_T
     ((Id (Short_8_A) xor Id (Short_8_B)) = 2#0000_0000#,
      "8 short xor 3" & Br);

   Short_8_A := 2#1010_1010#;
   Short_8_B := 2#1111_1110#;

   Is_T
     ((Id (Short_8_A) and Id (Short_8_B)) = 2#1010_1010#,
      "8 short and 4" & Br);
   Is_T
     ((Id (Short_8_A) or Id (Short_8_B)) = 2#1111_1110#,
      "8 short  or 4" & Br);
   Is_T
     ((Id (Short_8_A) xor Id (Short_8_B)) = 2#0101_0100#,
      "8 short xor 4" & Br);

   -- the cases for the over 8 bit type have similar issues to the short type
   -- however the bit patterns are a little different.  The rule is to subtract
   -- the modulus (258) from any resulting value equal or greater than the
   -- modulus       -- note that 258 =    2#100000010#

   Over_8_A := 2#1_0000_0000#;
   Over_8_B := 2#0_1111_1111#;

   Is_T
     ((Id (Over_8_A) and Id (Over_8_B)) = 2#0_0000_0000#,
      "8 over and" & Br);
   Is_T ((Id (Over_8_A) or Id (Over_8_B)) = 2#0_1111_1101#, "8 over  or" & Br);
   Is_T
     ((Id (Over_8_A) xor Id (Over_8_B)) = 2#0_1111_1101#,
      "8 over xor" & Br);

   Over_8_A := 2#1_0000_0001#;
   Over_8_B := 2#0_1111_1111#;

   Is_T
     ((Id (Over_8_A) and Id (Over_8_B)) = 2#0_0000_0001#,
      "8 over and" & Br);
   Is_T ((Id (Over_8_A) or Id (Over_8_B)) = 2#0_1111_1101#, "8 over  or" & Br);
   Is_T
     ((Id (Over_8_A) xor Id (Over_8_B)) = 2#0_1111_1100#,
      "8 over xor" & Br);

   Whole_8_A := 128;
   Whole_8_B := 255;

   Is_T (Id (Whole_8_A) /= Id (Whole_8_B), "8 /=" & Br);
   Is_F (Id (Whole_8_A) = Id (Whole_8_B), "8  =" & Br);

   Is_T (Id (Whole_8_A) <= Id (Whole_8_B), "8 <=" & Br);
   Is_T (Id (Whole_8_A) < Id (Whole_8_B), "8 < " & Br);

   Is_F (Id (Whole_8_A) >= Id (Whole_8_B), "8 >=" & Br);
   Is_T (Id (Whole_8_A) > Id (Whole_8_B + 7), "8 > " & Br);

   Is_T (Id (Whole_8_A) in Id (100) .. Id (200), "8 in" & Br);
   Is_F (Id (Whole_8_A) not in Id (100) .. Id (200), "8 not in" & Br);

   Is_F (Id (Whole_8_A) in Id (200) .. Id (250), "8 in" & Br);
   Is_T (Id (Whole_8_A) not in Id (200) .. Id (250), "8 not in" & Br);

   Short_8_A := 127;
   Short_8_B := 254;

   Is_T (Id (Short_8_A) /= Id (Short_8_B), "short 8 /=" & Br);
   Is_F (Id (Short_8_A) = Id (Short_8_B), "short 8  =" & Br);

   Is_T (Id (Short_8_A) <= Id (Short_8_B), "short 8 <=" & Br);
   Is_T (Id (Short_8_A) < Id (Short_8_B), "short 8 < " & Br);

   Is_F (Id (Short_8_A) >= Id (Short_8_B), "short 8 >=" & Br);
   Is_F (Id (Short_8_A) > Id (Short_8_B), "short 8 > " & Br);

   Is_T (Id (Short_8_A) in Id (100) .. Id (200), "8 in" & Br);
   Is_F (Id (Short_8_A) not in Id (100) .. Id (200), "8 not in" & Br);

   Is_F (Id (Short_8_A) in Id (200) .. Id (250), "8 in" & Br);
   Is_T (Id (Short_8_A) not in Id (200) .. Id (250), "8 not in" & Br);

   Whole_8_A := 1;
   Whole_8_B := 254;
   Short_8_A := 1;
   Short_8_B := 2;

   Whole_8_C := Id (Whole_8_A) + Id (Whole_8_B);
   Is_T (Whole_8_C = C450001_0.Unsigned_8_Bit'Last, "8 binary + 1" & Br);

   Whole_8_C := Whole_8_C + Id (Whole_8_A);
   Is_T (Whole_8_C = C450001_0.Unsigned_8_Bit'First, "8 binary + 2" & Br);

   Whole_8_C := Id (Whole_8_A) - Id (Whole_8_A);
   Is_T (Whole_8_C = 0, "8 binary -" & Br);

   Whole_8_C := Whole_8_C - Id (Whole_8_A);
   Is_T (Whole_8_C = C450001_0.Unsigned_8_Bit'Last, "8 binary + 3" & Br);

   Short_8_C := Id (Short_8_A) + Id (C450001_0.Unsigned_Edge_8'Last);
   Is_T (Short_8_C = C450001_0.Unsigned_Edge_8'First, "Short binary + 1" & Br);

   Short_8_C := Short_8_A + Id (Short_8_A);
   Is_T (Short_8_C = Id (Short_8_B), "Short binary + 2" & Br);

   Short_8_C := Id (Short_8_A) - Id (Short_8_A);
   Is_T (Short_8_C = 0, "Short 8 binary -" & Br);

   Short_8_C := Short_8_C - Id (Short_8_A);
   Is_T (Short_8_C = C450001_0.Unsigned_Edge_8'Last, "Short binary + 3" & Br);

   Whole_8_C := (+Id (Whole_8_B));
   Is_T (Whole_8_C = 254, "8 unary +" & Br);

   Whole_8_C := (-Id (Whole_8_A));
   Is_T (Whole_8_C = C450001_0.Unsigned_8_Bit'Last, "8 unary -" & Br);

   Whole_8_C := (-Id (0));
   Is_T (Whole_8_C = 0, "8 unary -0" & Br);

   Short_8_C := (+Id (C450001_0.Unsigned_Edge_8'Last));
   Is_T (Short_8_C = 254, "Short 8 unary +" & Br);

   Short_8_C := (-Id (Short_8_A));
   Is_T (Short_8_C = C450001_0.Unsigned_Edge_8'Last, "Short 8 unary -" & Br);

   Whole_8_A := 20;
   Whole_8_B := 255;

   Whole_8_C :=
     Id (Whole_8_A) * Id (Whole_8_B); -- 5100 = 19*256 + 236 (256-20)
   Is_T (Whole_8_C = 236, "8 *" & Br);

   Short_8_A := 9;
   Short_8_B := 254;

   Short_8_C := Id (Short_8_A) * Id (Short_8_B); -- 2286 = 8*255 + 246 (255-9)
   Is_T (Short_8_C = 246, "short 8 *" & Br);

   Over_8_A := 12;
   Over_8_B := 86;

   Over_8_C := Id (Over_8_A) * Id (Over_8_B); -- 1032 = 4*258 + 0
   Is_T (Over_8_C = 0, "over 8 *" & Br);

   Whole_8_A := 255;
   Whole_8_B := 4;

   Whole_8_C := Id (Whole_8_A) / Id (Whole_8_B);
   Is_T (Whole_8_C = 63, "8 /" & Br);

   Short_8_A := 253;
   Short_8_B := 127;

   Short_8_C := Id (Short_8_A) / Id (Short_8_B);
   Is_T (Short_8_C = 1, "short 8 / 1" & Br);

   Short_8_C := Id (Short_8_A) / Id (126);
   Is_T (Short_8_C = 2, "short 8 / 2" & Br);

   Whole_8_A := 255;
   Whole_8_B := 254;

   Whole_8_C := Id (Whole_8_A) rem Id (Whole_8_B);
   Is_T (Whole_8_C = 1, "8 rem" & Br);

   Short_8_A := 222;
   Short_8_B := 111;

   Short_8_C := Id (Short_8_A) rem Id (Short_8_B);
   Is_T (Short_8_C = 0, "short 8 rem" & Br);

   Whole_8_A := 99;
   Whole_8_B := 9;

   Whole_8_C := Id (Whole_8_A) mod Id (Whole_8_B);
   Is_T (Whole_8_C = 0, "8 mod" & Br);

   Short_8_A := 254;
   Short_8_B := 250;

   Short_8_C := Id (Short_8_A) mod Id (Short_8_B);
   Is_T (Short_8_C = 4, "short 8 mod" & Br);

   Whole_8_A := 99;

   Whole_8_C := abs Whole_8_A;
   Is_T (Whole_8_C = Id (99), "8 abs" & Br);

   Short_8_A := 254;

   Short_8_C := Id (abs Short_8_A);
   Is_T (Short_8_C = 254, "short 8 abs" & Br);

   Whole_8_B := 2#0000_1111#;

   Whole_8_C := not Whole_8_B;
   Is_T (Whole_8_C = Id (2#1111_0000#), "8 not" & Br);

   Short_8_B := 2#0000_1111#;                      -- 15

   Short_8_C := Id (not Short_8_B);                  -- 254 - 15
   Is_T (Short_8_C = 2#1110_1111#, "short 8 not" & Br); -- 239

   Whole_8_A := 2;

   Whole_8_C := Whole_8_A**7;
   Is_T (Whole_8_C = Id (128), "2 ** 7, whole 8" & Br);

   Whole_8_C := Whole_8_A**9;
   Is_T (Whole_8_C = Id (0), "2 ** 9, whole 8" & Br);

   Short_8_A := 4;

   Short_8_C := Id (Short_8_A)**4;
   Is_T (Short_8_C = 1, "4 ** 4, short" & Br);

   Over_8_A := 4;

   Over_8_C := Id (Over_8_A)**4;
   Is_T (Over_8_C = 256, "4 ** 4, over" & Br);

   Over_8_C := Id (Over_8_A)**5; -- 1024 = 3*258 + 250
   Is_T (Over_8_C = 250, "4 ** 5, over" & Br);

   C450001_0.Loop_Check;

   Report.Result;

end C450001;
