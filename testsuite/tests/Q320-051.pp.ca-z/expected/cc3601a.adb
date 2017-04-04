-- CC3601A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT PREDEFINED OPERATORS MAY BE PASSED AS ACTUAL GENERIC SUBPROGRAM
-- PARAMETERS (CHECKS FOR "=" AND "/=" ARE IN CC3601C).

-- R.WILLIAMS 10/9/86
-- JRL 11/15/95 Added unknown discriminant part to all formal
--                     private types.

with Report; use Report;
procedure Cc3601a is

   generic
      type T (<>) is private;
      V, V1 : T;
      Kind : String;
      with function F1 (X : in T) return T;
   package Gp1 is
      R : Boolean := F1 (V) = V1;
   end Gp1;

   package body Gp1 is
   begin
      if not (Ident_Bool (R)) then
         Failed ("INCORRECT VALUE FOR UNARY OP - " & Kind);
      end if;
   end Gp1;

   generic
      type T (<>) is private;
      V, V1, V2 : in T;
      Kind : String;
      with function F1 (P1 : in T; P2 : in T) return T;
   package Gp2 is
      R : Boolean := V /= F1 (V1, V2);
   end Gp2;

   package body Gp2 is
   begin
      if Ident_Bool (R) then
         Failed ("INCORRECT VALUE FOR BINARY OP - " & Kind);
      end if;
   end Gp2;

   generic
      type T1 (<>) is private;
      type T2 (<>) is private;
      V1 : T1;
      V2 : T2;
      Kind : String;
      with function F1 (X : in T1) return T2;
   package Gp3 is
      R : Boolean := F1 (V1) = V2;
   end Gp3;

   package body Gp3 is
   begin
      if not (Ident_Bool (R)) then
         Failed ("INCORRECT VALUE FOR OP - " & Kind);
      end if;
   end Gp3;

begin
   Test
     ("CC3601A",
      "CHECK THAT PREDEFINED OPERATORS MAY BE " &
      "PASSED AS ACTUAL GENERIC SUBPROGRAM " &
      "PARAMETERS");

   begin -- CHECKS WITH RELATIONAL OPERATORS AND LOGICAL OPERATORS AS
      -- ACTUAL PARAMETERS.

      for I1 in Boolean loop

         for I2 in Boolean loop
            Comment
              ("B1 = " &
               Boolean'Image (I1) &
               " AND " &
               "B2 = " &
               Boolean'Image (I2));
            declare
               B1 : Boolean := Ident_Bool (I1);
               B2 : Boolean := Ident_Bool (I2);

               package P1 is new Gp1
                 (Boolean,
                  not B2,
                  B2,
                  """NOT"" - 1",
                  "NOT");
               package P2 is new Gp2 (Boolean, B1 or B2, B1, B2, "OR", "OR");
               package P3 is new Gp2
                 (Boolean,
                  B1 and B2,
                  B2,
                  B1,
                  "AND",
                  "AND");
               package P4 is new Gp2 (Boolean, B1 /= B2, B1, B2, "XOR", "XOR");
               package P5 is new Gp2 (Boolean, B1 < B2, B1, B2, "<", "<");
               package P6 is new Gp2 (Boolean, B1 <= B2, B1, B2, "<=", "<=");
               package P7 is new Gp2 (Boolean, B1 > B2, B1, B2, ">", ">");
               package P8 is new Gp2 (Boolean, B1 >= B2, B1, B2, ">=", ">=");

               type Ab is array (Boolean range <>) of Boolean;
               Ab1 : Ab (Boolean)  := (B1, B2);
               Ab2 : Ab (Boolean)  := (B2, B1);
               T   : Ab (B1 .. B2) := (B1 .. B2 => True);
               F   : Ab (B1 .. B2) := (B1 .. B2 => False);
               Vb1 : Ab (B1 .. B1) := (B1 => B2);
               Vb2 : Ab (B2 .. B2) := (B2 => B1);

               package P9 is new Gp1 (Ab, Ab1, not Ab1, """NOT"" - 2", "NOT");
               package P10 is new Gp1 (Ab, T, F, """NOT"" - 3", "NOT");
               package P11 is new Gp1
                 (Ab,
                  Vb2,
                  (B2 => not B1),
                  """NOT"" - 4",
                  "NOT");
               package P12 is new Gp2
                 (Ab,
                  Ab1 and Ab2,
                  Ab1,
                  Ab2,
                  "AND",
                  "AND");
            begin
               null;
            end;
         end loop;
      end loop;
   end;

   declare -- CHECKS WITH ADDING AND MULTIPLYING OPERATORS, "**",
      -- AND "ABS".

      package P1 is new Gp1 (Integer, -4, -4, """+"" - 1", "+");

      package P2 is new Gp1 (Float, 4.0, 4.0, """+"" - 2", "+");

      package P3 is new Gp1 (Duration, -4.0, -4.0, """+"" - 3", "+");
      package P4 is new Gp1 (Integer, -4, 4, """-"" - 1", "-");

      package P5 is new Gp1 (Float, 0.0, 0.0, """-"" - 2", "-");

      package P6 is new Gp1 (Duration, 1.0, -1.0, """-"" - 3", "-");
      package P7 is new Gp2 (Integer, 6, 1, 5, """+"" - 1", "+");

      package P8 is new Gp2 (Float, 6.0, 1.0, 5.0, """+"" - 2", "+");
      package P9 is new Gp2 (Duration, 6.0, 1.0, 5.0, """+"" - 3", "+");
      package P10 is new Gp2 (Integer, 1, 6, 5, """-"" - 1", "-");
      package P11 is new Gp2 (Duration, 11.0, 6.0, -5.0, """-"" - 2", "-");
      package P12 is new Gp2 (Float, 1.0, 6.0, 5.0, """-"" - 3", "-");

      subtype Subint is Integer range 0 .. 2;
      type Str is array (Subint range <>) of Character;
      Vstr : Str (0 .. 1) := "AB";

      package P13 is new Gp2
        (Str,
         Vstr (0 .. 0) & Vstr (1 .. 1),
         Vstr (0 .. 0),
         Vstr (1 .. 1),
         """&"" - 1",
         "&");

      package P14 is new Gp2
        (Str,
         Vstr (1 .. 1) & Vstr (0 .. 0),
         Vstr (1 .. 1),
         Vstr (0 .. 0),
         """&"" - 2",
         "&");

      package P15 is new Gp2 (Integer, 0, -1, 0, """*"" - 1", "*");

      package P16 is new Gp2 (Float, 6.0, 3.0, 2.0, """*"" - 2", "*");
      package P17 is new Gp2 (Integer, 0, 0, 6, """/"" - 1", "/");

      package P18 is new Gp2 (Float, 3.0, 6.0, 2.0, """/"" - 2", "/");
      package P19 is new Gp2 (Integer, -1, -11, 5, "REM", "REM");

      package P20 is new Gp2 (Integer, 4, -11, 5, "MOD", "MOD");

      package P21 is new Gp1 (Integer, 5, 5, """ABS"" - 1", "ABS");

      package P22 is new Gp1 (Float, -5.0, 5.0, """ABS"" - 2", "ABS");

      package P23 is new Gp1 (Duration, 0.0, 0.0, """ABS"" - 3", "ABS");

      package P24 is new Gp2 (Integer, 9, 3, 2, """**"" - 1", "**");

      package P25 is new Gp2 (Integer, 1, 5, 0, """**"" - 2", "**");

   begin
      null;
   end;

   declare -- CHECKS WITH ATTRIBUTES.

      type Weekday is (Mon, Tues, Wed, Thur, Fri);

      package P1 is new Gp1 (Weekday, Tues, Wed, "WEEKDAY'SUCC", Weekday'Succ);

      package P2 is new Gp1 (Weekday, Tues, Mon, "WEEKDAY'PRED", Weekday'Pred);

      package P3 is new Gp3
        (Weekday,
         String,
         Thur,
         "THUR",
         "WEEKDAY'IMAGE",
         Weekday'Image);

      package P4 is new Gp3
        (String,
         Weekday,
         "FRI",
         Fri,
         "WEEKDAY'VALUE",
         Weekday'Value);
   begin
      null;
   end;

   Result;
end Cc3601a;
