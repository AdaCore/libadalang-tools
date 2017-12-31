-- C52102B.ADA

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
-- CHECK THAT THE ASSIGNMENT OF OVERLAPPING SOURCE AND TARGET VARIABLES
--    (INCLUDING ARRAYS AND SLICES IN VARIOUS COMBINATIONS) SATISFIES
--    THE SEMANTICS OF "COPY" ASSIGNMENT.  (THIS TEST IS IN TWO PARTS,
--    COVERING RESPECTIVELY STATIC AND DYNAMIC BOUNDS.)

-- PART 2:  DYNAMIC BOUNDS

-- RM 02/27/80
-- SPS 2/18/83
-- JBG 3/15/84
-- JBG 6/9/84

with Report;
procedure C52102b is

   use Report;
   Ident_Int_0 : Integer := Ident_Int (0);
   Ident_Int_1 : Integer := Ident_Int (1);
   Ident_Int_2 : Integer := Ident_Int (2);
   Ident_Int_3 : Integer := Ident_Int (3);
   Ident_Int_4 : Integer := Ident_Int (4);
   Ident_Int_5 : Integer := Ident_Int (5);
   Ident_Int_6 : Integer := Ident_Int (6);
   Ident_Int_8 : Integer := Ident_Int (8);
   Ident_Int_9 : Integer := Ident_Int (9);

begin

   Test
     ("C52102B",
      "CHECK THAT THE ASSIGNMENT OF OVERLAPPING " &
      "SOURCE AND TARGET VARIABLES (INCLUDING " &
      "ARRAYS AND SLICES IN VARIOUS COMBINATIONS) " &
      "SATISFIES THE SEMANTICS OF ""COPY"" " &
      "ASSIGNMENT (PART 2: DYNAMIC BOUNDS)");

   -------------------------------------------------------------------
   --------------------  ARRAYS OF INTEGERS  -------------------------

   declare
      A : array (1 .. Ident_Int_4) of Integer;

   begin
      A := (11, 12, 13, 14);
      A := (1, A (Ident_Int_1), A (Ident_Int_2), A (Ident_Int_1));
      if A /= (1, 11, 12, 11) then
         Failed ("WRONG VALUES  -  I1");
      end if;

      A := (11, 12, 13, 14);
      A := (A (Ident_Int_4), A (Ident_Int_3), A (Ident_Int_4), 1);
      if A /= (14, 13, 14, 1) then
         Failed ("WRONG VALUES  -  I2");
      end if;

   end;

   declare
      A : array (-4 .. Ident_Int_4) of Integer;

   begin
      A                     := (-4, -3, -2, -1, 100, 1, 2, 3, 4);
      A (-4 .. Ident_Int_0) := A (Ident_Int_0 .. 4);
      if A /= (100, 1, 2, 3, 4, 1, 2, 3, 4) then
         Failed ("WRONG VALUES  -  I3");
      end if;

      A                    := (-4, -3, -2, -1, 100, 1, 2, 3, 4);
      A (Ident_Int_0 .. 4) := A (-4 .. Ident_Int_0);
      if A /= (-4, -3, -2, -1, -4, -3, -2, -1, 100) then
         Failed ("WRONG VALUES  -  I4");
      end if;

   end;

   declare
      type Arr is array (Integer range <>) of Integer;
      A : Arr (1 .. 10);

   begin
      A := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
      A :=
        0 & A (Ident_Int_1 .. Ident_Int_2) & A (Ident_Int_1 .. Ident_Int_2) &
        A (Ident_Int_1 .. Ident_Int_5);
      if A /= (0, 1, 2, 1, 2, 1, 2, 3, 4, 5) then
         Failed ("WRONG VALUES  -  I5");
      end if;

      A := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
      A :=
        A (Ident_Int_6 .. Ident_Int_9) & A (Ident_Int_8 .. Ident_Int_9) &
        A (Ident_Int_8 .. Ident_Int_9) & 0 & 0;
      if A /= (6, 7, 8, 9, 8, 9, 8, 9, 0, 0) then
         Failed ("WRONG VALUES  -  I6");
      end if;

   end;

   -------------------------------------------------------------------
   --------------------  ARRAYS OF BOOLEANS  -------------------------

   declare
      A : array (1 .. 4) of Boolean;

   begin
      A := (False, True, True, False);
      A := (True, A (Ident_Int_1), A (Ident_Int_2), A (Ident_Int_1));
      if A /= (True, False, True, False) then
         Failed ("WRONG VALUES  -  B1");
      end if;

      A := (False, True, True, False);
      A := (A (Ident_Int_4), A (Ident_Int_3), A (Ident_Int_4), True);
      if A /= (False, True, False, True) then
         Failed ("WRONG VALUES  -  B2");
      end if;

   end;

   declare
      A : array (-Ident_Int_4 .. 4) of Boolean;

   begin
      A := (False, False, False, False, False, True, True, True, True);
      A (-Ident_Int_4 .. Ident_Int_0) := A (Ident_Int_0 .. 4);
      if A /= (False, True, True, True, True, True, True, True, True) then
         Failed ("WRONG VALUES  -  B3");
      end if;

      A := (False, False, False, False, True, True, True, True, True);
      A (Ident_Int_0 .. 4) := A (-4 .. Ident_Int_0);
      if A /= (False, False, False, False, False, False, False, False, True)
      then
         Failed ("WRONG VALUES  -  B4");
      end if;

   end;

   declare
      type B_Arr is array (Integer range <>) of Boolean;
      A : B_Arr (1 .. 10);

   begin
      A := (True, False, True, False, True, False, True, False, True, False);
      A :=
        False & A (Ident_Int_1 .. Ident_Int_2) &
        A (Ident_Int_1 .. Ident_Int_2) & A (Ident_Int_1 .. Ident_Int_5);
      if A /= (False, True, False, True, False, True, False, True, False, True)
      then
         Failed ("WRONG VALUES  -  B5");
      end if;

      A := (True, False, True, False, True, False, True, False, True, False);
      A :=
        A (Ident_Int_6 .. Ident_Int_9) & A (Ident_Int_8 .. Ident_Int_9) &
        A (Ident_Int_8 .. Ident_Int_9) & False & True;
      if A /= (False, True, False, True, False, True, False, True, False, True)
      then
         Failed ("WRONG VALUES  -  B6");
      end if;

   end;

   -------------------------------------------------------------------
   --------------------  CHARACTER STRINGS  --------------------------

   declare
      A : String (1 .. 4);

   begin
      A := "ARGH";
      A := ('Q', A (Ident_Int_1), A (Ident_Int_2), A (Ident_Int_1));
      if A /= "QARA" then
         Failed ("WRONG VALUES  -  C1");
      end if;

      A := "ARGH";
      A := (A (Ident_Int_4), A (Ident_Int_3), A (Ident_Int_4), 'X');
      if A /= "HGHX" then
         Failed ("WRONG VALUES  -  C2");
      end if;

   end;

   declare
      A : String (Ident_Int (96) .. 104);

   begin
      A                                     := "APHRODITE";
      A (Ident_Int (96) .. Ident_Int (100)) :=
        A (Ident_Int (100) .. Ident_Int (104));
      if A /= "ODITEDITE" then
         Failed ("WRONG VALUES  -  C3");
      end if;

      A                                      := "APHRODITE";
      A (Ident_Int (100) .. Ident_Int (104)) :=
        A (Ident_Int (96) .. Ident_Int (100));
      if A /= "APHRAPHRO" then
         Failed ("WRONG VALUES  -  C4");
      end if;

   end;

   declare
      type Ch_Arr is array (Integer range <>) of Character;
      A : Ch_Arr (Ident_Int_1 .. 9);

   begin
      A := "CAMBRIDGE";
      A :=
        'S' & A (Ident_Int_1 .. Ident_Int_2) & A (Ident_Int_1 .. Ident_Int_2) &
        A (Ident_Int_1 .. Ident_Int_4);
      if A /= "SCACACAMB" then
         Failed ("WRONG VALUES  -  C5");
      end if;

      A := "CAMBRIDGE";
      A :=
        A (Ident_Int_8 .. Ident_Int_8) & A (Ident_Int_6 .. Ident_Int_8) &
        A (Ident_Int_6 .. Ident_Int_8) & "EA";
      if A /= "GIDGIDGEA" then
         Failed ("WRONG VALUES  -  C6");
      end if;

   end;

   Result;

end C52102b;
