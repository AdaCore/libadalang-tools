-- C52102D.ADA

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
--    THE SEMANTICS OF "COPY" ASSIGNMENT WHEN INITIAL ASSIGNMENT VALUES
--    REQUIRE RUN-TIME EVALUATION.   (THIS TEST IS IN TWO PARTS,
--    COVERING RESPECTIVELY STATIC AND DYNAMIC BOUNDS.)

-- PART 2:  DYNAMIC BOUNDS

-- RM 02/27/80
-- SPS 2/18/83
-- JBG 3/15/84
-- JBG 6/9/84
-- BHS 6/26/84

with Report;
procedure C52102d is

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

   function Id_I (X : Integer) return Integer renames Ident_Int;
   function Id_B (X : Boolean) return Boolean renames Ident_Bool;

begin

   Test
     ("C52102D",
      "CHECK THAT THE ASSIGNMENT OF OVERLAPPING " &
      "SOURCE AND TARGET VARIABLES (INCLUDING " &
      "ARRAYS AND SLICES IN VARIOUS COMBINATIONS) " &
      "SATISFIES THE SEMANTICS OF ""COPY"" " &
      "ASSIGNMENT WHEN INITIAL ASSIGNMENT VALUES " &
      "ARE DYNAMIC  (PART 2: DYNAMIC BOUNDS)");

   -------------------------------------------------------------------
   --------------------  ARRAYS OF INTEGERS  -------------------------

   declare
      A : array (1 .. Ident_Int_4) of Integer;

   begin
      A := (Id_I (11), Id_I (12), Id_I (13), Id_I (14));
      A := (1, A (Ident_Int_1), A (Ident_Int_2), A (Ident_Int_1));
      if A /= (1, 11, 12, 11) then
         Failed ("WRONG VALUES  -  I1");
      end if;

      A := (Id_I (11), Id_I (12), Id_I (13), Id_I (14));
      A := (A (Ident_Int_4), A (Ident_Int_3), A (Ident_Int_4), 1);
      if A /= (14, 13, 14, 1) then
         Failed ("WRONG VALUES  -  I2");
      end if;

   end;

   declare
      A : array (-4 .. Ident_Int_4) of Integer;

   begin
      A :=
        (-Id_I (4), -Id_I (3), -Id_I (2), -Id_I (1), Id_I (100), Id_I (1),
         Id_I (2), Id_I (3), Id_I (4));
      A (-4 .. Ident_Int_0) := A (Ident_Int_0 .. 4);
      if A /= (100, 1, 2, 3, 4, 1, 2, 3, 4) then
         Failed ("WRONG VALUES  -  I3");
      end if;

      A :=
        (-Id_I (4), -Id_I (3), -Id_I (2), -Id_I (1), Id_I (100), Id_I (1),
         Id_I (2), Id_I (3), Id_I (4));
      A (Ident_Int_0 .. 4) := A (-4 .. Ident_Int_0);
      if A /= (-4, -3, -2, -1, -4, -3, -2, -1, 100) then
         Failed ("WRONG VALUES  -  I4");
      end if;

   end;

   declare
      type Int_Arr is array (Integer range <>) of Integer;
      A : Int_Arr (1 .. 10);

   begin
      A :=
        (Id_I (1), Id_I (2), Id_I (3), Id_I (4), Id_I (5), Id_I (6), Id_I (7),
         Id_I (8), Id_I (9), Id_I (10));
      A :=
        0 & A (Ident_Int_1 .. Ident_Int_2) & A (Ident_Int_1 .. Ident_Int_2) &
        A (Ident_Int_1 .. Ident_Int_5);
      if A /= (0, 1, 2, 1, 2, 1, 2, 3, 4, 5) then
         Failed ("WRONG VALUES  -  I5");
      end if;

      A :=
        (Id_I (1), Id_I (2), Id_I (3), Id_I (4), Id_I (5), Id_I (6), Id_I (7),
         Id_I (8), Id_I (9), Id_I (10));
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
      A := (Id_B (False), Id_B (True), Id_B (True), Id_B (False));
      A := (True, A (Ident_Int_1), A (Ident_Int_2), A (Ident_Int_1));
      if A /= (True, False, True, False) then
         Failed ("WRONG VALUES  -  B1");
      end if;

      A := (Id_B (False), Id_B (True), Id_B (True), Id_B (False));
      A := (A (Ident_Int_4), A (Ident_Int_3), A (Ident_Int_4), True);
      if A /= (False, True, False, True) then
         Failed ("WRONG VALUES  -  B2");
      end if;

   end;

   declare
      A : array (-Ident_Int_4 .. 4) of Boolean;

   begin
      A :=
        (Id_B (False), Id_B (False), Id_B (False), Id_B (False), Id_B (False),
         Id_B (True), Id_B (True), Id_B (True), Id_B (True));
      A (-Ident_Int_4 .. Ident_Int_0) := A (Ident_Int_0 .. 4);
      if A /= (False, True, True, True, True, True, True, True, True) then
         Failed ("WRONG VALUES  -  B3");
      end if;

      A :=
        (Id_B (False), Id_B (False), Id_B (False), Id_B (False), Id_B (True),
         Id_B (True), Id_B (True), Id_B (True), Id_B (True));
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
      A :=
        (Id_B (True), Id_B (False), Id_B (True), Id_B (False), Id_B (True),
         Id_B (False), Id_B (True), Id_B (False), Id_B (True), Id_B (False));
      A :=
        False & A (Ident_Int_1 .. Ident_Int_2) &
        A (Ident_Int_1 .. Ident_Int_2) & A (Ident_Int_1 .. Ident_Int_5);
      if A /= (False, True, False, True, False, True, False, True, False, True)
      then
         Failed ("WRONG VALUES  -  B5");
      end if;

      A :=
        (Id_B (True), Id_B (False), Id_B (True), Id_B (False), Id_B (True),
         Id_B (False), Id_B (True), Id_B (False), Id_B (True), Id_B (False));
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
      if Equal (3, 3) then
         A := "ARGH";
      end if;
      A := ('Q', A (Ident_Int_1), A (Ident_Int_2), A (Ident_Int_1));
      if A /= "QARA" then
         Failed ("WRONG VALUES  -  C1");
      end if;

      if Equal (3, 3) then
         A := "ARGH";
      end if;
      A := (A (Ident_Int_4), A (Ident_Int_3), A (Ident_Int_4), 'X');
      if A /= "HGHX" then
         Failed ("WRONG VALUES  -  C2");
      end if;

   end;

   declare
      A : String (Ident_Int (96) .. 104);

   begin
      if Equal (3, 3) then
         A := "APHRODITE";
      end if;
      A (Ident_Int (96) .. Ident_Int (100)) :=
        A (Ident_Int (100) .. Ident_Int (104));
      if A /= "ODITEDITE" then
         Failed ("WRONG VALUES  -  C3");
      end if;

      if Equal (3, 3) then
         A := "APHRODITE";
      end if;
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
      if Equal (3, 3) then
         A := "CAMBRIDGE";
      end if;
      A :=
        'S' & A (Ident_Int_1 .. Ident_Int_2) & A (Ident_Int_1 .. Ident_Int_2) &
        A (Ident_Int_1 .. Ident_Int_4);
      if A /= "SCACACAMB" then
         Failed ("WRONG VALUES  -  C5");
      end if;

      if Equal (3, 3) then
         A := "CAMBRIDGE";
      end if;
      A :=
        A (Ident_Int_8 .. Ident_Int_8) & A (Ident_Int_6 .. Ident_Int_8) &
        A (Ident_Int_6 .. Ident_Int_8) & "EA";
      if A /= "GIDGIDGEA" then
         Failed ("WRONG VALUES  -  C6");
      end if;

   end;

   Result;

end C52102d;