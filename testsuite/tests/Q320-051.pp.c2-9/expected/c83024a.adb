-- C83024A.ADA

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
-- OBJECTIVE:
--     CHECK THAT A DECLARATION IN A DECLARATIVE REGION FOR A GENERIC
--     PACKAGE HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK
--     THAT THE OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH
--     DECLARATIVE REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH
--     AND THE OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAH DECLARATION.

-- HISTORY:
--     BCB 08/30/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C83024a is

   generic
      type T is private;
      X : T;
   function Gen_Fun return T;

   function Gen_Fun return T is
   begin
      return X;
   end Gen_Fun;

begin
   Test
     ("C83024A",
      "CHECK THAT A DECLARATION IN A DECLARATIVE " &
      "REGION FOR A GENERIC PACKAGE HIDES AN OUTER " &
      "DECLARATION OF A HOMOGRAPH");

   One : declare
      A   : Integer := Ident_Int (2);
      B   : Integer := A;
      Obj : Integer := Ident_Int (3);

      generic
         X : in Integer := A;
         A : in out Integer;
      package Inner is
         C : Integer := A;
      end Inner;

      package body Inner is
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 10");
         end if;

         if One.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 11");
         end if;

         if One.B /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 12");
         end if;

         if C /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 13");
         end if;

         if X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE PASSED IN - 14");
         end if;

         if Equal (1, 1) then
            A := Ident_Int (4);
         else
            A := 1;
         end if;
      end Inner;

      package New_Inner is new Inner (A => Obj);

   begin  -- ONE
      if Obj /= Ident_Int (4) then
         Failed ("INCORRECT VALUE PASSED OUT - 15");
      end if;
   end One;

   Two : declare            -- AFTER THE SPECIFICATION OF PACKAGE.
      A : Integer := Ident_Int (2);

      generic
         X : in out Integer;
      package Inner is
         A : Integer := Ident_Int (3);
      end Inner;

      B : Integer := A;

      package body Inner is
         C : Integer := Two.A;
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 20");
         end if;

         if Two.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 21");
         end if;

         if Two.B /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 22");
         end if;

         if C /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 23");
         end if;

         if X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE PASSED IN - 24");
         end if;

         if Equal (1, 1) then
            X := A;
         else
            null;
         end if;
      end Inner;

      package New_Inner is new Inner (A);

   begin  -- TWO
      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 25");
      end if;
   end Two;

   Three : declare                 --  OVERLOADING OF FUNCTIONS.

      Obj : Integer := 1;
      Flo : Float   := 6.25;

      function F is new Gen_Fun (Integer, Obj);

      generic
         X : in out Integer;
         F : in Float;
      package Inner is
      end Inner;

      function F is new Gen_Fun (Float, Flo);

      package body Inner is
      begin
         X := Integer (F);
      end Inner;

      package New_Inner is new Inner (Obj, Flo);

   begin
      if Obj /= Ident_Int (6) then
         Failed ("INCORRECT VALUE RETURNED FROM FUNCTION - 60");
      end if;
   end Three;

   Result;
end C83024a;
