-- C83025A.ADA

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
--     CHECK THAT A DECLARATION IN THE DECLARATIVE REGION OF A GENERIC
--     SUBPROGRAM HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK
--     THAT THE OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH
--     DECLARATIVE REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH
--     AND THE OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAPH DECLARATION.

-- HISTORY:
--     BCB 08/31/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C83025a is

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
     ("C83025A",
      "CHECK THAT A DECLARATION IN THE DECLARATIVE " &
      "REGION OF A GENERIC SUBPROGRAM HIDES AN OUTER " &
      "DECLARATION OF A HOMOGRAPH");

   One : declare                      -- SUBPROGRAM DECLARATIVE REGION.
      A : Integer := Ident_Int (2);
      B : Integer := A;

      generic
      procedure Inner (X : in out Integer);

      procedure Inner (X : in out Integer) is
         C : Integer := A;
         A : Integer := Ident_Int (3);
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 1");
         end if;

         if One.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 2");
         end if;

         if One.B /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 3");
         end if;

         if C /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 4");
         end if;

         if X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE PASSED IN - 5");
         end if;

         if Equal (1, 1) then
            X := A;
         else
            X := One.A;
         end if;
      end Inner;

      procedure New_Inner is new Inner;

   begin  -- ONE
      New_Inner (A);

      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 6");
      end if;
   end One;

   Two : declare               -- FORMAL PARAMETER OF GENERIC SUBPROGRAM.
      A   : Integer := Ident_Int (2);
      B   : Integer := A;
      Obj : Integer := Ident_Int (3);

      generic
      procedure Inner (X : in Integer := A; A : in out Integer);

      procedure Inner (X : in Integer := Two.A; A : in out Integer) is
         C : Integer := A;
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH -10");
         end if;

         if Two.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 11");
         end if;

         if Two.B /= Ident_Int (2) then
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

      procedure New_Inner is new Inner;

   begin  -- TWO
      New_Inner (A => Obj);

      if Obj /= Ident_Int (4) then
         Failed ("INCORRECT VALUE PASSED OUT - 15");
      end if;
   end Two;

   Three : declare      -- AFTER THE SPECIFICATION OF GENERIC SUBPROGRAM.
      generic
         A : Integer := Ident_Int (3);
      function Inner (X : Integer) return Integer;

      A : Integer := Ident_Int (2);

      B : Integer := A;

      function Inner (X : Integer) return Integer is
         C : Integer := Three.A;
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 20");
         end if;

         if Three.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 21");
         end if;

         if Three.B /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 22");
         end if;

         if C /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 23");
         end if;

         if X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE PASSED IN - 24");
         end if;

         if Equal (1, 1) then
            return A;
         else
            return X;
         end if;
      end Inner;

      function New_Inner is new Inner;

   begin  -- THREE
      if New_Inner (A) /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 25");
      end if;
   end Three;

   Four : declare
      A : Integer := Ident_Int (2);

      generic
         A : Integer;
         B : Integer := A;
      procedure Inner (X : in out Integer);

      procedure Inner (X : in out Integer) is
         C : Integer := Four.A;
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 30");
         end if;

         if B /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 31");
         end if;

         if Four.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 32");
         end if;

         if C /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 33");
         end if;

         if X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE PASSED IN - 34");
         end if;

         if Equal (1, 1) then
            X := A;
         else
            X := Four.A;
         end if;
      end Inner;

      procedure New_Inner is new Inner (A => Ident_Int (3));

   begin
      New_Inner (A);

      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 35");
      end if;
   end Four;

   Five : declare                 --  OVERLOADING OF FUNCTIONS.

      Obj : Integer := 1;
      Flo : Float   := 5.0;

      function F is new Gen_Fun (Integer, Obj);

      generic
      procedure Inner (X : in out Integer; F : in Float);

      function F is new Gen_Fun (Float, Flo);

      procedure Inner (X : in out Integer; F : in Float) is
      begin
         X := Integer (F);
      end Inner;

      procedure New_Inner is new Inner;

   begin  -- FIVE
      Flo := 6.25;

      New_Inner (Obj, Flo);

      if Obj /= Ident_Int (6) then
         Failed ("INCORRECT VALUE RETURNED FROM FUNCTION - 40");
      end if;
   end Five;

   Result;
end C83025a;
