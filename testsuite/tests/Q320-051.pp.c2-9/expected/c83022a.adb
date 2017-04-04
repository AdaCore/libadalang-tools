-- C83022A.ADA

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
--     CHECK THAT A DECLARATION IN A SUBPROGRAM FORMAL PART OR BODY
--     HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK THAT THE
--     OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH DECLARATIVE
--     REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH AND THE
--     OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAH DECLARATION.

-- HISTORY:
--     TBN 08/01/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83022a is

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
     ("C83022A",
      "CHECK THAT A DECLARATION IN A SUBPROGRAM " &
      "FORMAL PART OR BODY HIDES AN OUTER " &
      "DECLARATION OF A HOMOGRAPH");

   One : declare                      -- SUBPROGRAM DECLARATIVE REGION.
      A : Integer := Ident_Int (2);
      B : Integer := A;

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

   begin  -- ONE
      Inner (A);
      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 6");
      end if;
   end One;

   Two : declare                     -- FORMAL PARAMETER OF SUBPROGRAM.
      A   : Integer := Ident_Int (2);
      B   : Integer := A;
      Obj : Integer := Ident_Int (3);

      procedure Inner (X : in Integer := A; A : in out Integer) is
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

   begin  -- TWO
      Inner (A => Obj);
      if Obj /= Ident_Int (4) then
         Failed ("INCORRECT VALUE PASSED OUT - 15");
      end if;
   end Two;

   Three : declare            -- AFTER THE SPECIFICATION OF SUBPROGRAM.
      A : Integer := Ident_Int (2);

      function Inner (X : Integer) return Integer;

      B : Integer := A;

      function Inner (X : Integer) return Integer is
         C : Integer := A;
         A : Integer := Ident_Int (3);
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

   begin  -- THREE
      if Inner (A) /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 25");
      end if;
   end Three;

   Four : declare                              -- RENAMING DECLARATION.
      A : Integer := Ident_Int (2);

      procedure Template (X : in Integer := A; Y : in out Integer);

      procedure Inner
        (Z : in     Integer := A;
         A : in out Integer) renames
        Template;

      B   : Integer := A;
      Obj : Integer := 5;

      procedure Template (X : in Integer := A; Y : in out Integer) is
      begin  -- TEMPLATE
         if X /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FOR VARIABLE - 30");
         end if;
         if Y /= Ident_Int (5) then
            Failed ("INCORRECT RESULTS FOR VARIABLE - 31");
         end if;
         Y := Ident_Int (2 * X);
         if Four.A /= Ident_Int (2) then
            Failed ("INCORRECT RESULTS FOR OUTER HOMOGRAPH - " & "32");
         end if;
      end Template;

   begin  -- FOUR
      if B /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 32");
      end if;
      Inner (A => Obj);
      if Obj /= Ident_Int (4) then
         Failed ("INCORRECT VALUE PASSED OUT - 33");
      end if;
   end Four;

   Five : declare                         -- GENERIC FORMAL SUBPROGRAM.
      A : Integer := Ident_Int (2);
      B : Integer := A;

      procedure Inner (X : in out Integer);

      generic
         with procedure Subpr (Y : in out Integer) is <>;
      package P is
         Pac_Var : Integer := 1;
      end P;

      procedure Inner (X : in out Integer) is
         C : Integer := A;
         A : Integer := Ident_Int (3);
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 41");
         end if;
         if Five.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 42");
         end if;
         if Five.B /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 43");
         end if;
         if C /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 44");
         end if;
         if X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE PASSED IN - 45");
         end if;
         if Equal (1, 1) then
            X := A;
         else
            X := Five.A;
         end if;
      end Inner;

      package body P is
      begin
         Subpr (A);
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE PASSED OUT - 46");
         end if;
         if Pac_Var /= Ident_Int (1) then
            Failed ("INCORRECT VALUE FOR PAC_VAR - 47");
         end if;
      end P;

      package New_P is new P (Inner);

   begin  -- FIVE
      null;
   end Five;

   Six : declare                              -- GENERIC INSTANTIATION.
      A   : Integer := Ident_Int (2);
      B   : Integer := A;
      Obj : Integer := Ident_Int (3);

      generic
      procedure Inner (X : in Integer := A; A : in out Integer);

      procedure Inner (X : in Integer := Six.A; A : in out Integer) is
         C : Integer := A;
      begin
         if A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH -50");
         end if;
         if Six.A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 51");
         end if;
         if Six.B /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 52");
         end if;
         if C /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 53");
         end if;
         if X /= Ident_Int (2) then
            Failed ("INCORRECT VALUE PASSED IN - 54");
         end if;
         if Equal (1, 1) then
            A := Ident_Int (4);
         else
            A := 1;
         end if;
      end Inner;

      procedure Subpr is new Inner;

   begin  -- SIX
      Subpr (A => Obj);
      if Obj /= Ident_Int (4) then
         Failed ("INCORRECT VALUE PASSED OUT - 55");
      end if;
   end Six;

   Seven : declare                 --  OVERLOADING OF FUNCTIONS.

      Obj : Integer := 1;
      Flo : Float   := 5.0;

      function F is new Gen_Fun (Integer, Obj);

      procedure Inner (X : in out Integer; F : in Float);

      function F is new Gen_Fun (Float, Flo);

      procedure Inner (X : in out Integer; F : in Float) is
      begin
         X := Integer (F);
      end Inner;

   begin
      Flo := 6.25;
      Inner (Obj, Flo);
      if Obj /= Ident_Int (6) then
         Failed ("INCORRECT VALUE RETURNED FROM FUNCTION - 60");
      end if;
   end Seven;

   Result;
end C83022a;
