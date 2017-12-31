-- C83022G0M.ADA

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
--     HOMOGRAPH DECLARATION, IF THE SUBPROGRAM BODY IS COMPILED
--     SEPARATELY AS A SUBUNIT.

-- SEPARATE FILES ARE:
--     C83022G0M.ADA - (THIS FILE) MAIN PROGRAM.
--     C83022G1.ADA -- SUBPROGRAM BODIES.

-- HISTORY:
--     BCB 08/26/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C83022g0m is

   generic
      type T is private;
      X : T;
   function Gen_Fun return T;

   A : Integer := Ident_Int (2);
   B : Integer := A;

   Obj : Integer := Ident_Int (3);

   Flo : Float := 5.0;

   procedure Template (X : in Integer := A; Y : in out Integer);

   procedure Inner4 (Z : in Integer := A; A : in out Integer) renames Template;

   procedure Inner (X : in out Integer) is separate;

   procedure Inner2 (X : in Integer := A; A : in out Integer) is separate;

   function Inner3 (X : Integer) return Integer is separate;

   procedure Template (X : in Integer := A; Y : in out Integer) is separate;

   procedure Inner5 (X : in out Integer) is separate;

   generic
      with procedure Subpr (Y : in out Integer) is <>;
   package P is
      Pac_Var : Integer := 1;
   end P;

   package body P is
   begin
      Subpr (A);

      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 1");
      end if;

      if Pac_Var /= Ident_Int (1) then
         Failed ("INCORRECT VALUE FOR PAC_VAR - 2");
      end if;
   end P;

   package New_P is new P (Inner5);

   function Gen_Fun return T is
   begin
      return X;
   end Gen_Fun;

   function F is new Gen_Fun (Integer, Obj);

   procedure Inner6 (X : in out Integer; F : in Float);

   function F is new Gen_Fun (Float, Flo);

   procedure Inner6 (X : in out Integer; F : in Float) is separate;

begin
   Test
     ("C83022G",
      "CHECK THAT A DECLARATION IN A SUBPROGRAM " &
      "FORMAL PART OR BODY HIDES AN OUTER " & "DECLARATION OF A HOMOGRAPH");

   A := Ident_Int (2);
   B := A;

   Inner (A);

   if A /= Ident_Int (3) then
      Failed ("INCORRECT VALUE PASSED OUT - 3");
   end if;

   A := Ident_Int (2);

   Inner2 (A => Obj);

   if Obj /= Ident_Int (4) then
      Failed ("INCORRECT VALUE PASSED OUT - 4");
   end if;

   A := Ident_Int (2);

   B := A;

   if Inner3 (A) /= Ident_Int (3) then
      Failed ("INCORRECT VALUE PASSED OUT - 5");
   end if;

   A := Ident_Int (2);

   B   := A;
   Obj := 5;

   if B /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 6");
   end if;

   Inner4 (A => Obj);

   if Obj /= Ident_Int (4) then
      Failed ("INCORRECT VALUE PASSED OUT - 7");
   end if;

   Obj := 1;

   Flo := 6.25;

   Inner6 (Obj, Flo);

   if Obj /= Ident_Int (6) then
      Failed ("INCORRECT VALUE RETURNED FROM FUNCTION - 8");
   end if;

   Result;
end C83022g0m;
