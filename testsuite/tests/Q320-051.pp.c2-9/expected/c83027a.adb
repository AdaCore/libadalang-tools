-- C83027A.ADA

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
--     CHECK THAT A DECLARATION IN A RECORD DECLARATION HIDES AN OUTER
--     DECLARATION OF A HOMOGRAPH. ALSO CHECK THAT THE OUTER DECLARATION
--     IS DIRECTLY VISIBLE IN BOTH DECLARATIVE REGIONS BEFORE THE
--     DECLARATION OF THE INNER HOMOGRAPH AND THE OUTER DECLARATION IS
--     VISIBLE BY SELECTION AFTER THE INNER HOMOGRAPH DECLARATION.

-- HISTORY:
--     BCB 09/02/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83027a is

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
     ("C83027A",
      "CHECK THAT A DECLARATION IN A RECORD " &
      "DECLARATION HIDES AN OUTER DECLARATION OF " &
      "A HOMOGRAPH");

   One : declare
      A   : Integer := Ident_Int (2);
      Obj : Integer := Ident_Int (3);

      type Inner2 (A : Integer := Ident_Int (3)) is record
         C : Integer := One.A;
         D : Integer := A;
      end record;

      E : Integer := A;

      Recvar : Inner2;

   begin  -- ONE
      if A /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 1");
      end if;

      if Recvar.A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 2");
      end if;

      if E /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 3");
      end if;

      if Recvar.C /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR INNER VARIABLE - 4");
      end if;

      if Recvar.D /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR INNER VARIABLE - 5");
      end if;

      if Equal (1, 1) then
         Obj := Recvar.A;
      else
         Obj := 1;
      end if;

      if Obj /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 6");
      end if;
   end One;

   Two : declare

      generic
         A : Integer := Ident_Int (2);
         B : Integer := A;
      package P is
         type Inner (C : Integer := A; A : Integer := Ident_Int (3)) is record
            D : Integer := A;
         end record;
      end P;

      package body P is
         Recvar : Inner;
      begin
         if Recvar.A /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 10");
         end if;

         if A /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 11");
         end if;

         if B /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR OUTER VARIABLE - 12");
         end if;

         if Recvar.C /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 13");
         end if;

         if Recvar.D /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR INNER VARIABLE - 14");
         end if;
      end P;

      package Pack is new P;

   begin  -- TWO
      null;
   end Two;

   Three : declare
      A   : Integer := Ident_Int (2);
      Obj : Integer := Ident_Int (3);

      type Inner4
        (C : Integer := A;
         A : Integer := Ident_Int (3);
         X : Integer := Three.A)
      is record
         D : Integer := A;
      end record;

      Recvar : Inner4;

   begin  -- THREE
      if A /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 20");
      end if;

      if Recvar.A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH - 21");
      end if;

      if Recvar.C /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR INNER VARIABLE - 22");
      end if;

      if Recvar.D /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR INNER VARIABLE - 23");
      end if;

      if Recvar.X /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR INNER VARIABLE - 24");
      end if;

      if Equal (1, 1) then
         Obj := Recvar.A;
      else
         Obj := 1;
      end if;

      if Obj /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 25");
      end if;
   end Three;

   Result;
end C83027a;
