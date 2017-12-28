-- C83023A.ADA

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
--     CHECK THAT A DECLARATION IN A DECLARATIVE REGION OF A TASK
--     HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK THAT THE
--     OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH DECLARATIVE
--     REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH AND THE
--     OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAPH DECLARATION.

-- HISTORY:
--     BCB 08/29/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83023a is

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
     ("C83023A",
      "CHECK THAT A DECLARATION IN A DECLARATIVE " &
      "REGION OF A TASK HIDES AN OUTER " &
      "DECLARATION OF A HOMOGRAPH");

   One :
   declare                      -- DECLARATIVE REGION.
      A : Integer := Ident_Int (2);
      B : Integer := A;

      task Inner is
         entry Here (X : in out Integer);
      end Inner;

      task body Inner is
         C : Integer := A;
         A : Integer := Ident_Int (3);
      begin
         accept Here (X : in out Integer) do
            if A /= Ident_Int (3) then
               Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH" & " - 1");
            end if;

            if One.A /= Ident_Int (2) then
               Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH" & " - 2");
            end if;

            if One.B /= Ident_Int (2) then
               Failed ("INCORRECT VALUE FOR OUTER VARIABLE " & "- 3");
            end if;

            if C /= Ident_Int (2) then
               Failed ("INCORRECT VALUE FOR INNER VARIABLE " & "- 4");
            end if;

            if X /= Ident_Int (2) then
               Failed ("INCORRECT VALUE PASSED IN - 5");
            end if;

            if Equal (1, 1) then
               X := A;
            else
               X := One.A;
            end if;
         end Here;
      end Inner;

   begin  -- ONE
      Inner.Here (A);

      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 6");
      end if;
   end One;

   Two :
   declare            -- AFTER THE SPECIFICATION OF TASK.
      task Inner is
         entry Here (X : in out Integer);
      end Inner;

      A : Integer := Ident_Int (2);

      B : Integer := A;

      task body Inner is
         C : Integer := A;
         A : Integer := Ident_Int (3);
      begin
         accept Here (X : in out Integer) do
            if A /= Ident_Int (3) then
               Failed ("INCORRECT VALUE FOR INNER HOMOGRAPH" & " - 10");
            end if;

            if Two.A /= Ident_Int (2) then
               Failed ("INCORRECT VALUE FOR OUTER HOMOGRAPH" & " - 11");
            end if;

            if Two.B /= Ident_Int (2) then
               Failed ("INCORRECT VALUE FOR OUTER VARIABLE " & "- 12");
            end if;

            if C /= Ident_Int (2) then
               Failed ("INCORRECT VALUE FOR INNER VARIABLE " & "- 13");
            end if;

            if X /= Ident_Int (2) then
               Failed ("INCORRECT VALUE PASSED IN - 14");
            end if;

            if Equal (1, 1) then
               X := A;
            else
               null;
            end if;
         end Here;
      end Inner;

   begin  -- TWO
      Inner.Here (A);

      if A /= Ident_Int (3) then
         Failed ("INCORRECT VALUE PASSED OUT - 15");
      end if;
   end Two;

   Three :
   declare                 --  OVERLOADING OF FUNCTIONS.

      Obj : Integer := 1;
      Flo : Float   := 5.0;

      function F is new Gen_Fun (Integer, Obj);

      task Inner is
         entry Here (X : in out Integer);
      end Inner;

      function F is new Gen_Fun (Float, Flo);

      task body Inner is
         F : Float := 6.25;
      begin
         accept Here (X : in out Integer) do
            X := Integer (F);
         end Here;
      end Inner;

   begin
      Inner.Here (Obj);

      if Obj /= Ident_Int (6) then
         Failed ("INCORRECT VALUE RETURNED FROM FUNCTION - 20");
      end if;
   end Three;

   Result;
end C83023a;
