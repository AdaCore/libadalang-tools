-- C41306C.ADA

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
-- CHECK THAT IF  F  IS A FUNCTION RETURNING AN ACCESS VALUE DESIGNATING
--     A TASK OF A TYPE HAVING
--     AN ENTRY  E ,  AN ENTRY CALL OF THE FORM
--
--                           F.E
--
--     IS PERMITTED.

-- RM  02/02/82
-- ABW 07/16/82
-- EG  05/28/85

with Report; use Report;

procedure C41306c is

begin

   Test
     ("C41306C",
      "CHECK THAT IF  F  IS A FUNCTION RETURNING" &
      " AN ACCESS VALUE DESIGNATING" &
      " A TASK OF A TYPE HAVING AN ENTRY  E ,  AN" &
      " ENTRY CALL OF THE FORM  F.E  IS PERMITTED");

   -------------------------------------------------------------------

   declare

      X : Integer := 0;

      task type T is
         entry E;
      end T;

      type A_T is access T;

      task body T is
      begin
         accept E do
            X := Ident_Int (17);
         end E;
      end T;

      function F1 return A_T is
         A_T_Var1 : A_T := new T;
      begin
         return A_T_Var1;
      end F1;

      function F2 (A, B : Boolean) return A_T is
         A_T_Var2 : A_T := new T;
      begin
         if A and B then
            null;
         end if;
         return A_T_Var2;
      end F2;

   begin

      F1.E;      --  THE ELABOR. OF  F1 (BODY)  ACTIVATES THE TASK,
      --      WHICH  PROCEEDS TO WAIT FOR ENTRY  E  TO
      --      BE CALLED.

      --  THE CALLED ENTRY CAUSES  X  TO BE SET TO  17 .

      if X /= 17 then
         Failed ("WRONG VALUE FOR GLOBAL VARIABLE   (1)");
      end if;

      X := 0;
      F2 (True, True).E;   -- THE ELABORATION OF F2 (BODY) ACTIVATES
      -- THE TASK, WHICH PROCEEDS TO WAIT FOR
      -- ENTRY E TO BE CALLED.

      -- THE CALLED ENTRY CAUSES X TO BE SET TO
      -- 17.

      if X /= 17 then
         Failed ("WRONG VALUE FOR GLOBAL VARIABLE (2)");
      end if;

   end;

   -------------------------------------------------------------------

   declare

      X : Integer := 0;

      task type T is
         entry E;
      end T;

      type A_T is access T;

      task body T is
      begin
         accept E do
            X := Ident_Int (17);
         end E;
      end T;

      function F3 return A_T is
      begin
         return new T;
      end F3;

      function F4 (C, D : Boolean) return A_T is
      begin
         if C and D then
            null;
         end if;
         return new T;
      end F4;

   begin

      F3.E;      --  THE ELABOR. OF  F3 (BODY)  ACTIVATES THE TASK,
      --      WHICH  PROCEEDS TO WAIT FOR ENTRY  E  TO
      --      BE CALLED.

      --  THE CALLED ENTRY CAUSES  X  TO BE SET TO  17 .

      if X /= 17 then
         Failed ("WRONG VALUE FOR GLOBAL VARIABLE   (3)");
      end if;

      X := 0;
      F4 (True, True).E;   -- THE ELABORATION OF F4 (BODY) ACTIVATES
      -- THE TASK WHICH PROCEEDS TO WAIT FOR
      -- ENTRY E TO BE CALLED.

      -- THE CALLED ENTRY CAUSES X TO BE SET TO
      -- 17.

      if X /= 17 then
         Failed ("WRONG VALUE FOR GLOBAL VARIABLE (4)");
      end if;

   end;

   -------------------------------------------------------------------

   declare

      X : Integer := 0;

      task type T is
         entry E;
      end T;

      type A_T is access T;

      task body T is
      begin
         accept E do
            X := Ident_Int (17);
         end E;
      end T;

   begin

      declare

         F3 : A_T := new T;

      begin

         F3.E;

         --  THE CALLED ENTRY CAUSES  X  TO BE SET TO  17 .

         if X /= 17 then
            Failed ("WRONG VALUE FOR GLOBAL VARIABLE   (5)");
         end if;

      end;

   end;

   -------------------------------------------------------------------

   Result;

end C41306c;
