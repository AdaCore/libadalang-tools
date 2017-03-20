-- C34012A.ADA

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
--     CHECK THAT DEFAULT EXPRESSIONS IN DERIVED RECORD TYPES AND
--     DERIVED SUBPROGRAMS ARE EVALUATED USING THE ENTITIES DENOTED BY
--     THE EXPRESSIONS IN THE PARENT TYPE.

-- HISTORY:
--     RJW 06/19/86  CREATED ORIGINAL TEST.
--     BCB 08/19/87  CHANGED HEADER TO STANDARD HEADER FORMAT.  CHANGED
--                   PACKAGE B SO WOULD HAVE ONE CASE WHERE DEFAULT IS
--                   DECLARED BEFORE THE DERIVED TYPE DECLARATION.

with Report; use Report;

procedure C34012a is

begin
   Test
     ("C34012A",
      "CHECK THAT DEFAULT EXPRESSIONS IN DERIVED " &
      "RECORD TYPES AND DERIVED SUBPROGRAMS ARE " &
      "EVALUATED USING THE ENTITIES DENOTED BY THE " &
      "EXPRESSIONS IN THE PARENT TYPE");

   declare
      package P is
         X : Integer := 5;
         type Rec is record
            C : Integer := X;
         end record;
      end P;

      package Q is
         X : Integer := 6;
         type New_Rec is new P.Rec;
         Qvar : New_Rec;
      end Q;

      package R is
         X : Integer := 7;
         type Brand_New_Rec is new Q.New_Rec;
         Rvar : Brand_New_Rec;
      end R;

      use Q;
      use R;
   begin
      if Qvar.C = 5 then
         null;
      else
         Failed ("INCORRECT VALUE FOR QVAR");
      end if;

      if Rvar.C = 5 then
         null;
      else
         Failed ("INCORRECT VALUE FOR RVAR");
      end if;
   end;

   declare
      package A is
         type T is range 1 .. 10;
         Default : T := 5;
         function F (X : T := Default) return T;
      end A;

      package body A is
         function F (X : T := Default) return T is
         begin
            return X;
         end F;
      end A;

      package B is
         Default : A.T := 6;
         type New_T is new A.T;
         Bvar : New_T := F;
      end B;

      package C is
         type Brand_New_T is new B.New_T;
         Default : Brand_New_T := 7;
         Cvar    : Brand_New_T := F;
      end C;

      use B;
      use C;
   begin
      if Bvar = 5 then
         null;
      else
         Failed ("INCORRECT VALUE FOR BVAR");
      end if;

      if Cvar = 5 then
         null;
      else
         Failed ("INCORRECT VALUE FOR CVAR");
      end if;

      declare
         Var : Brand_New_T := F;
      begin
         if Var = 5 then
            null;
         else
            Failed ("INCORRECT VALUE FOR VAR");
         end if;
      end;
   end;

   Result;
end C34012a;
