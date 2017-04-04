-- C43108A.ADA

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
--     CHECK THAT IN A RECORD AGGREGATE THE VALUE OF A DISCRIMINANT IS
--     USED TO RESOLVE THE TYPE OF A COMPONENT THAT DEPENDS ON THE
--     DISCRIMINANT.

-- HISTORY:
--     DHH 09/08/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43108a is

begin
   Test
     ("C43108A",
      "CHECK THAT IN A RECORD AGGREGATE THE VALUE OF " &
      "A DISCRIMINANT IS USED TO RESOLVE THE TYPE OF " &
      "A COMPONENT THAT DEPENDS ON THE DISCRIMINANT");

   declare
      A : Integer;

      type Dis (A : Boolean) is record
         case A is
            when True =>
               B : Boolean;
               C : Integer;
            when False =>
               D : Integer;
         end case;
      end record;

      function Diff (Param : Dis) return Integer is
      begin
         if Param.B then
            return Param.C;
         else
            return Param.D;
         end if;
      end Diff;

   begin
      A := Diff ((C => 3, others => True));

      if A /= Ident_Int (3) then
         Failed ("STATIC OTHERS NOT DECIDED CORRECTLY");
      end if;
   end;

   declare
      Global : Integer := 0;
      type Int is new Integer;

      type Dis (A : Boolean) is record
         case A is
            when True =>
               I1 : Int;
            when False =>
               I2 : Integer;
         end case;
      end record;
      function F return Int;
      function F return Integer;

      A : Dis (True);

      function F return Int is
      begin
         Global := 1;
         return 5;
      end F;

      function F return Integer is
      begin
         Global := 2;
         return 5;
      end F;

   begin
      A := (True, others => F);

      if Global /= 1 then
         Failed ("NON_STATIC OTHERS NOT DECIDED CORRECTLY");
      end if;
   end;

   Result;
end C43108a;
