-- C85005F.ADA

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
--     CHECK THAT, FOR A RENAMED VARIABLE DESIGNATED BY AN ACCESS VALUE,
--     A CHANGE IN THE ACCESS VALUE DOES NOT AFFECT WHICH VARIABLE IS
--     DENOTED BY THE NEW NAME.

-- HISTORY:
--     JET 07/26/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85005f is
   type Acc is access Integer;

   Bump : Integer := 0;

   A : Acc := null;

   function Get_Pointer return Acc is
   begin
      Bump := Ident_Int (Bump) + 1;
      return new Integer'(Bump);
   end Get_Pointer;

begin
   Test
     ("C85005F",
      "CHECK THAT, FOR A RENAMED VARIABLE DESIGNATED " &
      "BY AN ACCESS VALUE, A CHANGE IN THE ACCESS " &
      "VALUE DOES NOT AFFECT WHICH VARIABLE IS " &
      "DENOTED BY THE NEW NAME");

   A := Get_Pointer;

   declare
      X1 : Integer renames A.all;
      X2 : Integer renames Get_Pointer.all;
   begin
      A := Get_Pointer;

      if X1 /= 1 then
         Failed ("CHANGING ACCESS VALUE CHANGED RENAMED VARIABLE");
      end if;

      if X2 /= 2 then
         Failed ("INCORRECT RESULT FROM FUNCTION AS PREFIX");
      end if;
   end;

   Result;
end C85005f;
