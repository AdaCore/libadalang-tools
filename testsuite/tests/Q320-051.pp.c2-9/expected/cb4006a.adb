-- CB4006A.ADA

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
--     CHECK THAT EXCEPTIONS IN A BLOCK IN A HANDLER
--     ARE HANDLED CORRECTLY.

-- HISTORY:
--     DAT 04/15/81
--     SPS 11/02/82
--     JET 01/06/88  UPDATED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.
--     JRL 05/28/92  CHANGED CODE IN PROGRAM_ERROR BLOCK TO
--                   PREVENT OPTIMIZATION.

with Report; use Report;

procedure Cb4006a is

   I1 : Integer range 1 .. 2 := 1;

   procedure P is
   begin
      if Equal (3, 3) then
         raise Program_Error;
      end if;
   exception
      when Program_Error =>
         declare
            I : Integer range 1 .. 1 := I1;
         begin
            if Equal (I, I) then
               I := I1 + 1;
            end if;
            Failed ("EXCEPTION NOT RAISED 1");

            if not Equal (I, I) then
               Comment ("CAN'T OPTIMIZE THIS");
            end if;

         exception
            when Constraint_Error =>
               if I1 /= 1 then
                  Failed ("WRONG HANDLER 1");
               else
                  I1 := I1 + 1;
               end if;
         end;
      when Constraint_Error =>
         Failed ("WRONG HANDLER 3");
   end P;

begin
   Test ("CB4006A", "CHECK THAT EXCEPTIONS IN BLOCKS IN " & "HANDLERS WORK");

   P;
   if Ident_Int (I1) /= 2 then
      Failed ("EXCEPTION NOT HANDLED CORRECTLY");
   else
      begin
         P;
         Failed ("EXCEPTION NOT RAISED CORRECTLY 2");
      exception
         when Constraint_Error =>
            null;
      end;
   end if;

   Result;

exception
   when others =>
      Failed ("WRONG HANDLER 2");
      Result;

end Cb4006a;
