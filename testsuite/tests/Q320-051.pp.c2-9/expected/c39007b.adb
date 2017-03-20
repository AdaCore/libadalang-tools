-- C39007B.ADA

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
--     CHECK THAT PROGRAM_ERROR IS RAISED BY AN ATTEMPT TO INSTANTIATE
--     A GENERIC UNIT WHOSE BODY IS NOT YET ELABORATED.  USE A GENERIC
--     UNIT THAT IS DECLARED AND INSTANTIATED IN A PACKAGE
--     SPECIFICATION.

-- HISTORY:
--     BCB 08/01/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C39007b is

begin
   Test
     ("C39007B",
      "CHECK THAT PROGRAM_ERROR IS RAISED BY AN " &
      "ATTEMPT TO INSTANTIATE A GENERIC UNIT WHOSE " &
      "BODY IS NOT YET ELABORATED.  USE A GENERIC " &
      "UNIT THAT IS DECLARED AND INSTANTIATED IN A " &
      "PACKAGE SPECIFICATION");

   declare
   begin
      declare
         package P is
            generic
            function F return Boolean;

            function New_F is new F;
         end P;

         package body P is
            function F return Boolean is
            begin
               return True;
            end F;
         end P;
      begin
         Failed ("NO EXCEPTION RAISED");
         declare
            X : Boolean := Ident_Bool (False);
         begin
            X := P.New_F;
            if X /= Ident_Bool (True) then
               Comment ("NOT RELEVANT");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED TOO LATE");
         end;
      end;
   exception
      when Program_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED");
   end;

   Result;
end C39007b;
