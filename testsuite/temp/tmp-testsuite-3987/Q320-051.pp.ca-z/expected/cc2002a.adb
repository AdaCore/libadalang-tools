-- CC2002A.ADA

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
-- CHECK THAT THE ELABORATION OF A GENERIC BODY HAS NO EFFECT OTHER
-- THAN TO ESTABLISH THE TEMPLATE BODY TO BE USED FOR THE
-- CORRESPONDING INSTANTIATIONS.

-- ASL 09/02/81
-- EG  10/30/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
procedure Cc2002a is

   Global : Integer              := 0;
   Q      : Integer range 1 .. 1 := 1;
begin
   Test ("CC2002A", "NO SIDE EFFECTS OF ELABORATION OF GENERIC BODY");

   begin
      declare
         generic
         package P is
         end P;

         generic
         procedure Proc;

         procedure Proc is
            C : constant Integer range 1 .. 1 := 2;
         begin
            raise Program_Error;
         end Proc;

         package body P is
            C : constant Boolean := Boolean'Succ (Ident_Bool (True));
         begin
            Global := 1;
            Q      := Q + 1;
         end P;
      begin
         null;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED DURING ELABORATION OF " & "GENERIC BODY");
   end;

   if Global /= 0 then
      Failed
        ("VALUE OF GLOBAL VARIABLE CHANGED BY ELABORATION " &
         "OF GENERIC BODY");
   end if;

   Result;
end Cc2002a;
