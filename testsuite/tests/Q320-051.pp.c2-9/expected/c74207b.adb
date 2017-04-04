-- C74207B.ADA

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
-- CHECK THAT 'CONSTRAINED CAN BE APPLIED AFTER THE FULL DECLARATION OF A
-- PRIVATE TYPE THAT IS DERIVED FROM A PRIVATE TYPE.

-- BHS 6/18/84

with Report; use Report;
procedure C74207b is
begin
   Test
     ("C74207B",
      "AFTER THE FULL DECLARATION OF A PRIVATE " &
      "TYPE DERIVED FROM A PRIVATE TYPE, " &
      "'CONSTRAINED MAY BE APPLIED");

   declare
      package P1 is
         type Prec (D : Integer) is private;
         type P is private;
      private
         type Prec (D : Integer) is record
            null;
         end record;
         type P is new Integer;
      end P1;

      package P2 is
         type Lp1 is limited private;
         type Lp2 is limited private;
      private
         type Lp1 is new P1.Prec (3);
         type Lp2 is new P1.P;
         B1 : Boolean := Lp1'Constrained;
         B2 : Boolean := Lp2'Constrained;
      end P2;

      package body P2 is
      begin
         if not Ident_Bool (B1) then
            Failed ("WRONG VALUE FOR LP1'CONSTRAINED");
         end if;
         if not Ident_Bool (B2) then
            Failed ("WRONG VALUE FOR LP2'CONSTRAINED");
         end if;
      end P2;

   begin
      null;
   end;

   Result;

end C74207b;
