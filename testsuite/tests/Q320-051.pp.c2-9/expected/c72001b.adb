-- C72001B.ADA

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
-- CHECK THAT A PACKAGE BODY CAN BE PROVIDED FOR A PACKAGE SPECIFICATION
--    THAT DOES NOT CONTAIN ANY SUBPROGRAM OR TASK DECLARATIONS AND THAT
--    STATEMENTS WITHIN THE PACKAGE BODIES CAN BE USED TO INITIALIZE
--    VARIABLES VISIBLE WITHIN THE PACKAGE BODY.

-- RM 04/30/81
-- RM 05/07/81 (TO INCORPORATE OLD TEST OBJECTIVE  7.1/T1 )
-- ABW 6/10/82
-- SPS 11/4/82
-- JBG 9/15/83

with Report;
procedure C72001b is

   use Report;

begin

   Test
     ("C72001B",
      "CHECK: PACKAGE BODIES CAN INITIALIZE VISIBLE" & " VARIABLES");

   declare

      package P5 is

         A : Character := 'B';
         B : Boolean   := False;

         package P6 is
            I : Integer := Ident_Int (6);
         end P6;

      end P5;

      package body P5 is
         package body P6 is
         begin
            A := 'C';
            I := 17;
            B := Ident_Bool (True);
         end P6;
      begin
         A := 'A';
      end P5;

      use P5;
      use P6;

   begin

      if A /= 'A' then
         Failed ("INITIALIZATIONS NOT CORRECT - 1");
      end if;

      if B /= True then
         Failed ("INITIALIZATIONS NOT CORRECT - 2");
      end if;

      if I /= 17 then
         Failed ("INITIALIZATIONS NOT CORRECT - 3");
      end if;

   end;

   Result;

end C72001b;
