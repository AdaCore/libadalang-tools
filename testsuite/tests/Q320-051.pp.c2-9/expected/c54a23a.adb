-- C54A23A.ADA

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
-- CHECK THAT CASE CHOICES MAY BE CONSTANT NAMES

-- DAT 3/18/81
-- SPS 4/7/82

with Report; use Report;

procedure C54a23a is

   C1 : constant Integer := 1;
   C2 : constant Integer := 2;
   C3 : constant Integer := 3;

begin
   Test ("C54A23A", "CASE CHOICES MAY BE CONSTANTS");

   case Ident_Int (C3) is
      when C1 | C2 =>
         Failed ("WRONG CASE CHOICE 1");
      when 3 =>
         null;
      when others =>
         Failed ("WRONG CASE CHOICE 2");
   end case;

   Result;
end C54a23a;
