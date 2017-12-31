-- CB1004A.ADA

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
-- CHECK THAT EXCEPTIONS DECLARED IN RECURSIVE PROCEDURES ARE NOT
--    REPLICATED ANEW FOR EACH RECURSIVE ACTIVATION OF THE PROCEDURE.

-- DCB 03/30/80
-- JRK 11/17/80
-- SPS 3/23/83

with Report;
procedure Cb1004a is

   use Report;

   Flow_Count : Integer := 0;

   procedure P1 (Switch1 : in Integer) is

      E1 : exception;

      procedure P2 is

      begin
         Flow_Count := Flow_Count + 1;   -- 3
         P1 (2);
         Failed ("EXCEPTION NOT PROPAGATED");

      exception
         when E1 =>
            Flow_Count := Flow_Count + 1;   -- 6
         when others =>
            Failed ("WRONG EXCEPTION RAISED");
      end P2;

   begin
      Flow_Count := Flow_Count + 1;   -- 2   -- 4
      if Switch1 = 1 then
         P2;
      elsif Switch1 = 2 then
         Flow_Count := Flow_Count + 1;   -- 5
         raise E1;
         Failed ("EXCEPTION NOT RAISED");
      end if;
   end P1;

begin
   Test
     ("CB1004A", "CHECK THAT EXCEPTIONS ARE NOT RECURSIVELY " & "REPLICATED");

   Flow_Count := Flow_Count + 1;   -- 1
   P1 (1);

   if Flow_Count /= 6 then
      Failed ("INCORRECT FLOW_COUNT VALUE");
   end if;

   Result;

exception
   when others =>
      Failed ("EXCEPTION HANDLED IN WRONG SCOPE");
      Result;
end Cb1004a;
