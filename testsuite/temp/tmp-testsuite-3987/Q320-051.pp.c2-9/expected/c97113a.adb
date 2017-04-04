-- C97113A.ADA

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
-- CHECK THAT ALL CONDITIONS, OPEN DELAY ALTERNATIVE EXPRESSIONS, AND
--   OPEN ENTRY FAMILY INDICES ARE EVALUATED (EVEN WHEN SOME (PERHAPS
--   ALL BUT ONE) OF THE ALTERNATIVES CAN BE RULED OUT WITHOUT
--   COMPLETING THE EVALUATIONS).

-- RM 5/06/82
-- SPS 11/21/82
-- WRG 7/9/86    ADDED DELAY EXPRESSIONS AND ENTRY FAMILY INDICES.

with Report; use Report;
procedure C97113a is

   Expr1_Evaluated : Boolean := False;
   Expr2_Evaluated : Boolean := False;
   Expr3_Evaluated : Boolean := False;

   function F1 return Boolean is
   begin
      Expr1_Evaluated := True;
      return True;
   end F1;

   function F2 (X : Integer) return Integer is
   begin
      Expr2_Evaluated := True;
      return X;
   end F2;

   function F3 (X : Duration) return Duration is
   begin
      Expr3_Evaluated := True;
      return X;
   end F3;

begin

   Test
     ("C97113A",
      "CHECK THAT ALL CONDITIONS, OPEN DELAY " &
      "ALTERNATIVE EXPRESSIONS, AND OPEN ENTRY " &
      "FAMILY INDICES ARE EVALUATED");

   declare

      task T is
         entry E1;
         entry E2;
         entry E3 (1 .. 1);
      end T;

      task body T is
      begin
         --ENSURE THAT E1 HAS BEEN CALLED BEFORE PROCEEDING:
         while E1'Count = 0 loop
            delay 1.0;
         end loop;

         select
            accept E1;
         or when F1 =>
            accept E2;
         or
            accept E3 (F2 (1));
         or
            delay F3 (1.0);
         end select;
      end T;

   begin

      T.E1;

   end;

   if not Expr1_Evaluated then
      Failed ("GUARD NOT EVALUATED");
   end if;

   if not Expr2_Evaluated then
      Failed ("ENTRY FAMILY INDEX NOT EVALUATED");
   end if;

   if not Expr3_Evaluated then
      Failed ("OPEN DELAY ALTERNATIVE EXPRESSION NOT EVALUATED");
   end if;

   Result;

end C97113a;
