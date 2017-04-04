-- C38108D0M.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE CAN BE DECLARED IN THE PRIVATE PART OF
-- A PACKAGE, WITH THE FULL DECLARATION OCCURRING LATER IN A PACKAGE BODY
-- SUBUNIT.

-- OTHER FILES: C38108D1.ADA (PACKAGE BODY SUBUNIT.)

-- AH  8/20/86

with Report; use Report;

procedure C38108d0m is
   package C38108d1 is
      type L is limited private;
      procedure Assign (X : in Integer; Y : in out L);
      function "=" (X, Y : in L) return Boolean;
   private
      type Inc (D : Integer);
      type L is access Inc;
   end C38108d1;

   package body C38108d1 is separate;

   use C38108d1;
begin

   Test
     ("C38108D",
      "CHECK THAT INCOMPLETE TYPE CAN BE DECLARED IN " &
      "PRIVATE PART WITH FULL DECLARATION IN " &
      "A PACKAGE BODY SUBUNIT");

   declare
      Val_1, Val_2 : L;
   begin
      Assign (2, Val_1);
      Assign (2, Val_2);
      if not "=" (Val_1, Val_2) then
         Failed ("INCOMPLETE TYPE NOT FULLY DECLARED");
      end if;
   end;

   Result;
end C38108d0m;
