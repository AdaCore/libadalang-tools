-- CB4005A.ADA

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
-- CHECK THAT EXCEPTIONS PROPAGATED OUT OF A HANDLER ARE PROPAGATED
-- OUTSIDE THE ENCLOSING UNIT.

-- DAT 4/15/81
-- SPS 3/28/83

with Report; use Report;

procedure Cb4005a is

   E, F : exception;

   B : Boolean := False;

   procedure P is
   begin
      raise E;
   exception
      when F =>
         Failed ("WRONG HANDLER 1");
      when E =>
         if B then
            Failed ("WRONG HANDLER 2");
         else
            B := True;
            raise F;
         end if;
   end P;

begin
   Test ("CB4005A", "EXCEPTIONS FROM HANDLERS ARE PROPAGATED " & "OUTSIDE");

   begin
      P;
      Failed ("EXCEPTION NOT PROPAGATED 1");
   exception
      when F =>
         null;
      when others =>
         Failed ("WRONG HANDLER 3");
   end;

   Result;
end Cb4005a;
