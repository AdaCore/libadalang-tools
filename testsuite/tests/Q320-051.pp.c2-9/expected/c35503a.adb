-- C35503A.ADA

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
-- CHECK THAT 'WIDTH' YIELDS THE CORRECT RESULT WHEN THE PREFIX IS AN INTEGER
-- TYPE.

-- RJW 3/12/86

with Report; use Report;

procedure C35503a is

begin
   Test
     ("C35503A",
      "CHECK THAT 'WIDTH' YIELDS THE CORRECT " &
      "RESULT WHEN THE PREFIX IS AN INTEGER TYPE");

   declare
      subtype Sinteger is Integer;

      type Int is range -1_000 .. 1_000;
      type Int2 is new Int range 1E2 .. 1E2;

      subtype Sint1 is Int range 00_000 .. 100;
      subtype Sint2 is Int range 16#E#E1 .. 2#1111_1111#;
      subtype Sint3 is Int range -100 .. 9;
      subtype Noint is Int range 1 .. -1;

   begin
      if Ident_Int (Sinteger'Width) /= Integer'Width then
         Failed ("WRONG WIDTH FOR 'SINTEGER'");
      end if;

      if Ident_Int (Int'Width) /= 5 then
         Failed ("WRONG WIDTH FOR 'INT'");
      end if;

      if Ident_Int (Int2'Width) /= 4 then
         Failed ("WRONG WIDTH FOR 'INT2'");
      end if;

      if Ident_Int (Sint1'Width) /= 4 then
         Failed ("WRONG WIDTH FOR 'SINT1'");
      end if;

      if Ident_Int (Sint2'Width) /= 4 then
         Failed ("WRONG WIDTH FOR 'SINT2'");
      end if;

      if Ident_Int (Sint3'Width) /= 4 then
         Failed ("WRONG WIDTH FOR 'SINT3'");
      end if;

      if Ident_Int (Noint'Width) /= 0 then
         Failed ("WRONG WIDTH FOR 'NOINT'");
      end if;
   end;

   Result;
end C35503a;
