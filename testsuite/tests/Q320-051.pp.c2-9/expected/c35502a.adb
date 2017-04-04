-- C35502A.ADA

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
-- CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS THE CORRECT RESULTS WHEN THE PREFIX
-- IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR A CHARACTER TYPE.

-- RJW 5/05/86

with Report; use Report;

procedure C35502a is

begin

   Test
     ("C35502A",
      "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
      "THE CORRECT RESULTS WHEN THE PREFIX " &
      "IS AN ENUMERATION TYPE OTHER THAN " &
      "A BOOLEAN OR A CHARACTER TYPE");

   declare
      type Enum is (A, Bc, Abc, A_B_C, Abcd);

      subtype Subenum is Enum range A .. Abc;
      subtype Noenum is Enum range Abc .. A;

      type Newenum is new Enum;

   begin

      if Enum'Width /= Ident_Int (5) then
         Failed ("INCORRECT WIDTH FOR ENUM");
      end if;

      if Newenum'Width /= Ident_Int (5) then
         Failed ("INCORRECT WIDTH FOR NEWENUM");
      end if;

      if Subenum'Width /= Ident_Int (3) then
         Failed ("INCORRECT WIDTH FOR SUBENUM");
      end if;

      if Noenum'Width /= Ident_Int (0) then
         Failed ("INCORRECT WIDTH FOR NOENUM");
      end if;

   end;

   Result;
end C35502a;
