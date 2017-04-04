-- C35507A.ADA

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
-- CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS THE CORRECT RESULTS
-- WHEN THE PREFIX IS A CHARACTER TYPE.

-- RJW 5/29/86

with Report; use Report;

procedure C35507a is

begin

   Test
     ("C35507A",
      "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
      "THE CORRECT RESULTS WHEN THE PREFIX " &
      "IS A CHARACTER TYPE");

   declare
      type Char1 is (A, 'A');

      subtype Char2 is Character range 'A' .. 'Z';

      subtype Nochar is Character range 'Z' .. 'A';

      type Newchar is new Character range 'A' .. 'Z';

   begin
      if Char1'Width /= 3 then
         Failed ("INCORRECT WIDTH FOR CHAR1");
      end if;

      if Char2'Width /= 3 then
         Failed ("INCORRECT WIDTH FOR CHAR2");
      end if;

      if Newchar'Width /= 3 then
         Failed ("INCORRECT WIDTH FOR NEWCHAR");
      end if;

      if Nochar'Width /= 0 then
         Failed ("INCORRECT WIDTH FOR NOCHAR");
      end if;
   end;

   declare
      subtype Nongraph is
        Character range Character'Val (0) .. Character'Val (31);

      Max : Integer := 0;

   begin
      for Ch in Nongraph loop
         if Character'Image (Ch)'Length > Max then
            Max := Character'Image (Ch)'Length;
         end if;
      end loop;

      if Nongraph'Width /= Max then
         Failed ("INCORRECT WIDTH FOR NONGRAPH");
      end if;
   end;

   Result;
end C35507a;
