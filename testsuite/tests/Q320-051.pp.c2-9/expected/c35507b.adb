-- C35507B.ADA

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
-- IS FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS A CHARACTER TYPE.

-- RJW 5/29/86

with Report; use Report;

procedure C35507b is

   generic
      type Ch is (<>);
   procedure P (Str : String; W : Integer);

   procedure P (Str : String; W : Integer) is

      subtype Nochar is Ch range Ch'Val (1) .. Ch'Val (0);
   begin
      if Ch'Width /= W then
         Failed ("INCORRECT WIDTH FOR " & Str);
      end if;

      if Nochar'Width /= 0 then
         Failed ("INCORRECT WIDTH FOR NOCHAR WITH " & Str);
      end if;
   end P;

begin

   Test
     ("C35507B",
      "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
      "THE CORRECT RESULTS WHEN THE PREFIX " &
      "IS A FORMAL DISCRETE TYPE WHOSE ACTUAL " &
      "PARAMETER IS A CHARACTER TYPE");

   declare
      type Char1 is (A, 'A');

      subtype Char2 is Character range 'A' .. 'Z';

      type Newchar is new Character range 'A' .. 'Z';

      procedure P1 is new P (Char1);
      procedure P2 is new P (Char2);
      procedure P3 is new P (Newchar);
   begin
      P1 ("CHAR1", 3);
      P2 ("CHAR2", 3);
      P3 ("NEWCHAR", 3);
   end;

   declare
      subtype Nongraph is
        Character range Character'Val (0) .. Character'Val (31);

      Max : Integer := 0;

      procedure Pn is new P (Nongraph);
   begin
      for Ch in Nongraph loop
         if Character'Image (Ch)'Length > Max then
            Max := Character'Image (Ch)'Length;
         end if;
      end loop;

      Pn ("NONGRAPH", Max);
   end;

   Result;
end C35507b;
