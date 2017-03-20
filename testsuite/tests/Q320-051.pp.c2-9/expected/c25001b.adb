-- C25001B.ADA

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
-- CHECK THAT ALL CHARACTER LITERALS CAN BE WRITTEN.

--      CASE B: THE LOWER CASE LETTERS AND THE OTHER
--              SPECIAL CHARACTERS.

-- TBN  8/1/86

with Report; use Report;
procedure C25001b is

begin
   Test
     ("C25001B",
      "CHECK THAT EACH CHARACTER IN THE LOWER CASE " &
      "LETTERS AND THE OTHER SPECIAL CHARACTERS CAN " &
      "BE WRITTEN");

   if Character'Pos ('a') /= 97 then
      Failed ("INCORRECT POSITION NUMBER FOR 'a'");
   end if;
   if Character'Pos ('b') /= 98 then
      Failed ("INCORRECT POSITION NUMBER FOR 'b'");
   end if;
   if Character'Pos ('c') /= 99 then
      Failed ("INCORRECT POSITION NUMBER FOR 'c'");
   end if;
   if Character'Pos ('d') /= 100 then
      Failed ("INCORRECT POSITION NUMBER FOR 'd'");
   end if;
   if Character'Pos ('e') /= 101 then
      Failed ("INCORRECT POSITION NUMBER FOR 'e'");
   end if;
   if Character'Pos ('f') /= 102 then
      Failed ("INCORRECT POSITION NUMBER FOR 'f'");
   end if;
   if Character'Pos ('g') /= 103 then
      Failed ("INCORRECT POSITION NUMBER FOR 'g'");
   end if;
   if Character'Pos ('h') /= 104 then
      Failed ("INCORRECT POSITION NUMBER FOR 'h'");
   end if;
   if Character'Pos ('i') /= 105 then
      Failed ("INCORRECT POSITION NUMBER FOR 'i'");
   end if;
   if Character'Pos ('j') /= 106 then
      Failed ("INCORRECT POSITION NUMBER FOR 'j'");
   end if;
   if Character'Pos ('k') /= 107 then
      Failed ("INCORRECT POSITION NUMBER FOR 'k'");
   end if;
   if Character'Pos ('l') /= 108 then
      Failed ("INCORRECT POSITION NUMBER FOR 'l'");
   end if;
   if Character'Pos ('m') /= 109 then
      Failed ("INCORRECT POSITION NUMBER FOR 'm'");
   end if;
   if Character'Pos ('n') /= 110 then
      Failed ("INCORRECT POSITION NUMBER FOR 'n'");
   end if;
   if Character'Pos ('o') /= 111 then
      Failed ("INCORRECT POSITION NUMBER FOR 'o'");
   end if;
   if Character'Pos ('p') /= 112 then
      Failed ("INCORRECT POSITION NUMBER FOR 'p'");
   end if;
   if Character'Pos ('q') /= 113 then
      Failed ("INCORRECT POSITION NUMBER FOR 'q'");
   end if;
   if Character'Pos ('r') /= 114 then
      Failed ("INCORRECT POSITION NUMBER FOR 'r'");
   end if;
   if Character'Pos ('s') /= 115 then
      Failed ("INCORRECT POSITION NUMBER FOR 's'");
   end if;
   if Character'Pos ('t') /= 116 then
      Failed ("INCORRECT POSITION NUMBER FOR 't'");
   end if;
   if Character'Pos ('u') /= 117 then
      Failed ("INCORRECT POSITION NUMBER FOR 'u'");
   end if;
   if Character'Pos ('v') /= 118 then
      Failed ("INCORRECT POSITION NUMBER FOR 'v'");
   end if;
   if Character'Pos ('w') /= 119 then
      Failed ("INCORRECT POSITION NUMBER FOR 'w'");
   end if;
   if Character'Pos ('x') /= 120 then
      Failed ("INCORRECT POSITION NUMBER FOR 'x'");
   end if;
   if Character'Pos ('y') /= 121 then
      Failed ("INCORRECT POSITION NUMBER FOR 'y'");
   end if;
   if Character'Pos ('z') /= 122 then
      Failed ("INCORRECT POSITION NUMBER FOR 'z'");
   end if;

   if Character'Pos ('!') /= 33 then
      Failed ("INCORRECT POSITION NUMBER FOR '!'");
   end if;
   if Character'Pos ('$') /= 36 then
      Failed ("INCORRECT POSITION NUMBER FOR '$'");
   end if;
   if Character'Pos ('%') /= 37 then
      Failed ("INCORRECT POSITION NUMBER FOR '%'");
   end if;
   if Character'Pos ('?') /= 63 then
      Failed ("INCORRECT POSITION NUMBER FOR '?'");
   end if;
   if Character'Pos ('@') /= 64 then
      Failed ("INCORRECT POSITION NUMBER FOR '@'");
   end if;
   if Character'Pos ('[') /= 91 then
      Failed ("INCORRECT POSITION NUMBER FOR '['");
   end if;
   if Character'Pos ('\') /= 92 then
      Failed ("INCORRECT POSITION NUMBER FOR '\'");
   end if;
   if Character'Pos (']') /= 93 then
      Failed ("INCORRECT POSITION NUMBER FOR ']'");
   end if;
   if Character'Pos ('^') /= 94 then
      Failed ("INCORRECT POSITION NUMBER FOR '^'");
   end if;
   if Character'Pos ('`') /= 96 then
      Failed ("INCORRECT POSITION NUMBER FOR '`'");
   end if;
   if Character'Pos ('{') /= 123 then
      Failed ("INCORRECT POSITION NUMBER FOR '{'");
   end if;
   if Character'Pos ('}') /= 125 then
      Failed ("INCORRECT POSITION NUMBER FOR '}'");
   end if;
   if Character'Pos ('~') /= 126 then
      Failed ("INCORRECT POSITION NUMBER FOR '~'");
   end if;

   Result;
end C25001b;
