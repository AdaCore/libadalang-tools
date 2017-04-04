-- C25001A.ADA

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

--      CASE A: THE BASIC CHARACTER SET.

-- TBN  3/17/86

with Report; use Report;
procedure C25001a is

begin
   Test
     ("C25001A",
      "CHECK THAT EACH CHARACTER IN THE BASIC " &
      "CHARACTER SET CAN BE WRITTEN");

   if Character'Pos ('A') /= 65 then
      Failed ("INCORRECT POSITION NUMBER FOR 'A'");
   end if;
   if Character'Pos ('B') /= 66 then
      Failed ("INCORRECT POSITION NUMBER FOR 'B'");
   end if;
   if Character'Pos ('C') /= 67 then
      Failed ("INCORRECT POSITION NUMBER FOR 'C'");
   end if;
   if Character'Pos ('D') /= 68 then
      Failed ("INCORRECT POSITION NUMBER FOR 'D'");
   end if;
   if Character'Pos ('E') /= 69 then
      Failed ("INCORRECT POSITION NUMBER FOR 'E'");
   end if;
   if Character'Pos ('F') /= 70 then
      Failed ("INCORRECT POSITION NUMBER FOR 'F'");
   end if;
   if Character'Pos ('G') /= 71 then
      Failed ("INCORRECT POSITION NUMBER FOR 'G'");
   end if;
   if Character'Pos ('H') /= 72 then
      Failed ("INCORRECT POSITION NUMBER FOR 'H'");
   end if;
   if Character'Pos ('I') /= 73 then
      Failed ("INCORRECT POSITION NUMBER FOR 'I'");
   end if;
   if Character'Pos ('J') /= 74 then
      Failed ("INCORRECT POSITION NUMBER FOR 'J'");
   end if;
   if Character'Pos ('K') /= 75 then
      Failed ("INCORRECT POSITION NUMBER FOR 'K'");
   end if;
   if Character'Pos ('L') /= 76 then
      Failed ("INCORRECT POSITION NUMBER FOR 'L'");
   end if;
   if Character'Pos ('M') /= 77 then
      Failed ("INCORRECT POSITION NUMBER FOR 'M'");
   end if;
   if Character'Pos ('N') /= 78 then
      Failed ("INCORRECT POSITION NUMBER FOR 'N'");
   end if;
   if Character'Pos ('O') /= 79 then
      Failed ("INCORRECT POSITION NUMBER FOR 'O'");
   end if;
   if Character'Pos ('P') /= 80 then
      Failed ("INCORRECT POSITION NUMBER FOR 'P'");
   end if;
   if Character'Pos ('Q') /= 81 then
      Failed ("INCORRECT POSITION NUMBER FOR 'Q'");
   end if;
   if Character'Pos ('R') /= 82 then
      Failed ("INCORRECT POSITION NUMBER FOR 'R'");
   end if;
   if Character'Pos ('S') /= 83 then
      Failed ("INCORRECT POSITION NUMBER FOR 'S'");
   end if;
   if Character'Pos ('T') /= 84 then
      Failed ("INCORRECT POSITION NUMBER FOR 'T'");
   end if;
   if Character'Pos ('U') /= 85 then
      Failed ("INCORRECT POSITION NUMBER FOR 'U'");
   end if;
   if Character'Pos ('V') /= 86 then
      Failed ("INCORRECT POSITION NUMBER FOR 'V'");
   end if;
   if Character'Pos ('W') /= 87 then
      Failed ("INCORRECT POSITION NUMBER FOR 'W'");
   end if;
   if Character'Pos ('X') /= 88 then
      Failed ("INCORRECT POSITION NUMBER FOR 'X'");
   end if;
   if Character'Pos ('Y') /= 89 then
      Failed ("INCORRECT POSITION NUMBER FOR 'Y'");
   end if;
   if Character'Pos ('Z') /= 90 then
      Failed ("INCORRECT POSITION NUMBER FOR 'Z'");
   end if;

   if Character'Pos ('0') /= 48 then
      Failed ("INCORRECT POSITION NUMBER FOR '0'");
   end if;
   if Character'Pos ('1') /= 49 then
      Failed ("INCORRECT POSITION NUMBER FOR '1'");
   end if;
   if Character'Pos ('2') /= 50 then
      Failed ("INCORRECT POSITION NUMBER FOR '2'");
   end if;
   if Character'Pos ('3') /= 51 then
      Failed ("INCORRECT POSITION NUMBER FOR '3'");
   end if;
   if Character'Pos ('4') /= 52 then
      Failed ("INCORRECT POSITION NUMBER FOR '4'");
   end if;
   if Character'Pos ('5') /= 53 then
      Failed ("INCORRECT POSITION NUMBER FOR '5'");
   end if;
   if Character'Pos ('6') /= 54 then
      Failed ("INCORRECT POSITION NUMBER FOR '6'");
   end if;
   if Character'Pos ('7') /= 55 then
      Failed ("INCORRECT POSITION NUMBER FOR '7'");
   end if;
   if Character'Pos ('8') /= 56 then
      Failed ("INCORRECT POSITION NUMBER FOR '8'");
   end if;
   if Character'Pos ('9') /= 57 then
      Failed ("INCORRECT POSITION NUMBER FOR '9'");
   end if;

   if Character'Pos ('"') /= 34 then
      Failed ("INCORRECT POSITION NUMBER FOR '""'");
   end if;
   if Character'Pos ('#') /= 35 then
      Failed ("INCORRECT POSITION NUMBER FOR '#'");
   end if;
   if Character'Pos ('&') /= 38 then
      Failed ("INCORRECT POSITION NUMBER FOR '&'");
   end if;
   if Character'Pos (''') /= 39 then
      Failed ("INCORRECT POSITION NUMBER FOR '''");
   end if;
   if Character'Pos ('(') /= 40 then
      Failed ("INCORRECT POSITION NUMBER FOR '('");
   end if;
   if Character'Pos (')') /= 41 then
      Failed ("INCORRECT POSITION NUMBER FOR ')'");
   end if;
   if Character'Pos ('*') /= 42 then
      Failed ("INCORRECT POSITION NUMBER FOR '*'");
   end if;
   if Character'Pos ('+') /= 43 then
      Failed ("INCORRECT POSITION NUMBER FOR '+'");
   end if;
   if Character'Pos (',') /= 44 then
      Failed ("INCORRECT POSITION NUMBER FOR ','");
   end if;
   if Character'Pos ('-') /= 45 then
      Failed ("INCORRECT POSITION NUMBER FOR '-'");
   end if;
   if Character'Pos ('.') /= 46 then
      Failed ("INCORRECT POSITION NUMBER FOR '.'");
   end if;
   if Character'Pos ('/') /= 47 then
      Failed ("INCORRECT POSITION NUMBER FOR '/'");
   end if;
   if Character'Pos (':') /= 58 then
      Failed ("INCORRECT POSITION NUMBER FOR ':'");
   end if;
   if Character'Pos (';') /= 59 then
      Failed ("INCORRECT POSITION NUMBER FOR ';'");
   end if;
   if Character'Pos ('<') /= 60 then
      Failed ("INCORRECT POSITION NUMBER FOR '<'");
   end if;
   if Character'Pos ('=') /= 61 then
      Failed ("INCORRECT POSITION NUMBER FOR '='");
   end if;
   if Character'Pos ('>') /= 62 then
      Failed ("INCORRECT POSITION NUMBER FOR '>'");
   end if;
   if Character'Pos ('_') /= 95 then
      Failed ("INCORRECT POSITION NUMBER FOR '_'");
   end if;
   if Character'Pos ('|') /= 124 then
      Failed ("INCORRECT POSITION NUMBER FOR '|'");
   end if;

   if Character'Pos (' ') /= 32 then
      Failed ("INCORRECT POSITION NUMBER FOR ' '");
   end if;

   Result;
end C25001a;
