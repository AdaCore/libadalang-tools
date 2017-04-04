-- C85006F.ADA

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
-- OBJECTIVE:
--     CHECK THAT A RENAMED SLICE CAN BE SLICED AND INDEXED FOR PURPOSES
--     OF ASSIGNMENT AND TO READ THE VALUE.

-- HISTORY:
--     JET 07/26/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85006f is

   S : String (1 .. 30) := "IT WAS A DARK AND STORMY NIGHT";

   Adjectives : String renames S (10 .. 24);

begin
   Test
     ("C85006F",
      "CHECK THAT A RENAMED SLICE CAN BE SLICED AND " &
      "INDEXED FOR PURPOSES OF ASSIGNMENT AND TO " &
      "READ THE VALUE");

   Adjectives (19 .. 24) := "STARRY";

   if Adjectives /= Ident_Str ("DARK AND STARRY") then
      Failed ("INCORRECT VALUE OF SLICE AFTER ASSIGNMENT (1)");
   end if;

   if S /= Ident_Str ("IT WAS A DARK AND STARRY NIGHT") then
      Failed ("INCORRECT VALUE OF ORIGINAL STRING (1)");
   end if;

   Adjectives (17) := ''';

   if Adjectives /= Ident_Str ("DARK AN' STARRY") then
      Failed ("INCORRECT VALUE OF SLICE AFTER ASSIGNMENT (2)");
   end if;

   if S /= Ident_Str ("IT WAS A DARK AN' STARRY NIGHT") then
      Failed ("INCORRECT VALUE OF ORIGINAL STRING (2)");
   end if;

   if Adjectives (10 .. 13) /= Ident_Str ("DARK") then
      Failed ("INCORRECT VALUE OF SLICE WHEN READING");
   end if;

   Result;

end C85006f;
