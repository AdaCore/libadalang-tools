-- C41320A.ADA

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
--    CHECK THAT IMPLICITLY DECLARED ENUMERATION LITERALS, CHARACTER
--    LITERALS, AND THE RELATIONAL OPERATORS CAN BE SELECTED FROM
--    OUTSIDE THE PACKAGE USING AN EXPANDED NAME, FOR ENUMERATION TYPES.

-- HISTORY:
--    TBN 07/15/86  CREATED ORIGINAL TEST.
--    JET 08/04/87  ADDED TEST FOR OVERLOADED VARIABLES.

with Report; use Report;
procedure C41320a is

   package P is
      type Flag is (Red, White, Blue);
      type Roman_Digits is ('I', 'V', 'X', 'C', 'M');
      type Traffic_Light is (Red, Yellow, Green);
      type Hex is ('A', 'B', 'C', 'D', 'E', 'F');
      Flag_Color_1          : Flag         := Red;
      Flag_Color_2          : Flag         := White;
      Traffic_Light_Color_1 : Flag         := Red;
      Hex_3                 : Hex          := 'C';
      Roman_1               : Roman_Digits := 'I';
   end P;

   Usa_Flag_1      : P.Flag          := P.Red;
   Usa_Flag_3      : P.Flag          := P.Blue;
   Hex_Char_3      : P.Hex           := P.'C';
   Roman_Digits_4  : P.Roman_Digits  := P.'C';
   Traffic_Light_1 : P.Traffic_Light := P.Red;

begin
   Test
     ("C41320A",
      "CHECK THAT IMPLICITLY DECLARED ENUMERATION " &
      "LITERALS, CHARACTER LITERALS, AND THE " &
      "RELATIONAL OPERATORS CAN BE SELECTED FROM " &
      "OUTSIDE THE PACKAGE USING AN EXPANDED NAME " &
      "FOR ENUMERATION TYPES");

   if P."/=" (Usa_Flag_1, P.Flag_Color_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."=" (Usa_Flag_3, P.Flag_Color_2) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   if P."<" (Hex_Char_3, P.Hex_3) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
   end if;

   if P.">" (P.Roman_1, Roman_Digits_4) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
   end if;

   if P.">=" (Traffic_Light_1, P.Traffic_Light'Pred (P.Green)) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
   end if;

   for J in P.Flag'(P.White) .. P.Flag'(P.White) loop
      if P."<=" (P.Flag'Succ (P.White), J) then
         Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
      end if;
   end loop;

   if P.">=" (P.Red, P.Green) then
      Failed ("INCORRECT RESULT FROM OVERLOADED VARIABLE NAME - 1");
   end if;

   if P."<=" (P.Blue, P.Red) then
      Failed ("INCORRECT RESULT FROM OVERLOADED VARIABLE NAME - 2");
   end if;

   Result;
end C41320a;
