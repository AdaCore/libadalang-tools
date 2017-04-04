-- C41404A.ADA

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
-- CHECK THAT THE PREFIX OF THE ARRAY ATTRIBUTES CAN BE THE VALUE OF AN
-- IMAGE ATTRIBUTE.

-- JBG 6/1/85
-- PWB 2/3/86  CORRECTED COMPARISON VALUES FOR 'LAST AND 'LENGTH.

with Report; use Report;
procedure C41404a is

   type Enum is (One, Four, 'C');

begin

   Test ("C41404A", "CHECK WHEN PREFIX OF AN ATTRIBUTE IS 'IMAGE");

   if Enum'Image (Four)'Length /= Ident_Int (4) then
      Failed ("WRONG VALUE FOR LENGTH - ENUM");
   end if;

   if Enum'Image ('C')'Length /= Ident_Int (3) then
      Failed ("WRONG VALUE FOR LENGTH - ENUM: 'C'");
   end if;

   if Integer'Image (Ident_Int (56))'Length /= Ident_Int (3) then
      Failed ("WRONG VALUE FOR LENGTH - INTEGER: 56");
   end if;

   if Character'Image (Ident_Char ('B'))'Length /= Ident_Int (3) then
      Failed ("WRONG VALUE FOR LENGTH - CHAR: 'B'");
   end if;

   if Enum'Image (Four)'First /= Ident_Int (1) then
      Failed ("WRONG VALUE FOR FIRST - ENUM");
   end if;

   if Enum'Image ('C')'First (1) /= Ident_Int (1) then
      Failed ("WRONG VALUE FOR FIRST - ENUM: 'C'");
   end if;

   if Integer'Image (Ident_Int (56))'First /= Ident_Int (1) then
      Failed ("WRONG VALUE FOR FIRST - INTEGER: 56");
   end if;

   if Character'Image (Ident_Char ('B'))'First /= Ident_Int (1) then
      Failed ("WRONG VALUE FOR FIRST - CHAR: 'B'");
   end if;

   if Enum'Image (Four)'Last /= Ident_Int (4) then
      Failed ("WRONG VALUE FOR LAST - ENUM");
   end if;

   if Enum'Image ('C')'Last (1) /= Ident_Int (3) then
      Failed ("WRONG VALUE FOR LAST - ENUM: 'C'");
   end if;

   if Integer'Image (Ident_Int (-56))'Last /= Ident_Int (3) then
      Failed ("WRONG VALUE FOR LAST - INTEGER: -56");
   end if;

   if Character'Image (Ident_Char ('B'))'Last /= Ident_Int (3) then
      Failed ("WRONG VALUE FOR LAST - CHAR: 'B'");
   end if;

   declare

      Four_Var : String (Enum'Image (Four)'Range);
      C_Var    : String (Enum'Image ('C')'Range);
      Var_101  : String (Integer'Image (Ident_Int (101))'Range);
      Char_Var : String (Character'Image (Ident_Char ('B'))'Range);

   begin

      if Four_Var'First /= 1 or Four_Var'Last /= 4 or Four_Var'Length /= 4 then
         Failed
           ("FOUR_VAR ATTRIBUTES INCORRECT.  FIRST IS" &
            Integer'Image (Four_Var'First) &
            ".  LAST IS" &
            Integer'Image (Four_Var'Last) &
            ".  LENGTH IS" &
            Integer'Image (Four_Var'Length));
      end if;

      if C_Var'First /= 1 or C_Var'Last /= 3 or C_Var'Length /= 3 then
         Failed
           ("C_VAR ATTRIBUTES INCORRECT.  FIRST IS" &
            Integer'Image (C_Var'First) &
            ".  LAST IS" &
            Integer'Image (C_Var'Last) &
            ".  LENGTH IS" &
            Integer'Image (C_Var'Length));
      end if;

      if Var_101'First /= 1 or Var_101'Last /= 4 or Var_101'Length /= 4 then
         Failed
           ("VAR_101 ATTRIBUTES INCORRECT.  FIRST IS" &
            Integer'Image (Var_101'First) &
            ".  LAST IS" &
            Integer'Image (Var_101'Last) &
            ".  LENGTH IS" &
            Integer'Image (Var_101'Length));
      end if;

      if Char_Var'First /= 1 or Char_Var'Last /= 3 or Char_Var'Length /= 3 then
         Failed
           ("CHAR_VAR ATTRIBUTES INCORRECT.  FIRST IS" &
            Integer'Image (Char_Var'First) &
            ".  LAST IS" &
            Integer'Image (Char_Var'Last) &
            ".  LENGTH IS" &
            Integer'Image (Char_Var'Length));
      end if;

   end;

   Result;
end C41404a;
