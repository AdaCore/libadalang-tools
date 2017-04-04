-- C54A03A.ADA

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
-- CHECK THAT BOOLEAN, CHARACTER, USER-DEFINED ENUMERATED, INTEGER,
--    AND DERIVED TYPES MAY BE USED IN A CASE EXPRESSION.

-- DAT 1/22/81
-- PWB 4/22/86  RENAME TO -AB;
--              REMOVE EXTRANEOUS <CR> FROM BEGINNING OF LINE 45.

with Report;
procedure C54a03a is

   use Report;

   type D_Int is new Integer range 1 .. 2;
   type D_Bool is new Boolean;
   type D_Bool_2 is new D_Bool;
   type M_Enum is (First, Second, Third);
   type M_Char is new Character range Ascii.Nul .. 'Z';
   type M_Enum_2 is new M_Enum;

   I     : Integer   := 1;
   D_I   : D_Int     := 1;
   B     : Boolean   := True;
   D_B   : D_Bool    := True;
   D_B_2 : D_Bool_2  := False;
   E     : M_Enum    := Third;
   C     : Character := 'A';
   M_C   : M_Char    := 'Z';
   D_E   : M_Enum_2  := Second;

begin
   Test ("C54A03A", "CHECK VARIOUS DISCRETE TYPES " & "IN CASE EXPRESSIONS");

   case I is
      when 2 | 3 =>
         Failed ("WRONG CASE 1");
      when 1 =>
         null;
      when others =>
         Failed ("WRONG CASE 2");
   end case;

   case D_I is
      when 1 =>
         null;
      when 2 =>
         Failed ("WRONG CASE 2A");
   end case;

   case B is
      when True =>
         null;
      when False =>
         Failed ("WRONG CASE 3");
   end case;

   case D_B is
      when True =>
         null;
      when False =>
         Failed ("WRONG CASE 4");
   end case;

   case D_B_2 is
      when False =>
         null;
      when True =>
         Failed ("WRONG CASE 5");
   end case;

   case E is
      when Second | First =>
         Failed ("WRONG CASE 6");
      when Third =>
         null;
   end case;

   case C is
      when 'A' .. 'Z' =>
         null;
      when others =>
         Failed ("WRONG CASE 7");
   end case;

   case M_C is
      when 'Z' =>
         null;
      when others =>
         Failed ("WRONG CASE 8");
   end case;

   case D_E is
      when First =>
         Failed ("WRONG CASE 9");
      when Second | Third =>
         null;
   end case;

   Result;
end C54a03a;
