-- C95041A.ADA

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
--     CHECK THAT AN ENTRY FAMILY INDEX CAN BE SPECIFIED WITH THE FORM
--     A'RANGE.

-- HISTORY:
--     DHH 03/17/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C95041a is

   Global_A, Global_B : Integer;
   Global_C, Global_D : Integer;
   type Color is (Red, Blue, Yellow);
   type Arr is array (Color range Red .. Blue) of Boolean;
   Arry : Arr;

   task Check is
      entry Check_Link (Arr'Range) (I : Integer);
   end Check;

   task Check_Obj is
      entry Check_Obj_Link (Arry'Range) (I : Integer);
   end Check_Obj;

   task body Check is
   begin
      accept Check_Link (Red) (I : Integer) do
         Global_A := Ident_Int (I);
      end Check_Link;

      accept Check_Link (Blue) (I : Integer) do
         Global_B := Ident_Int (I);
      end Check_Link;
   end Check;

   task body Check_Obj is
   begin
      accept Check_Obj_Link (Red) (I : Integer) do
         Global_C := Ident_Int (I);
      end Check_Obj_Link;

      accept Check_Obj_Link (Blue) (I : Integer) do
         Global_D := Ident_Int (I);
      end Check_Obj_Link;
   end Check_Obj;

begin
   Test
     ("C95041A",
      "CHECK THAT AN ENTRY FAMILY INDEX CAN BE " &
      "SPECIFIED WITH THE FORM A'RANGE");
   Check.Check_Link (Red) (10);
   Check.Check_Link (Blue) (5);

   Check_Obj.Check_Obj_Link (Red) (10);
   Check_Obj.Check_Obj_Link (Blue) (5);

   if Global_A /= Ident_Int (10) then
      Failed ("ENTRY CHECK_LINK(RED) HAS INCORRECT VALUE");
   end if;

   if Global_B /= Ident_Int (5) then
      Failed ("ENTRY CHECK_LINK(BLUE) HAS INCORRECT VALUE");
   end if;

   if Global_C /= Ident_Int (10) then
      Failed ("ENTRY CHECK_LINK(RED) HAS INCORRECT VALUE");
   end if;

   if Global_D /= Ident_Int (5) then
      Failed ("ENTRY CHECK_LINK(BLUE) HAS INCORRECT VALUE");
   end if;

   Result;
end C95041a;
