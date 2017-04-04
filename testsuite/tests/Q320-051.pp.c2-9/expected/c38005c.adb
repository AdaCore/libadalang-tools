-- C38005C.ADA

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
--     CHECK THAT ALL OBJECTS OF FORMAL ACCESS TYPE, INCLUDING ARRAY AND
--     RECORD COMPONENTS, ARE INITIALIZED BY DEFAULT WITH THE VALUE
--     NULL.

-- HISTORY:
--     DHH 08/04/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C38005c is

   subtype Int is Integer range 1 .. 10;

   type Acc_I is access Int;

   subtype New_Node is Character;

   type Acc_Char is access New_Node;

   X : Acc_I    := new Int'(Ident_Int (5));
   Y : New_Node := 'A';
   Z : Acc_Char := new New_Node'(Y);

   generic
      type Acc_Int is access Int;
      type Node is private;
      type Link is access Node;
   procedure P (U : Acc_Int; V : Node; W : Link);

   generic
      type Acc_Int is access Int;
      type Node is private;
      type Link is access Node;
   package Pack is

      subtype New_Acc is Acc_Int;

      subtype New_L is Link;

      type Arr is array (1 .. 4) of Acc_Int;

      type Rec is record
         I : Acc_Int;
         L : Link;
      end record;

   end Pack;

   package New_Pack is new Pack (Acc_I, New_Node, Acc_Char);
   use New_Pack;

   A : New_Pack.New_Acc;
   B : New_Pack.New_L;
   C : New_Pack.Arr;
   D : New_Pack.Rec;

   procedure P (U : Acc_Int; V : Node; W : Link) is

      type Arr is array (1 .. 4) of Acc_Int;

      type Rec is record
         I : Acc_Int;
         L : Link;
      end record;

      A : Acc_Int;
      B : Link;
      C : Arr;
      D : Rec;

   begin
      if A /= null then
         Failed ("OBJECT A NOT INITIALIZED - PROC");
      end if;

      if B /= null then
         Failed ("OBJECT B NOT INITIALIZED - PROC");
      end if;

      for I in 1 .. 4 loop
         if C (I) /= null then
            Failed ("ARRAY " & Integer'Image (I) & "NOT INITIALIZED - PROC");
         end if;
      end loop;

      if D.I /= null then
         Failed ("RECORD.I NOT INITIALIZED - PROC");
      end if;

      if D.L /= null then
         Failed ("RECORD.L NOT INITIALIZED - PROC");
      end if;

   end P;

   procedure Proc is new P (Acc_I, New_Node, Acc_Char);

begin
   Test
     ("C38005C",
      "CHECK THAT ALL OBJECTS OF FORMAL ACCESS TYPE, " &
      "INCLUDING ARRAY AND RECORD COMPONENTS, ARE " &
      "INITIALIZED BY DEFAULT WITH THE VALUE NULL");

   Proc (X, Y, Z);

   if A /= null then
      Failed ("OBJECT A NOT INITIALIZED - PACK");
   end if;

   if B /= null then
      Failed ("OBJECT B NOT INITIALIZED - PACK");
   end if;

   for I in 1 .. 4 loop
      if C (I) /= null then
         Failed ("ARRAY " & Integer'Image (I) & "NOT INITIALIZED - PACK");
      end if;
   end loop;

   if D.I /= null then
      Failed ("RECORD.I NOT INITIALIZED - PACK");
   end if;

   if D.L /= null then
      Failed ("RECORD.L NOT INITIALIZED - PACK");
   end if;

   Result;
end C38005c;
