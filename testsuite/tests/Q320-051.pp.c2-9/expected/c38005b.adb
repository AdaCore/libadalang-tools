-- C38005B.ADA

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
--     CHECK THAT ANY OBJECT WITH A FORMAL PRIVATE TYPE, WHOSE ACTUAL
--     TYPE IN AN INSTANTIATION IS AN ACCESS TYPE, IS INITIALIZED BY
--     DEFAULT TO THE VALUE NULL. THIS INCLUDES OBJECTS WHICH ARE ARRAY
--     AND RECORD COMPONENTS.

-- HISTORY:
--     DHH 07/12/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C38005b is

begin
   Test
     ("C38005B",
      "CHECK THAT ANY OBJECT WITH A FORMAL PRIVATE " &
      "TYPE, WHOSE ACTUAL TYPE IN AN INSTANTIATION " &
      "IS AN ACCESS TYPE, IS INITIALIZED BY DEFAULT " &
      "TO THE VALUE NULL. THIS INCLUDES OBJECTS WHICH " &
      "ARE ARRAY AND RECORD COMPONENTS");
   declare
      type Arry is array (1 .. 10) of Boolean;
      type Rec1 is record
         A : Integer;
         B : Arry;
      end record;

      type Pointer is access Rec1;

      generic
         type New_Ptr is private;
      package Gen_Pack is
         type Ptr_Ary is array (1 .. 5) of New_Ptr;
         type Record1 is record
            A : New_Ptr;
            B : Ptr_Ary;
         end record;

         Obj : New_Ptr;
         Ary : Ptr_Ary;
         Rec : Record1;
      end Gen_Pack;

      package Test_P is new Gen_Pack (Pointer);
      use Test_P;

   begin
      if Obj /= null then
         Failed ("OBJECT NOT INITIALIZED TO NULL");
      end if;

      for I in 1 .. 5 loop
         if Ary (I) /= null then
            Failed
              ("ARRAY COMPONENT " & Integer'Image (I) &
               " NOT INITIALIZED TO NULL");
         end if;
      end loop;

      if Rec.A /= null then
         Failed ("RECORD OBJECT NOT INITIALIZED TO NULL");
      end if;

      for I in 1 .. 5 loop
         if Rec.B (I) /= null then
            Failed
              ("RECORD SUBCOMPONENT " & Integer'Image (I) &
               " NOT INITIALIZED TO NULL");
         end if;
      end loop;
   end;

   Result;
end C38005b;
