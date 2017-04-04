-- C48012A.ADA

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
-- CHECK THAT DISCRIMINANTS GOVERNING VARIANT PARTS NEED NOT BE
-- SPECIFIED WITH STATIC VALUES IN AN ALLOCATOR OF THE FORM
-- "NEW T X".

-- EG  08/30/84

with Report;

procedure C48012a is

   use Report;

begin

   Test
     ("C48012A",
      "CHECK THAT DISCRIMINANTS GOVERNING VARIANT " &
      "PARTS NEED NOT BE SPECIFIED WITH STATIC " &
      "VALUES IN AN ALLOCATOR OF THE FORM 'NEW T X'");

   declare

      type Int is range 1 .. 5;
      type Arr is array (Int range <>) of Integer;

      type Ur (A : Int) is record
         case A is
            when 1 =>
               null;
            when others =>
               B : Arr (1 .. A);
         end case;
      end record;

      type A_Ur is access Ur;

      V_A_Ur : A_Ur;

   begin

      V_A_Ur := new Ur (A => Int (Ident_Int (2)));
      if V_A_Ur.A /= 2 then
         Failed ("WRONG DISCRIMINANT VALUE");
      elsif V_A_Ur.B'First /= 1 and V_A_Ur.B'Last /= 2 then
         Failed ("WRONG BOUNDS IN VARIANT PART");
      end if;

   end;

   Result;

end C48012a;
