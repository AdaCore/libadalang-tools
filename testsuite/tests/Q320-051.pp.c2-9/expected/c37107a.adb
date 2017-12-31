-- C37107A.ADA

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
-- CHECK THAT A DEFAULT DISCRIMINANT EXPRESSION NEED NOT BE STATIC AND IS
-- EVALUATED ONLY WHEN NEEDED.

-- R.WILLIAMS 8/25/86
-- GMT 6/29/87 ADDED INTEGER ARGUMENT TO THE FUNCTION F.

with Report; use Report;
procedure C37107a is

   function F (B : Boolean; I : Integer) return Integer is
   begin
      if not B then
         Failed
           ("DEFAULT DISCRIMINANT EVALUATED " & "UNNECESSARILY - " &
            Integer'Image (I));
      end if;

      return Ident_Int (1);
   end F;

begin
   Test
     ("C37107A",
      "CHECK THAT A DEFAULT DISCRIMINANT " &
      "EXPRESSION NEED NOT BE STATIC AND IS " & "EVALUATED ONLY WHEN NEEDED");

   declare
      type Rec1 (D : Integer := F (True, 1)) is record
         null;
      end record;

      R1 : Rec1;

      type Rec2 (D : Integer := F (False, 2)) is record
         null;
      end record;

      R2 : Rec2 (D => 0);

   begin
      if R1.D /= 1 then
         Failed ("INCORRECT VALUE FOR R1.D");
      end if;

      if R2.D /= 0 then
         Failed ("INCORRECT VALUE FOR R2.D");
      end if;
   end;

   declare

      package Priv is
         type Rec3 (D : Integer := F (True, 3)) is private;
         type Rec4 (D : Integer := F (False, 4)) is private;

      private
         type Rec3 (D : Integer := F (True, 3)) is record
            null;
         end record;

         type Rec4 (D : Integer := F (False, 4)) is record
            null;
         end record;
      end Priv;

      use Priv;

   begin
      declare
         R3 : Rec3;
         R4 : Rec4 (D => 0);

      begin
         if R3.D /= 1 then
            Failed ("INCORRECT VALUE FOR R3.D");
         end if;

         if R4.D /= 0 then
            Failed ("INCORRECT VALUE FOR R4.D");
         end if;
      end;

   end;

   declare

      package Lpriv is
         type Rec5 (D : Integer := F (True, 5)) is limited private;
         type Rec6 (D : Integer := F (False, 6)) is limited private;

      private
         type Rec5 (D : Integer := F (True, 5)) is record
            null;
         end record;

         type Rec6 (D : Integer := F (False, 6)) is record
            null;
         end record;
      end Lpriv;

      use Lpriv;

   begin
      declare
         R5 : Rec5;
         R6 : Rec6 (D => 0);

      begin
         if R5.D /= 1 then
            Failed ("INCORRECT VALUE FOR R5.D");
         end if;

         if R6.D /= 0 then
            Failed ("INCORRECT VALUE FOR R6.D");
         end if;
      end;

   end;

   Result;
end C37107a;
