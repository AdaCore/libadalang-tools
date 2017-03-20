-- C37010A.ADA

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
-- CHECK THAT EXPRESSIONS IN CONSTRAINTS OF COMPONENT DECLARATIONS ARE
-- EVALUATED IN THE ORDER THE COMPONENTS APPEAR.

-- R.WILLIAMS 8/22/86

with Report; use Report;
procedure C37010a is

   type R (D : Integer) is record
      null;
   end record;

   type Accr is access R;

   type Arr is array (Positive range <>) of Integer;

   type Acca is access Arr;

   Bump : Integer := 0;

   function F return Integer is
   begin
      Bump := Bump + 1;
      return Bump;
   end F;

begin
   Test
     ("C37010A",
      "CHECK THAT EXPRESSIONS IN CONSTRAINTS OF " &
      "COMPONENT DECLARATIONS ARE EVALUATED IN " &
      "THE ORDER THE COMPONENTS APPEAR");

   declare

      type Rec1 is record
         A1 : R (D => F);
         B1 : String (1 .. F);
         C1 : Accr (F);
         D1 : Acca (1 .. F);
      end record;

      R1 : Rec1;

   begin
      if R1.A1.D /= 1 then
         Failed ("INCORRECT VALUE FOR R1.A1.D");
      end if;

      if R1.B1'Last /= 2 then
         Failed ("INCORRECT VALUE FOR R1.B1'LAST");
      end if;

      begin
         R1.C1 := new R'(D => 3);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R1.C1");
      end;

      begin
         R1.D1 := new Arr (1 .. 4);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R1.D1");
      end;

   end;

   Bump := 0;

   declare

      type Rec2 (I : Integer) is record
         case I is
            when 1 =>
               null;
            when others =>
               A2 : R (D => F);
               B2 : Arr (1 .. F);
               C2 : Accr (F);
               D2 : Acca (1 .. F);
         end case;
      end record;

      R2 : Rec2 (Ident_Int (2));

   begin

      if R2.A2.D /= 1 then
         Failed ("INCORRECT VALUE FOR R2.A2.D");
      end if;

      if R2.B2'Last /= 2 then
         Failed ("INCORRECT VALUE FOR R2.B2'LAST");
      end if;

      begin
         R2.C2 := new R (D => 3);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R2.C2");
      end;

      begin
         R2.D2 := new Arr (1 .. 4);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R2.D2");
      end;

   end;

   Result;
end C37010a;
