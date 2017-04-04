-- C37010B.ADA

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
-- CHECK THAT EXPRESSIONS IN AN INDEX CONSTRAINT OR DISCRIMINANT CONSTRAINT ARE
-- EVALUATED WHEN THE COMPONENT DECLARATION IS ELABORATED EVEN IF SOME BOUNDS
-- OR DISCRIMINANTS ARE GIVEN BY A DISCRIMINANT OF AN ENCLOSING RECORD TYPE.

-- R.WILLIAMS 8/22/86

with Report; use Report;
procedure C37010b is

   Init : Integer := Ident_Int (5);

   type R (D1, D2 : Integer) is record
      null;
   end record;

   type Accr is access R;

   type Arr is array (Integer range <>) of Integer;

   type Acca is access Arr;

   function Reset (N : Integer) return Integer is
   begin
      Init := Ident_Int (N);
      return N;
   end Reset;

begin
   Test
     ("C37010B",
      "CHECK THAT EXPRESSIONS IN AN INDEX " &
      "CONSTRAINT OR DISCRIMINANT CONSTRAINT " &
      "ARE EVALUATED WHEN THE COMPONENT " &
      "DECLARATION IS ELABORATED EVEN IF SOME " &
      "BOUNDS OR DISCRIMINANTS ARE GIVEN BY " &
      "A DISCRIMINANT OF AN ENCLOSING RECORD TYPE");

   declare

      type Rec1 (D : Integer) is record
         W1 : R (D1 => Init, D2 => D);
         X1 : Arr (Init .. D);
         Y1 : Accr (D, Init);
         Z1 : Acca (D .. Init);
      end record;

      Int1 : Integer := Reset (10);

      R1 : Rec1 (D => 4);

   begin
      if R1.W1.D1 /= 5 then
         Failed ("INCORRECT VALUE FOR R1.W1.D1");
      end if;

      if R1.W1.D2 /= 4 then
         Failed ("INCORRECT VALUE FOR R1.W1.D2");
      end if;

      if R1.X1'First /= 5 then
         Failed ("INCORRECT VALUE FOR R1.X1'FIRST");
      end if;

      if R1.X1'Last /= 4 then
         Failed ("INCORRECT VALUE FOR R1.X1'LAST");
      end if;

      begin
         R1.Y1 := new R (4, 5);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R1.Y1");
      end;

      begin
         R1.Z1 := new Arr (4 .. 5);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R1.Z1");
      end;

   end;

   declare

      type Rec2 (D : Integer) is record
         case D is
            when 1 =>
               null;
            when 2 =>
               null;
            when others =>
               W2 : R (D1 => D, D2 => Init);
               X2 : Arr (D .. Init);
               Y2 : Accr (Init, D);
               Z2 : Acca (D .. Init);
         end case;
      end record;

      Int2 : Integer := Reset (20);

      R2 : Rec2 (D => 6);

   begin
      if R2.W2.D1 /= 6 then
         Failed ("INCORRECT VALUE FOR R2.W2.D1");
      end if;

      if R2.W2.D2 /= 10 then
         Failed ("INCORRECT VALUE FOR R2.W2.D2");
      end if;

      if R2.X2'First /= 6 then
         Failed ("INCORRECT VALUE FOR R2.X2'FIRST");
      end if;

      if R2.X2'Last /= 10 then
         Failed ("INCORRECT VALUE FOR R2.X2'LAST");
      end if;

      begin
         R2.Y2 := new R (10, 6);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R2.Y2");
      end;

      begin
         R2.Z2 := new Arr (6 .. 10);
      exception
         when others =>
            Failed ("INCORRECT VALUE FOR R2.Z2");
      end;

   end;

   Result;
end C37010b;
