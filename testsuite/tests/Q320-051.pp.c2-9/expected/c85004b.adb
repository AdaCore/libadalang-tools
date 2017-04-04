-- C85004B.ADA

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
--     CHECK THAT A RENAMED CONSTANT OBJECT, "IN" PARAMETER OF A
--     SUBPROGRAM OR ENTRY, "IN" FORMAL GENERIC, RECORD DISCRIMINANT,
--     LOOP PARAMETER, DEFERRED CONSTANT, OR RENAMED CONSTANT HAS THE
--     CORRECT VALUE.

-- HISTORY:
--     JET 07/25/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85004b is

   type A is array (Positive range <>) of Integer;
   subtype P is Positive range 1 .. 10;

   C1 : constant Integer := 1;
   X1 : Integer renames C1;
   X2 : Integer renames X1;

   type Rec (D : P := 1) is record
      I : A (1 .. D);
   end record;
   type Accrec1 is access Rec;
   type Accrec2 is access Rec (10);

   R1  : Rec;
   R2  : Rec (10);
   Ar1 : Accrec1 := new Rec;
   Ar2 : Accrec2 := new Rec (10);

   X3 : P renames R1.D;
   X4 : P renames R2.D;
   X5 : P renames Ar1.D;
   X6 : P renames Ar2.D;

   C2 : constant A (1 .. 3) := (1, 2, 3);
   X7 : Integer renames C2 (1);

   generic
      K1 : in Integer;
   package Genpkg is
      type K is private;
      K2 : constant K;
   private
      type K is range 1 .. 100;
      K2 : constant K := 5;
   end Genpkg;

   task Fooey is
      entry Ent1 (I : in Integer);
   end Fooey;

   task body Fooey is
   begin
      accept Ent1 (I : in Integer) do
         declare
            Tx1 : Integer renames I;
         begin
            if Tx1 /= Ident_Int (2) then
               Failed ("INCORRECT VALUE");
            end if;
         end;
      end Ent1;
   end Fooey;

   package body Genpkg is
      Kx1 : Integer renames K1;
      Kx2 : K renames K2;
   begin
      if Kx1 /= Ident_Int (4) then
         Failed ("INCORRECT VALUE OF KX1");
      end if;

      if Kx2 /= K (Ident_Int (5)) then
         Failed ("INCORRECT VALUE OF KX2");
      end if;
   end Genpkg;

   procedure Proc (I : in Integer) is
      Px1 : Integer renames I;
   begin
      if Px1 /= Ident_Int (3) then
         Failed ("INCORRECT VALUE OF PX1");
      end if;
   end Proc;

   package Pkg is new Genpkg (4);

begin
   Test
     ("C85004B",
      "CHECK THAT A RENAMED CONSTANT OBJECT, 'IN' " &
      "PARAMETER OF A SUBPROGRAM OR ENTRY, 'IN' FORMAL GENERIC, " &
      "RECORD DISCRIMINANT, LOOP PARAMETER, DEFERRED CONSTANT, " &
      "OR RENAMED CONSTANT HAS THE CORRECT VALUE");

   Fooey.Ent1 (2);

   Proc (3);

   if X1 /= Ident_Int (1) then
      Failed ("INCORRECT VALUE OF X1");
   end if;

   if X2 /= Ident_Int (1) then
      Failed ("INCORRECT VALUE OF X2");
   end if;

   if X3 /= Ident_Int (1) then
      Failed ("INCORRECT VALUE OF X3");
   end if;

   if X4 /= Ident_Int (10) then
      Failed ("INCORRECT VALUE OF X4");
   end if;

   if X5 /= Ident_Int (1) then
      Failed ("INCORRECT VALUE OF X5");
   end if;

   if X6 /= Ident_Int (10) then
      Failed ("INCORRECT VALUE OF X6");
   end if;

   if X7 /= Ident_Int (1) then
      Failed ("INCORRECT VALUE OF X7");
   end if;

   for I in 1 .. Ident_Int (2) loop
      declare
         X8 : Integer renames I;
      begin
         if X8 /= Ident_Int (I) then
            Failed ("INCORRECT VALUE OF X8");
         end if;
      end;
   end loop;

   Result;

end C85004b;
