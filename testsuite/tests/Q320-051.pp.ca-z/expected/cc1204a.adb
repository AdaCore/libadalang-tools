-- CC1204A.ADA

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
-- CHECK THAT GENERIC FORMAL TYPES MAY HAVE A DISCRIMINANT PART, WHICH MAY BE
-- OF A GENERIC FORMAL TYPE.

-- DAT 8/14/81
-- SPS 5/12/82

with Report; use Report;

procedure Cc1204a is
begin
   Test ("CC1204A", "DISCRIMINANT PARTS FOR GENERIC FORMAL TYPES");

   declare
      generic
         type T is (<>);
         type I is range <>;
         type R1 (C : Boolean) is private;
         type R2 (C : T) is private;
         type R3 (C : I) is limited private;
         P1 : in R1;
         P2 : in R2;
         V1 : in out R1;
         V2 : in out R2;
         V3 : in out R3;
      procedure Proc;

      type Dd is new Integer range 1 .. 10;
      type Arr is array (Dd range <>) of Character;
      type Recd (C : Dd := Dd (Ident_Int (1))) is record
         C1 : Arr (1 .. C);
      end record;

      X1 : Recd;
      X2 : Recd := (1, "Y");

      type Recb (C : Boolean) is record
         V : Integer := 6;
      end record;
      Rb  : Recb (Ident_Bool (True));
      Rb1 : Recb (Ident_Bool (True));

      procedure Proc is
      begin
         if P1.C /= True or P2.C /= T'First or V1.C /= True or
           V2.C /= T'First or V3.C /= I'First then
            Failed ("WRONG GENERIC PARAMETER VALUE");
         end if;

         V1 := P1;
         V2 := P2;

         if V1 /= P1 or V2 /= P2 then
            Failed ("BAD ASSIGNMENT TO GENERIC PARAMETERS");
         end if;
      end Proc;

   begin
      Rb1.V := Ident_Int (1);
      X1.C1 := "X";

      declare

         procedure Pr is new Proc (T => Dd, I => Dd, R1 => Recb, R2 => Recd,
            R3 => Recd, P1 => Rb1, P2 => X1, V1 => Rb, V2 => X2, V3 => X2);
      begin
         Pr;
         if Rb /= (True, 1) or X2.C1 /= "X" then
            Failed ("PR NOT CALLED CORRECTLY");
         end if;
      end;
   end;

   Result;
end Cc1204a;
