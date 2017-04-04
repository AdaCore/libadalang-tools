-- C37002A.ADA

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
-- CHECK THAT INDEX CONSTRAINTS WITH NON-STATIC EXPRESSIONS CAN BE USED TO
-- CONSTRAIN RECORD COMPONENTS HAVING AN ARRAY TYPE.

-- RJW 2/28/86

with Report; use Report;

procedure C37002a is

begin
   Test
     ("C37002A",
      "CHECK THAT INDEX CONSTRAINTS WITH " &
      "NON-STATIC EXPRESSIONS CAN BE USED TO " &
      "CONSTRAIN RECORD COMPONENTS HAVING AN " &
      "ARRAY TYPE");

   declare
      X : Integer := Ident_Int (5);
      subtype S is Integer range 1 .. X;
      type Ar1 is array (S) of Integer;

      subtype T is Integer range X .. 10;
      type Ar2 is array (T) of Integer;
      type U is array (Integer range <>) of Integer;
      subtype V is Integer range 1 .. 10;

      type R is record
         A : String (1 .. X);
         B : String (X .. 10);
         C : Ar1;
         D : Ar2;
         E : String (S);
         F : U (T);
         G : U (V range 1 .. X);
         H : String (Positive range X .. 10);
         I : U (Ar1'Range);
         J : String (Ar2'Range);
      end record;
      Rr : R;

   begin
      if Rr.A'Last /= 5 or
        Rr.B'First /= 5 or
        Rr.C'Last /= 5 or
        Rr.D'First /= 5 or
        Rr.E'Last /= 5 or
        Rr.F'First /= 5 or
        Rr.G'Last /= 5 or
        Rr.H'First /= 5 or
        Rr.I'Last /= 5 or
        Rr.J'First /= 5
      then

         Failed ("WRONG VALUE FOR NON-STATIC BOUND");

      end if;

   end;

   Result;
end C37002a;
