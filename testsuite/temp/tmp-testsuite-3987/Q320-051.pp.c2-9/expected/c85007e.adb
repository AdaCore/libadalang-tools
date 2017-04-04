-- C85007E.ADA

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
-- CHECK THAT A RENAMED OUT PARAMETER, OUT PARAMETER COMPONENT, OR
-- OUT PARAMETER SLICE CAN BE ASSIGNED TO.

-- EG  02/22/84

with Report;

procedure C85007e is

   use Report;

begin

   Test
     ("C85007E",
      "CHECK THAT A RENAMED OUT PARAMETER, PARAMETER " &
      "COMPONENT, OR PARAMETER SLICE CAN BE ASSIGNED TO");

   declare

      type At1 is array (1 .. 3) of Integer;
      type Rt (A : Integer) is record
         B : At1;
         C : Integer;
      end record;

      A1, B1 : Integer;
      A2, B2 : At1;
      A3, B3 : Rt (1);

      procedure Proc1 (A : out Integer; B : out At1; C : out Rt) is

         Aa : Integer renames A;
         Bb : At1 renames B;
         Cc : Rt renames C;

      begin

         Aa := -1;
         Bb := (1 .. 3 => -2);
         Cc := (1, (2, 3, 4), 5);

      end Proc1;

      procedure Proc2 (X : out At1; Y : out Integer; Z : out Rt) is

         Xx : At1 renames X;
         Yy : Integer renames Y;
         Zz : Rt renames Z;

      begin

         Proc1 (Yy, Xx, Zz);

      end Proc2;

   begin

      Proc1 (A1, A2, A3);
      if A1 /= Ident_Int (-1) or
        A2 /= (1 .. 3 => Ident_Int (-2)) or
        A3 /= (1, (2, 3, 4), Ident_Int (5))
      then
         Failed ("CASE 1 : ERROR IN ASSIGNMENT");
      end if;

      Proc2 (B2, B1, B3);
      if B1 /= Ident_Int (-1) or
        B2 /= (1 .. 3 => Ident_Int (-2)) or
        B3 /= (1, (2, 3, 4), Ident_Int (5))
      then
         Failed ("CASE 2 : ERROR IN ASSIGNMENT");
      end if;

   end;

   Result;

end C85007e;
