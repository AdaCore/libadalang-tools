-- C32108A.ADA

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
-- CHECK THAT DEFAULT EXPRESSIONS ARE NOT EVALUATED, IF INITIALIZATION
-- EXPRESSIONS ARE GIVEN FOR THE OBJECT DECLARATIONS.

-- TBN 3/20/86

with Report; use Report;
procedure C32108a is

   function Default_Check (Number : Integer) return Integer is
   begin
      if Number /= 0 then
         Failed
           ("DEFAULT EXPRESSIONS ARE EVALUATED -" & Integer'Image (Number));
      end if;
      return (1);
   end Default_Check;

begin
   Test
     ("C32108A",
      "CHECK THAT DEFAULT EXPRESSIONS ARE NOT " &
      "EVALUATED, IF INITIALIZATION EXPRESSIONS ARE " &
      "GIVEN FOR THE OBJECT DECLARATIONS");

   declare     -- (A)

      type Rec_Typ1 is record
         Age : Integer := Default_Check (1);
      end record;

      Rec1 : Rec_Typ1 := (Age => Default_Check (0));

      type Rec_Typ2 (D : Integer := Default_Check (2)) is record
         null;
      end record;

      Rec2 : Rec_Typ2 (Default_Check (0));

      type Rec_Typ3 (D : Integer := Default_Check (3)) is record
         A : Integer := Default_Check (4);
      end record;

      Rec3 : Rec_Typ3 := (D => Default_Check (0), A => Default_Check (0));

   begin     -- (A)
      null;
   end;      -- (A)

   Result;
end C32108a;
