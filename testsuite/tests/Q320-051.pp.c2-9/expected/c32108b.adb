-- C32108B.ADA

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
-- CHECK THAT IF A DEFAULT EXPRESSION IS EVALUATED FOR A COMPONENT, NO DEFAULT
-- EXPRESSIONS ARE EVALUATED FOR ANY SUBCOMPONENTS.

-- TBN 3/21/86

with Report; use Report;
procedure C32108b is

   function Default_Check (Number : Integer) return Integer is
   begin
      if Number /= 0 then
         Failed
           ("SUBCOMPONENT DEFAULT EXPRESSIONS ARE " &
            "EVALUATED -" &
            Integer'Image (Number));
      end if;
      return (1);
   end Default_Check;

begin
   Test
     ("C32108B",
      "CHECK THAT IF A DEFAULT EXPRESSION IS " &
      "EVALUATED FOR A COMPONENT, NO DEFAULT " &
      "EXPRESSIONS ARE EVALUATED FOR ANY " &
      "SUBCOMPONENTS");

   declare     -- (A)

      type Rec_Typ1 is record
         Age : Integer := Default_Check (1);
      end record;

      type Rec_Typ2 (D : Integer := Default_Check (2)) is record
         null;
      end record;

      type Rec_Typ3 (D : Integer := Default_Check (3)) is record
         A : Integer := Default_Check (4);
      end record;

      type Rec_Typ4 is record
         One   : Rec_Typ1 := (Age => Default_Check (0));
         Two   : Rec_Typ2 (Default_Check (0));
         Three : Rec_Typ3 := (D => Default_Check (0), A => Default_Check (0));
      end record;

      Rec4 : Rec_Typ4;

   begin     -- (A)
      null;
   end;      -- (A)

   Result;
end C32108b;
