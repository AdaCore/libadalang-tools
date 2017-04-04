-- C85007A.ADA

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
-- CHECK THAT THE DISCRIMINANTS OF A RENAMED OUT FORMAL PARAMETER, AS WELL AS
-- THE DISCRIMINANTS OF THE RENAMED SUBCOMPONENTS OF AN OUT FORMAL PARAMETER,
-- MAY BE READ INSIDE THE PROCEDURE.

-- SPS 02/17/84 (SEE C62006A-B.ADA) EG 02/21/84

with Report; use Report;

procedure C85007a is

begin

   Test
     ("C85007A",
      "CHECK THAT THE DISCRIMINANTS OF A RENAMED OUT " &
      "FORMAL PARAMETER CAN BE READ INSIDE THE PROCEDURE");

   declare

      type R1 (D1 : Integer) is record
         null;
      end record;

      type R2 (D2 : Positive) is record
         C : R1 (2);
      end record;

      subtype R1_2 is R1 (2);

      R : R2 (5);

      procedure Proc (Rec : out R2) is

         Rec1 : R2 renames Rec;
         Rec2 : R1_2 renames Rec.C;
         Rec3 : R2 renames Rec1;
         Rec4 : R1_2 renames Rec1.C;
         Rec5 : R1_2 renames Rec4;

      begin

         if Rec1.D2 /= 5 then
            Failed
              ("UNABLE TO CORRECTLY READ DISCRIMINANT OF" &
               " A RENAMED OUT PARAMETER");
         end if;

         if Rec1.C.D1 /= 2 then
            Failed
              ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
               "OF THE SUBCOMPONENT OF A RENAMED OUT " &
               "PARAMETER");
         end if;

         if Rec2.D1 /= 2 then
            Failed
              ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
               "OF A RENAMED SUBCOMPONENT OF AN OUT " &
               "PARAMETER");
         end if;

         if Rec3.D2 /= 5 then
            Failed
              ("UNABLE TO CORRECTLY READ DISCRIMINANT OF" &
               " A RENAME OF A RENAMED OUT PARAMETER");
         end if;

         if Rec3.C.D1 /= 2 then
            Failed
              ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
               "OF THE SUBCOMPONENT OF A RENAME OF A " &
               "RENAMED OUT PARAMETER");
         end if;

         if Rec4.D1 /= 2 then
            Failed
              ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
               "OF A RENAMED SUBCOMPONENT OF A RENAMED" &
               " OUT PARAMETER");
         end if;

         if Rec5.D1 /= 2 then
            Failed
              ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
               "OF A RENAME OF RENAMED SUBCOMPONENT OF" &
               " A RENAMED OUT PARAMETER");
         end if;

      end Proc;

   begin

      Proc (R);

   end;

   Result;

end C85007a;
