-- C54A07A.ADA

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
-- CHECK THAT A VARIABLE USED AS A CASE EXPRESSION IS NOT CONSIDERED
--    LOCAL TO THE CASE STATEMENT.  IN PARTICULAR, CHECK THAT THE
--    VARIABLE CAN BE ASSIGNED A NEW VALUE, AND THE ASSIGNMENT TAKES
--    EFFECT IMMEDIATELY (I.E. THE CASE STATEMENT DOES NOT USE A
--    COPY OF THE CASE EXPRESSION).

-- RM 01/21/80

with Report;
procedure C54a07a is

   use Report;

begin

   Test
     ("C54A07A",
      "CHECK THAT A VARIABLE USED AS A CASE" &
      " EXPRESSION IS NOT CONSIDERED LOCAL TO" & " THE CASE STATEMENT");

   declare   -- A
   begin

      B1 :
      declare

         type Variant_Rec (Discr : Boolean := True) is record
            A, B : Integer;
            case Discr is
               when True =>
                  P, Q : Character;
               when False =>
                  X, Y : Integer;
            end case;
         end record;

         V : Variant_Rec := (True, 1, 2, Ident_Char ('P'), Ident_Char ('Q'));

      begin

         if Equal (3, 7) then
            V := (False, 3, 4, 7, 8);
         end if;

         case V.Discr is

            when True =>

               if (V.P /= 'P' or V.Q /= 'Q') then
                  Failed ("WRONG VALUES  -  1");
               end if;

               B1.V := (False, 3, 4, Ident_Int (5), Ident_Int (6));

               if V.Discr then
                  Failed ("WRONG DISCR.");
               end if;

               if (V.X /= 5 or V.Y /= 6) then
                  Failed ("WRONG VALUES  -  2");
               end if;

            when False =>
               Failed ("WRONG BRANCH IN CASE STMT.");

         end case;

      exception

         when others =>
            Failed ("EXCEPTION RAISED");

      end B1;

   exception

      when others =>
         Failed ("EXCEPTION RAISED BY DECLARATIONS");

   end;    -- A

   Result;

end C54a07a;
