-- C86003A.ADA

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
-- CHECK THAT 'STANDARD' IS NOT TREATED AS A RESERVED WORD IN
--    SELECTED COMPONENT NAMES.

-- RM  01/21/80
-- EG 10/29/85 ELIMINATE THE USE OF NUMERIC_ERROR IN TEST. RLB 06/29/01
-- CORRECTED TO ALLOW AGGRESSIVE OPTIMIZATION.

with Report;
procedure C86003a is

   use Report;

begin

   Test
     ("C86003A",
      "CHECK THAT  'STANDARD'  IS NOT TREATED AS A" &
      " RESERVED WORD IN SELECTED COMPONENT NAMES");

   declare    -- A
   begin

      declare

         package Standard is
            Character : Boolean;
            type Integer is (False, True);
            Constraint_Error : exception;
         end Standard;

         type Rec2 is record
            Aa, Bb : Boolean := False;
         end record;

         type Rec1 is record
            Standard : Rec2;
         end record;

         A : Rec1;
         type Asi is access Standard.Integer;
         Vasi : Asi;
         Vi   : Integer range 1 .. 10;   -- THE "REAL" STANDARD
      -- TYPE 'INTEGER'

      begin

         Vasi               := new Standard.Integer'(Standard.False);
         Standard.Character := A.Standard.Bb;

         if Standard.Character then
            Failed ("RES. (VAR.)");
         end if;

         Vi := Ident_Int (11);   -- TO CAUSE THE "REAL"
         -- (PREDEFINED) CONSTRAINT_ERROR EXCEPTION.
         if Vi /= Ident_Int (11) then
            Failed ("WRONG VALUE - V1");
         else
            Failed ("OUT OF RANGE VALUE - V1");
         end if;
      exception

         when Standard.Constraint_Error =>
            Failed ("RES. (EXC.)");

         when Constraint_Error =>
            null;

         when others =>
            Failed ("WRONG EXCEPTION RAISED - A");

      end;

   exception

      when others =>
         Failed ("EXCEPTION RAISED BY DECL. (A)");

   end;    -- A

   declare    -- B

      type Rec is record
         Integer : Boolean := False;
      end record;

      Standard : Rec;

   begin

      if Standard.Integer then
         Failed ("RESERVED  -  REC.,INT.");
      end if;

   end;    -- B

   Result;

end C86003a;
