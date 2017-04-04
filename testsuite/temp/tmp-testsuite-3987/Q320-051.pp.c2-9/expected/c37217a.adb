-- C37217A.ADA

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
--     CHECK WHETHER THE OPTIONAL COMPATIBILITY CHECK IS
--     PERFORMED WHEN A DISCRIMINANT CONSTRAINT IS GIVEN FOR AN ACCESS
--     TYPE - AFTER THE TYPE'S FULL DECLARATION.

-- HISTORY:
--     DHH 02/05/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C37217a is

   subtype Sm is Integer range 1 .. 10;

begin  --C37217A BODY
   Test
     ("C37217A",
      "CHECK WHETHER THE OPTIONAL COMPATIBILITY " &
      "CHECK IS PERFORMED WHEN A DISCRIMINANT " &
      "CONSTRAINT IS GIVEN FOR AN ACCESS TYPE " &
      "- AFTER THE TYPE'S FULL DECLARATION");

   -- CHECK FULL DECLARATION
   -- LOWER LIMIT
   begin
      declare

         type Sm_Rec (D : Sm) is record
            null;
         end record;

         type Rec (D1 : Integer) is record
            Int : Sm_Rec (D1);
         end record;

         type Ptr is access Rec;

         Y : Ptr (Ident_Int (0));           -- OPTIONAL EXCEPTION.
      begin
         Comment ("OPTIONAL COMBATIBILITY CHECK NOT PERFORMED " & "- LOWER");
         Y := new Rec (Ident_Int (0));      -- MANDATORY EXCEPTION.
         Failed ("CONSTRAINT ERROR NOT RAISED");

         if Ident_Int (Y.Int.D) /= Ident_Int (-1) then
            Comment ("IRRELEVANT");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION RAISED IN " &
               "VARIABLE ALLOCATION - LOWER");
      end;
   exception
      when Constraint_Error =>
         Comment ("OPTIONAL CONSTRAINT ERROR RAISED - LOWER");
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED IN " &
            "VARIABLE DECLARATION - LOWER");
   end;
---------------------------------------------------------------------
   -- CHECK FULL DECLARATION
   -- UPPER LIMIT
   begin
      declare
         type Sm_Arr is array (Sm range <>) of Integer;

         type Rec (D1 : Integer) is record
            Int : Sm_Arr (1 .. D1);
         end record;

         type Ptr is access Rec;

         Y : Ptr (Ident_Int (11));           -- OPTIONAL EXCEPTION.
      begin
         Comment ("OPTIONAL COMBATIBILITY CHECK NOT PERFORMED " & "- UPPER");
         Y := new Rec'(Ident_Int (11),     -- MANDATORY EXCEPTION.
         Int => (others => Ident_Int (0)));
         Failed ("CONSTRAINT ERROR NOT RAISED");

         if Ident_Int (Y.Int (Ident_Int (1))) /= 11 then
            Comment ("IRRELEVANT");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION RAISED IN " &
               "VARIABLE ALLOCATION - UPPER");
      end;
   exception
      when Constraint_Error =>
         Comment ("OPTIONAL COMPATIBILITY CHECK PERFORMED " & "- UPPER");
      when others =>
         Failed
           ("UNEXPECTED EXCEPTION RAISED IN " &
            "VARIABLE DECLARATION - UPPER");
   end;

   Result;

end C37217a;  -- BODY
