-- CD2A24I.ADA

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
--     CHECK THAT IF A SIZE CLAUSE (SPECIFYING THE SMALLEST APPROPRIATE
--     SIZE FOR A SIGNED REPRESENTATION) AND AN ENUMERATION
--     REPRESENTATION CLAUSE ARE GIVEN FOR AN ENUMERATION TYPE,
--     THEN THE TYPE CAN BE USED AS AN ACTUAL PARAMETER IN AN
--     INSTANTIATION.

-- HISTORY:
--     JET 08/19/87 CREATED ORIGINAL TEST.
--     PWB 05/11/89 CHANGED EXTENSION FROM '.DEP' TO '.ADA'.
--     WMC 03/27/92 ELIMINATED TEST REDUNDANCIES.

with Report; use Report;
procedure Cd2a24i is

   type Basic_Enum is (Zero, One, Two);
   Basic_Size : constant := 4;

   for Basic_Enum use (Zero => 3, One => 4, Two => 5);

   for Basic_Enum'Size use Basic_Size;

begin
   Test
     ("CD2A24I",
      "CHECK THAT IF A SIZE CLAUSE (SPECIFYING THE " &
      "SMALLEST APPROPRIATE SIZE FOR A SIGNED " &
      "REPRESENTATION) AND AN ENUMERATION " &
      "REPRESENTATION CLAUSE ARE GIVEN FOR AN " &
      "ENUMERATION TYPE, THEN THE TYPE CAN BE USED " &
      "AS AN ACTUAL PARAMETER IN AN INSTANTIATION");

   declare -- TYPE DECLARATION GIVEN WITHIN GENERIC PROCEDURE.

      generic
         type Gparm is (<>);
      procedure Genproc (C0, C1, C2 : Gparm);

      procedure Genproc (C0, C1, C2 : Gparm) is

         subtype Check_Type is Gparm;

         function Ident (Ch : Check_Type) return Check_Type is
         begin
            if Equal (3, 3) then
               return Ch;
            else
               return C1;
            end if;
         end Ident;

      begin -- GENPROC.

         if Check_Type'Size /= Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'SIZE");
         end if;

         if C0'Size < Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR C0'SIZE");
         end if;

         if not
           ((C0 < Ident (C1)) and (Ident (C2) > Ident (C1)) and
            (C1 <= Ident (C1)) and (Ident (C2) = C2))
         then
            Failed ("INCORRECT RESULTS FOR RELATIONAL " & "OPERATORS");
         end if;

         if Check_Type'First /= Ident (C0) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'FIRST");
         end if;

         if Check_Type'Pos (C0) /= Ident_Int (0) or
           Check_Type'Pos (C1) /= Ident_Int (1) or
           Check_Type'Pos (C2) /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'POS");
         end if;

         if Check_Type'Succ (C0) /= Ident (C1) or
           Check_Type'Succ (C1) /= Ident (C2) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'SUCC");
         end if;

         if Check_Type'Image (C0) /= Ident_Str ("ZERO") or
           Check_Type'Image (C1) /= Ident_Str ("ONE") or
           Check_Type'Image (C2) /= Ident_Str ("TWO") then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'IMAGE");
         end if;

      end Genproc;

      procedure Newproc is new Genproc (Basic_Enum);

   begin

      Newproc (Zero, One, Two);

   end;

   Result;

end Cd2a24i;
