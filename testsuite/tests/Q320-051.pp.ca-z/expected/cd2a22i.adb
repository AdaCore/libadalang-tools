-- CD2A22I.ADA

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
--     CHECK THAT IF A SIZE CLAUSE SPECIFIES THE SMALLEST APPROPRIATE
--     SIZE FOR A SIGNED REPRESENTATION FOR AN ENUMERATION TYPE,
--     THEN THE TYPE CAN BE USED AS AN ACTUAL PARAMETER IN AN
--     INSTANTIATION.

-- HISTORY:
--     JET 08/13/87 CREATED ORIGINAL TEST.
--     PWB 05/11/89 CHANGED EXTENSION FROM '.DEP' TO '.ADA'.
--     JRL 03/27/92 ELIMINATED REDUNDANT TESTING.

with Report; use Report;
procedure Cd2a22i is

   type Basic_Enum is (Zero, One, Two);
   Basic_Size : constant := 3;

   for Basic_Enum'Size use Basic_Size;

begin
   Test
     ("CD2A22I",
      "CHECK THAT IF A SIZE CLAUSE SPECIFIES THE " &
      "SMALLEST APPROPRIATE SIZE FOR A SIGNED " &
      "REPRESENTATION FOR AN ENUMERATION TYPE, THEN " &
      "THE TYPE CAN BE USED AS AN ACTUAL PARAMETER IN " & "AN INSTANTIATION");

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

         if not ((Ident (C1) in C1 .. C2) and (C0 not in Ident (C1) .. C2))
         then
            Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS");
         end if;

         if Check_Type'Last /= Ident (C2) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'LAST");
         end if;

         if Check_Type'Val (0) /= Ident (C0) or
           Check_Type'Val (1) /= Ident (C1) or Check_Type'Val (2) /= Ident (C2)
         then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'VAL");
         end if;

         if Check_Type'Pred (C1) /= Ident (C0) or
           Check_Type'Pred (C2) /= Ident (C1) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'PRED");
         end if;

         if Check_Type'Value ("ZERO") /= Ident (C0) or
           Check_Type'Value ("ONE") /= Ident (C1) or
           Check_Type'Value ("TWO") /= Ident (C2) then
            Failed ("INCORRECT VALUE FOR CHECK_TYPE'VALUE");
         end if;

      end Genproc;

      procedure Newproc is new Genproc (Basic_Enum);

   begin

      Newproc (Zero, One, Two);

   end;

   Result;

end Cd2a22i;
