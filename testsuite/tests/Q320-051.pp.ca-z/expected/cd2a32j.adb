-- CD2A32J.ADA

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
--     CHECK THAT WHEN A SIZE SPECIFICATION OF THE SMALLEST APPROPRIATE
--     UNSIGNED SIZE IS GIVEN FOR AN INTEGER TYPE, THE TYPE CAN BE
--     PASSED AS AN ACTUAL PARAMETER TO GENERIC PROCEDURES.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, AND CHANGED OPERATOR ON
--                   'SIZE CHECKS.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.

with Report; use Report;

procedure Cd2a32j is

   type Basic_Int is range 0 .. 126;
   Basic_Size : constant := 7;

   for Basic_Int'Size use Basic_Size;

begin

   Test
     ("CD2A32J",
      "CHECK THAT WHEN A SIZE SPECIFICATION " &
      "OF THE SMALLEST APPROPRIATE UNSIGNED SIZE " &
      "IS GIVEN FOR AN INTEGER TYPE, THE TYPE " &
      "CAN BE PASSED AS AN ACTUAL PARAMETER TO " &
      "GENERIC PROCEDURES");

   declare -- TYPE DECLARATION WITHIN GENERIC PROCEDURE.

      generic
         type Gparm is range <>;
      procedure Genproc;

      procedure Genproc is

         subtype Int is Gparm;

         I0 : Int := 0;
         I1 : Int := 63;
         I2 : Int := 126;

         function Ident (I : Int) return Int is
         begin
            if Equal (0, 0) then
               return I;
            else
               return 0;
            end if;
         end Ident;

      begin -- GENPROC.

         if Int'Size /= Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR INT'SIZE");
         end if;

         if I0'Size < Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR I0'SIZE");
         end if;

         if not
           ((I0 < Ident (1)) and
            (Ident (I2) > Ident (I1)) and
            (I1 <= Ident (63)) and
            (Ident (126) = I2))
         then
            Failed ("INCORRECT RESULTS FOR RELATIONAL " & "OPERATORS");
         end if;

         if not
           (((I0 + I2) = I2) and
            ((I2 - I1) = I1) and
            ((I1 * Ident (2)) = I2) and
            ((I2 / I1) = Ident (2)) and
            ((I1**1) = Ident (63)) and
            ((I2 rem 10) = Ident (6)) and
            ((I1 mod 10) = Ident (3)))
         then
            Failed ("INCORRECT RESULTS FOR BINARY ARITHMETIC " & "OPERATORS");
         end if;

         if Int'Pos (I0) /= Ident_Int (0) or
           Int'Pos (I1) /= Ident_Int (63) or
           Int'Pos (I2) /= Ident_Int (126)
         then
            Failed ("INCORRECT VALUE FOR INT'POS");
         end if;

         if Int'Succ (I0) /= Ident (1) or Int'Succ (I1) /= Ident (64) then
            Failed ("INCORRECT VALUE FOR INT'SUCC");
         end if;

         if Int'Image (I0) /= Ident_Str (" 0") or
           Int'Image (I1) /= Ident_Str (" 63") or
           Int'Image (I2) /= Ident_Str (" 126")
         then
            Failed ("INCORRECT VALUE FOR INT'IMAGE");
         end if;

      end Genproc;

      procedure Newproc is new Genproc (Basic_Int);

   begin

      Newproc;

   end;

   Result;

end Cd2a32j;
