-- CD2A32I.ADA

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
--     SIGNED SIZE IS GIVEN FOR AN INTEGER TYPE, THE TYPE CAN
--     BE PASSED AS AN ACTUAL PARAMETER TO GENERIC PROCEDURES.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, AND CHANGED OPERATOR ON
--                   'SIZE CHECKS.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.

with Report; use Report;
procedure Cd2a32i is

   type Basic_Int is range -63 .. 63;
   Basic_Size : constant := 7;

   for Basic_Int'Size use Basic_Size;

begin

   Test
     ("CD2A32I",
      "CHECK THAT WHEN A SIZE SPECIFICATION " &
      "OF THE SMALLEST APPROPRIATE SIGNED SIZE " &
      "IS GIVEN FOR AN INTEGER TYPE, " & "THE TYPE " &
      "CAN BE PASSED AS AN ACTUAL PARAMETER TO " & "GENERIC PROCEDURES");

   declare -- TYPE DECLARATION WITHIN GENERIC PROCEDURE.

      generic
         type Gparm is range <>;
      procedure Genproc;

      procedure Genproc is

         subtype Int is Gparm;

         I1 : Int := -63;
         I2 : Int := 0;
         I3 : Int := 63;

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

         if I1'Size < Ident_Int (Basic_Size) then
            Failed ("INCORRECT VALUE FOR I1'SIZE");
         end if;

         for I in Ident (I1) .. Ident (I3) loop
            if not (I in I1 .. I3) or (I not in Ident (-63) .. Ident (63)) then
               Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS");
            end if;
         end loop;

         if not ((+I1 = I1) and (-I3 = I1) and (abs I1 = I3)) then
            Failed ("INCORRECT RESULTS FOR UNARY ARITHMETIC " & "OPERATORS");
         end if;

         if Int'Last /= Ident (63) then
            Failed ("INCORRECT VALUE FOR INT'LAST");
         end if;

         if Int'Val (-63) /= Ident (I1) or Int'Val (0) /= Ident (I2) or
           Int'Val (63) /= Ident (I3) then
            Failed ("INCORRECT VALUE FOR INT'VAL");
         end if;

         if Int'Pred (I2) /= Ident (-1) or Int'Pred (I3) /= Ident (62) then
            Failed ("INCORRECT VALUE FOR INT'PRED");
         end if;

         if Int'Value ("-63") /= Ident (I1) or
           Int'Value (" 0") /= Ident (I2) or Int'Value (" 63") /= Ident (I3)
         then
            Failed ("INCORRECT VALUE FOR INT'VALUE");
         end if;

      end Genproc;

      procedure Newproc is new Genproc (Basic_Int);

   begin

      Newproc;

   end;

   Result;

end Cd2a32i;
