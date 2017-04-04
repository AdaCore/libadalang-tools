-- C4A012B.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR
--     A UNIVERSAL_REAL EXPRESSION IF DIVISION BY ZERO IS ATTEMPTED.

--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR
--     0.0 ** (-1) (OR ANY OTHER NEGATIVE EXPONENT VALUE).

-- HISTORY:
--     RJW 09/04/86  CREATED ORIGINAL TEST.
--     CJJ 09/04/87  ADDED PASS MESSAGE FOR RAISING NUMERIC_ERROR;
--                   MODIFIED CODE TO PREVENT COMPILER OPTIMIZING
--                   OUT THE TEST.
--     JET 12/31/87  ADDED MORE CODE TO PREVENT OPTIMIZATION.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY
--     JRL 02/29/96  Added code to check for value of Machine_Overflows; if
--                   False, test is inapplicable.

with Report; use Report;

procedure C4a012b is

   F : Float;

   I3 : Integer := -3;

   subtype Sint is Integer range -10 .. 10;
   Si5 : constant Sint := -5;

   function Ident (X : Float) return Float is
   begin
      if Equal (3, 3) then
         return X;
      else
         return 1.0;
      end if;
   end Ident;

begin

   Test
     ("C4A012B",
      "CHECK THAT CONSTRAINT_ERROR " &
      "IS RAISED FOR " &
      "0.0 ** (-1) (OR ANY OTHER NEGATIVE EXPONENT " &
      "VALUE)");

   if Float'Machine_Overflows = False then
      Report.Not_Applicable ("Float'Machine_Overflows = False");
   else

      begin
         F := Ident (0.0)**(-1);
         Failed
           ("THE EXPRESSION '0.0 ** (-1)' DID NOT RAISE " & "AN EXCEPTION");
         if Equal (Integer (F), Integer (F)) then
            Comment ("SHOULDN'T BE HERE!");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - 1");
         when others =>
            Failed
              ("THE EXPRESSION '0.0 ** (-1)' RAISED THE " & "WRONG EXCEPTION");
      end;

      begin
         F := 0.0**(Ident_Int (-1));
         Failed
           ("THE EXPRESSION '0.0 ** (IDENT_INT (-1))' DID " &
            "NOT RAISE AN EXCEPTION");
         if Equal (Integer (F), Integer (F)) then
            Comment ("SHOULDN'T BE HERE!");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - 2");
         when others =>
            Failed
              ("THE EXPRESSION '0.0 ** (IDENT_INT (-1))' " &
               "RAISED THE WRONG EXCEPTION");
      end;

      begin
         F := 0.0**(Integer'Pos (Ident_Int (-1)));
         Failed
           ("THE EXPRESSION '0.0 ** " &
            "(INTEGER'POS (IDENT_INT (-1)))' DID " &
            "NOT RAISE AN EXCEPTION");
         if Equal (Integer (F), Integer (F)) then
            Comment ("SHOULDN'T BE HERE!");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - 3");
         when others =>
            Failed
              ("THE EXPRESSION '0.0 ** " &
               "(INTEGER'POS (IDENT_INT (-1)))' RAISED " &
               "THE WRONG EXCEPTION");
      end;

      begin
         F := Ident (0.0)**I3;
         Failed ("THE EXPRESSION '0.0 ** I3' DID NOT RAISE " & "AN EXCEPTION");
         if Equal (Integer (F), Integer (F)) then
            Comment ("SHOULDN'T BE HERE!");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - 4");
         when others =>
            Failed
              ("THE EXPRESSION '0.0 ** I3' RAISED THE " & "WRONG EXCEPTION");
      end;

      begin
         F := 0.0**(Ident_Int (I3));
         Failed
           ("THE EXPRESSION '0.0 ** (IDENT_INT (I3))' DID " &
            "NOT RAISE AN EXCEPTION");
         if Equal (Integer (F), Integer (F)) then
            Comment ("SHOULDN'T BE HERE!");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - 5");
         when others =>
            Failed
              ("THE EXPRESSION '0.0 ** (IDENT_INT (I3))' " &
               "RAISED THE WRONG EXCEPTION");
      end;

      begin
         F := Ident (0.0)**Si5;
         Failed
           ("THE EXPRESSION '0.0 ** SI5' DID NOT RAISE " & "AN EXCEPTION");
         if Equal (Integer (F), Integer (F)) then
            Comment ("SHOULDN'T BE HERE!");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - 6");
         when others =>
            Failed
              ("THE EXPRESSION '0.0 ** SI5' RAISED THE " & "WRONG EXCEPTION");
      end;

      begin
         F := 0.0**(Ident_Int (Si5));
         Failed
           ("THE EXPRESSION '0.0 ** (IDENT_INT (SI5))' DID " &
            "NOT RAISE AN EXCEPTION");
         if Equal (Integer (F), Integer (F)) then
            Comment ("SHOULDN'T BE HERE!");
         end if;
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED - 7");
         when others =>
            Failed
              ("THE EXPRESSION '0.0 ** (IDENT_INT (SI5))' " &
               "RAISED THE WRONG EXCEPTION");
      end;

   end if;

   Result;

end C4a012b;
