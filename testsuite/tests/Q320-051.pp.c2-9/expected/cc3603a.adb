-- CC3603A.ADA

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
--     CHECK THAT ENUMERATION LITERALS (BOTH IDENTIFIERS AND CHARACTER
--     LITERALS) MAY BE PASSED AS ACTUALS CORRESPONDING TO GENERIC
--     FORMAL SUBPROGRAMS.

-- HISTORY:
--     RJW 06/11/86  CREATED ORIGINAL TEST.
--     VCL 08/18/87  CHANGED THE SECOND ACTUAL GENERIC PARAMETER IN THE
--                   INSTANTIATION OF PROCEDURE NP3 TO
--                   'IDENT_CHAR('X')'.

with Report; use Report;

procedure Cc3603a is

begin
   Test
     ("CC3603A",
      "CHECK THAT ENUMERATION LITERALS (BOTH " &
      "IDENTIFIERS AND CHARACTER LITERALS) MAY " &
      "BE PASSED AS ACTUALS CORRESPONDING TO " &
      "GENERIC FORMAL SUBPROGRAMS");

   declare

      type Enum1 is ('A', 'B');
      type Enum2 is (C, D);

      generic
         type E is (<>);
         E1 : E;
         with function F return E;
      procedure P;

      procedure P is
      begin
         if F /= E1 then
            Failed
              ("WRONG VALUE FOR " & E'Image (E1) & " AS ACTUAL PARAMETER");
         end if;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED INSIDE OF P WITH " &
               E'Image (E1) &
               " AS ACTUAL PARAMETER");
      end P;

      procedure Np1 is new P (Enum1, 'A', 'A');
      procedure Np2 is new P (Enum2, D, D);
      procedure Np3 is new P (Character, Ident_Char ('X'), 'X');
   begin
      begin
         Np1;
      exception
         when others =>
            Failed ("EXCEPTION RAISED WHEN NP1 CALLED");
      end;

      begin
         Np2;
      exception
         when others =>
            Failed ("EXCEPTION RAISED WHEN NP2 CALLED");
      end;

      begin
         Np3;
      exception
         when others =>
            Failed ("EXCEPTION RAISED WHEN NP3 CALLED");
      end;
   end;
   Result;

end Cc3603a;
