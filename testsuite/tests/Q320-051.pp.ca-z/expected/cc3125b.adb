-- CC3125B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A GENERIC IN PARAMETER HAVING
-- AN ENUMERATION TYPE IF AND ONLY IF THE VALUE OF THE ACTUAL PARAMETER LIES
-- OUTSIDE THE RANGE OF THE FORMAL PARAMETER.

-- TBN  12/15/86

with Report; use Report;
procedure Cc3125b is

   type Color is (Green, Red, White, Blue, Orange, Pink);
   subtype Flag is Color range Red .. Blue;

   function Ident_Col (X : Color) return Color is
   begin
      if Equal (3, 3) then
         return X;
      end if;
      return Green;
   end Ident_Col;

begin
   Test
     ("CC3125B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A " &
      "GENERIC IN PARAMETER HAVING AN ENUMERATION " &
      "TYPE IF AND ONLY IF THE VALUE OF THE ACTUAL " &
      "PARAMETER LIES OUTSIDE THE RANGE OF THE " & "FORMAL PARAMETER");
   declare
      generic
         Gen_Col : in Flag;
      package P is
         Pac_Col : Flag := Gen_Col;
      end P;
   begin
      begin
         declare
            package P1 is new P (Ident_Col (Red));
         begin
            if P1.Pac_Col /= Ident_Col (Red) then
               Failed ("INCORRECT VALUE PASSED - 1");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED - 1");
      end;

      begin
         declare
            package P2 is new P (Ident_Col (Green));
         begin
            Failed ("NO EXCEPTION RAISED - 2");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 2");
      end;

      begin
         declare
            package P3 is new P (Ident_Col (Pink));
         begin
            Failed ("NO EXCEPTION RAISED - 3");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 3");
      end;
   end;
   -------------------------------------------------------------------

   declare
      generic
         type Gen_Typ is (<>);
         Gen_Col : in Gen_Typ;
      package Q is
         Pac_Col : Gen_Typ := Gen_Col;
      end Q;
   begin
      begin
         declare
            package Q1 is new Q (Flag, Ident_Col (Blue));
         begin
            if Q1.Pac_Col /= Ident_Col (Blue) then
               Failed ("INCORRECT VALUE PASSED - 4");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED - 4");
      end;

      begin
         declare
            package Q2 is new Q (Flag, Ident_Col (Green));
         begin
            Failed ("NO EXCEPTION RAISED - 5");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 5");
      end;

      begin
         declare
            package Q3 is new Q (Flag, Ident_Col (Pink));
         begin
            Failed ("NO EXCEPTION RAISED - 6");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED - 6");
      end;
   end;

   Result;
end Cc3125b;
