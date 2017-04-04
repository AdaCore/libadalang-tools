-- CC3125C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A GENERIC IN PARAMETER HAVING A
-- FLOATING POINT TYPE IF AND ONLY IF THE VALUE OF THE ACTUAL PARAMETER LIES
-- OUTSIDE THE RANGE OF THE FORMAL PARAMETER.

-- TBN  12/15/86

with Report; use Report;
procedure Cc3125c is

   type Flt is digits 5 range -10.0 .. 10.0;
   subtype Flo is Flt range -5.0 .. 5.0;

   function Ident_Flt (X : Flt) return Flt is
   begin
      if Equal (3, 3) then
         return X;
      end if;
      return 0.0;
   end Ident_Flt;

begin
   Test
     ("CC3125C",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A " &
      "GENERIC IN PARAMETER HAVING A FLOATING POINT " &
      "TYPE IF AND ONLY IF THE VALUE OF THE ACTUAL " &
      "PARAMETER LIES OUTSIDE THE RANGE OF THE " &
      "FORMAL PARAMETER");
   declare
      generic
         Gen_Flo : in Flo;
      package P is
         Pac_Flo : Flt := Gen_Flo;
      end P;
   begin
      begin
         declare
            package P1 is new P (Ident_Flt (-5.0));
         begin
            if P1.Pac_Flo /= Ident_Flt (-5.0) then
               Failed ("INCORRECT VALUE PASSED - 1");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED - 1");
      end;

      begin
         declare
            package P2 is new P (Ident_Flt (-5.1));
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
            package P3 is new P (Ident_Flt (5.1));
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
         type Gen_Typ is digits <>;
         Gen_Flo : in Gen_Typ;
      package Q is
         Pac_Flo : Gen_Typ := Gen_Flo;
      end Q;
   begin
      begin
         declare
            package Q1 is new Q (Flo, Ident_Flt (5.0));
         begin
            if Q1.Pac_Flo /= Ident_Flt (5.0) then
               Failed ("INCORRECT VALUE PASSED - 4");
            end if;
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED - 4");
      end;

      begin
         declare
            package Q2 is new Q (Flo, Ident_Flt (-5.1));
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
            package Q3 is new Q (Flo, Ident_Flt (5.1));
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
end Cc3125c;
