-- C34018A.ADA

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
-- CHECK THAT CALLS OF DERIVED SUBPROGRAMS CHECK CONSTRAINTS OF THE PARENT
-- SUBPROGRAM, NOT THE CONSTRAINTS OF THE DERIVED SUBTYPE.

-- JBG 11/15/85
-- JRK 2/12/86 CORRECTED ERROR: RESOLVED AMBIGUOUS CALL G(41) TO
--               TYPE NEW_INT.
-- EDS 7/16/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C34018a is

   package P is
      type Int is range 1 .. 100;
      subtype Int_50 is Int range 1 .. 50;
      subtype Int_51 is Int range 51 .. 100;

      function "+" (L, R : Int) return Int;
      function G (X : Int_50) return Int_51;

      type Str is array (1 .. 10) of Character;
      function F (X : Str) return Str;
   end P;

   use P;

   type New_Str is new P.Str;
   type New_Int is new P.Int range 51 .. 90;

   package body P is

      function "+" (L, R : Int) return Int is
      begin
         return Int (Integer (L) + Integer (R));
      end "+";

      function G (X : Int_50) return Int_51 is
      begin
         return X + 10;
      end G;

      function F (X : Str) return Str is
      begin
         return X;
      end F;

   end P;

begin

   Test
     ("C34018A",
      "CHECK CONSTRAINTS PROCESSED CORRECTLY FOR " &
      "CALLS OF DERIVED SUBPROGRAMS");

   declare

      Y : New_Str := F ("1234567890");    -- UNAMBIGUOUS.

   begin
      if Y /= "1234567890" then
         Failed ("DERIVED F");
      end if;
   end;

   declare

      A : Int     := 51;
      B : New_Int := New_Int (Ident_Int (90));

   begin

      begin
         A := A + 0;
         Failed ("NO EXCEPTION - A + 0 = " & Int'Image (A)); --Use A
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION - 1");
      end;

      begin
         if B + 2 /= 92 then      -- 92 IN INT.
            Failed ("WRONG RESULT - B + 2");
         end if;
      exception
         when Constraint_Error =>
            Failed ("WRONG CONSTRAINT FOR DERIVED ""+""");
         when others =>
            Failed ("UNEXPECTED EXCEPTION - 2");
      end;

      begin
         if B + 14 > 90 then      -- 104 NOT IN INT.
            Failed ("NO EXCEPTION RAISED FOR DERIVED ""+""");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION - 3");
      end;

      begin
         if G (B) > 90 then        -- 90 NOT IN INT_50.
            Failed ("NO EXCEPTION RAISED FOR DERIVED G");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION - 4");
      end;

      begin
         if C34018a.G (41) /= 51 then  -- 41 CONVERTED TO
            --    NEW_INT'BASE.
            -- 41 IN INT_50.
            -- 51 IN INT_51.
            Failed ("WRONG RESULT - G(41)");
         end if;
      exception
         when Constraint_Error =>
            Failed ("C_E RAISED FOR LITERAL ARGUMENT");
         when others =>
            Failed ("UNEXPECTED EXCEPTION - 5");
      end;
   end;

   Result;
end C34018a;
