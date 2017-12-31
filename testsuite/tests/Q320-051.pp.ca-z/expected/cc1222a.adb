-- CC1222A.ADA

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
-- FOR A FORMAL FLOATING POINT TYPE, CHECK THAT THE FOLLOWING BASIC OPERATIONS
-- ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE WITHIN THE GENERIC UNIT:
-- ASSIGNMENT, MEMBERSHIP TESTS, QUALIFICATION, EXPLICIT CONVERSION TO AND FROM
-- OTHER NUMERIC TYPES, AND REAL LITERALS (IMPLICIT CONVERSION FROM UNIVERSAL
-- REAL TO THE FORMAL TYPE), 'FIRST, 'LAST, 'SIZE, 'ADDRESS, 'DIGITS,
-- 'MACHINE_RADIX, 'MACHINE_MANTISSA, 'MACHINE_EMAX, 'MACHINE_EMIN,
-- 'MACHINE_ROUNDS, 'MACHINE_OVERFLOWS.

-- R.WILLIAMS 9/30/86
-- PWN 01/31/95 REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
with System; use System;
procedure Cc1222a is

   type Newflt is new Float;

begin
   Test
     ("CC1222A",
      "FOR A FORMAL FLOATING POINT TYPE, CHECK " &
      "THAT THE BASIC OPERATIONS ARE " &
      "IMPLICITLY DECLARED AND ARE THEREFORE " &
      "AVAILABLE WITHIN THE GENERIC UNIT");

   declare -- (A). CHECKS FOR ASSIGNMENT, MEMBERSHIP TESTS AND
      --      QUALIFICATION.

      generic
         type T is digits <>;
         type T1 is digits <>;
         F : T;
         F1 : T1;
      procedure P (F2 : T; Str : String);

      procedure P (F2 : T; Str : String) is
         subtype St is T range -1.0 .. 1.0;
         F3, F4 : T;

         function Fun (X : T) return Boolean is
         begin
            return Ident_Bool (True);
         end Fun;

         function Fun (X : T1) return Boolean is
         begin
            return Ident_Bool (False);
         end Fun;

      begin
         F3 := F;
         F4 := F2;
         F3 := F4;

         if F3 /= F2 then
            Failed
              ("INCORRECT RESULTS FOR ASSIGNMENT " & "WITH TYPE - " & Str);
         end if;

         if F in St then
            null;
         else
            Failed ("INCORRECT RESULTS FOR ""IN"" WITH " & "TYPE  - " & Str);
         end if;

         if F2 not in St then
            null;
         else
            Failed
              ("INCORRECT RESULTS FOR ""NOT IN"" WITH " & "TYPE  - " & Str);
         end if;

         if T'(F) /= F then
            Failed
              ("INCORRECT RESULTS FOR QUALIFICATION " & "WITH TYPE - " & Str &
               " - 1");
         end if;

         if Fun (T'(1.0)) then
            null;
         else
            Failed
              ("INCORRECT RESULTS FOR QUALIFICATION " & "WITH TYPE - " & Str &
               " - 2");
         end if;

      end P;

      procedure P1 is new P (Float, Float, 0.0, 0.0);
      procedure P2 is new P (Newflt, Newflt, 0.0, 0.0);

   begin
      P1 (2.0, "FLOAT");
      P2 (2.0, "NEWFLT");
   end; -- (A).

   declare -- (B) CHECKS FOR EXPLICIT CONVERSION TO AND FROM OTHER
      --     NUMERIC TYPES, AND IMPLICIT CONVERSION FROM
      --     REAL LITERAL.

      generic
         type T is digits <>;
      procedure P (Str : String);

      procedure P (Str : String) is

         type Fixed is delta 0.1 range -100.0 .. 100.0;
         Fi0  : Fixed := 0.0;
         Fi2  : Fixed := 2.0;
         Fin2 : Fixed := -2.0;

         I0  : Integer := 0;
         I2  : Integer := 2;
         In2 : Integer := -2;

         T0  : T := 0.0;
         T2  : T := 2.0;
         Tn2 : T := -2.0;

         function Ident (X : T) return T is
         begin
            if Equal (3, 3) then
               return X;
            else
               return T'First;
            end if;
         end Ident;

      begin
         if T0 + 1.0 /= 1.0 then
            Failed
              ("INCORRECT RESULTS FOR IMPLICIT " & "CONVERSION WITH TYPE " &
               Str & " - 1");
         end if;

         if T2 + 1.0 /= 3.0 then
            Failed
              ("INCORRECT RESULTS FOR IMPLICIT " & "CONVERSION WITH TYPE " &
               Str & " - 2");
         end if;

         if Tn2 + 1.0 /= -1.0 then
            Failed
              ("INCORRECT RESULTS FOR IMPLICIT " & "CONVERSION WITH TYPE " &
               Str & " - 3");
         end if;

         if T (Fi0) /= T0 then
            Failed
              ("INCORRECT CONVERSION FROM " & "FIXED VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if T (Fi2) /= Ident (T2) then
            Failed
              ("INCORRECT CONVERSION FROM " & "FIXED VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if T (Fin2) /= Tn2 then
            Failed
              ("INCORRECT CONVERSION FROM " & "FIXED VALUE -2.0 WITH TYPE " &
               Str);
         end if;

         if T (I0) /= Ident (T0) then
            Failed
              ("INCORRECT CONVERSION FROM " & "INTEGER VALUE 0 WITH TYPE " &
               Str);
         end if;

         if T (I2) /= T2 then
            Failed
              ("INCORRECT CONVERSION FROM " & "INTEGER VALUE 2 WITH TYPE " &
               Str);
         end if;

         if T (In2) /= Ident (Tn2) then
            Failed
              ("INCORRECT CONVERSION FROM " & "INTEGER VALUE -2 WITH TYPE " &
               Str);
         end if;

         if Fixed (T0) /= Fi0 then
            Failed
              ("INCORRECT CONVERSION TO " & "FIXED VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if Fixed (Ident (T2)) /= Fi2 then
            Failed
              ("INCORRECT CONVERSION TO " & "FIXED VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if Fixed (Tn2) /= Fin2 then
            Failed
              ("INCORRECT CONVERSION TO " & "FIXED VALUE -2.0 WITH TYPE " &
               Str);
         end if;

         if Integer (Ident (T0)) /= I0 then
            Failed
              ("INCORRECT CONVERSION TO " & "INTEGER VALUE 0 WITH TYPE " &
               Str);
         end if;

         if Integer (T2) /= I2 then
            Failed
              ("INCORRECT CONVERSION TO " & "INTEGER VALUE 2 WITH TYPE " &
               Str);
         end if;

         if Integer (Ident (Tn2)) /= In2 then
            Failed
              ("INCORRECT CONVERSION TO " & "INTEGER VALUE -2 WITH TYPE " &
               Str);
         end if;

      end P;

      procedure P1 is new P (Float);
      procedure P2 is new P (Newflt);

   begin
      P1 ("FLOAT");
      P2 ("NEWFLT");
   end; -- (B).

   declare -- (C) CHECKS FOR ATTRIBUTES.

      generic
         type T is digits <>;
         F, L : T;
         D : Integer;
      procedure P (Str : String);

      procedure P (Str : String) is

         F1 : T;
         A  : Address := F'Address;
         S  : Integer := F'Size;

         I  : Integer;
         I1 : Integer := T'Machine_Radix;
         I2 : Integer := T'Machine_Mantissa;
         I3 : Integer := T'Machine_Emax;
         I4 : Integer := T'Machine_Emin;

         B1 : Boolean := T'Machine_Rounds;
         B2 : Boolean := T'Machine_Overflows;

      begin
         if T'Digits /= D then
            Failed ("INCORRECT VALUE FOR " & Str & "'DIGITS");
         end if;

         if T'First /= F then
            Failed ("INCORRECT VALUE FOR " & Str & "'FIRST");
         end if;

         if T'Last /= L then
            Failed ("INCORRECT VALUE FOR " & Str & "'LAST");
         end if;

      end P;

      procedure P1 is new P (Float, Float'First, Float'Last, Float'Digits);
      procedure P2 is new P (Newflt, Newflt'First, Newflt'Last, Newflt'Digits);

   begin
      P1 ("FLOAT");
      P2 ("NEWFLT");
   end; -- (C).

   Result;
end Cc1222a;
