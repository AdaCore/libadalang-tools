-- CC1223A.ADA

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
--     FOR A FORMAL FIXED POINT TYPE, CHECK THAT THE FOLLOWING BASIC
--     OPERATIONS ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE
--     WITHIN THE GENERIC UNIT: ASSIGNMENT, MEMBERSHIP TESTS,
--     QUALIFICATION, EXPLICIT CONVERSION TO AND FROM OTHER NUMERIC
--     TYPES, AND REAL LITERALS (IMPLICIT CONVERSION FROM UNIVERSAL REAL
--     TO THE FORMAL TYPE), 'FIRST, 'LAST, 'SIZE, 'ADDRESS, 'DELTA, 'FORE,
--     'AFT, 'MACHINE_ROUNDS, 'MACHINE_OVERFLOWS.

-- HISTORY:
--     RJW 09/30/86  CREATED ORIGINAL TEST.
--     JLH 09/25/87  REFORMATTED HEADER.
--     RJW 08/21/89  MODIFIED CHECKS FOR 'MANTISSA AND 'AFT.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure Cc1223a is

   type Fixed is delta 0.1 range -100.0 .. 100.0;

begin
   Test
     ("CC1223A",
      "FOR A FORMAL FIXED POINT TYPE, CHECK " &
      "THAT THE BASIC OPERATIONS ARE " &
      "IMPLICITLY DECLARED AND ARE THEREFORE " &
      "AVAILABLE WITHIN THE GENERIC UNIT");

   declare -- (A). CHECKS FOR ASSIGNMENT, MEMBERSHIP TESTS AND
      --      QUALIFICATION.

      generic
         type T is delta <>;
         type T1 is delta <>;
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

      procedure P1 is new P (Fixed, Fixed, 0.0, 0.0);
      procedure P2 is new P (Duration, Duration, 0.0, 0.0);

   begin
      P1 (2.0, "FIXED");
      P2 (2.0, "DURATION");
   end; -- (A).

   declare -- (B) CHECKS FOR EXPLICIT CONVERSION TO AND FROM OTHER
      --     NUMERIC TYPES, AND IMPLICIT CONVERSION FROM
      --     REAL LITERAL.

      generic
         type T is delta <>;
      procedure P (Str : String);

      procedure P (Str : String) is

         Fl0  : Float := 0.0;
         Fl2  : Float := 2.0;
         Fln2 : Float := -2.0;

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

         if T (Fl0) /= T0 then
            Failed
              ("INCORRECT CONVERSION FROM " & "FLOAT VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if T (Fl2) /= Ident (T2) then
            Failed
              ("INCORRECT CONVERSION FROM " & "FLOAT VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if T (Fln2) /= Tn2 then
            Failed
              ("INCORRECT CONVERSION FROM " & "FLOAT VALUE -2.0 WITH TYPE " &
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

         if Float (T0) /= Fl0 then
            Failed
              ("INCORRECT CONVERSION TO " & "FLOAT VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if Float (Ident (T2)) /= Fl2 then
            Failed
              ("INCORRECT CONVERSION TO " & "FLOAT VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if Float (Tn2) /= Fln2 then
            Failed
              ("INCORRECT CONVERSION TO " & "FLOAT VALUE -2.0 WITH TYPE " &
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

      procedure P1 is new P (Fixed);
      procedure P2 is new P (Duration);

   begin
      P1 ("FIXED");
      P2 ("DURATION");
   end; -- (B).

   declare -- (C) CHECKS FOR ATTRIBUTES.

      generic
         type T is delta <>;
         F, L, D : T;
      procedure P (Str : String);

      procedure P (Str : String) is

         F1 : T;
         A  : Address := F'Address;
         S  : Integer := F'Size;

         I : Integer;

         B1 : Boolean := T'Machine_Rounds;
         B2 : Boolean := T'Machine_Overflows;

      begin
         if T'Delta /= D then
            Failed ("INCORRECT VALUE FOR " & Str & "'DELTA");
         end if;

         if T'First /= F then
            Failed ("INCORRECT VALUE FOR " & Str & "'FIRST");
         end if;

         if T'Last /= L then
            Failed ("INCORRECT VALUE FOR " & Str & "'LAST");
         end if;

         if T'Fore < 2 then
            Failed ("INCORRECT VALUE FOR " & Str & "'FORE");
         end if;

         if T'Aft <= 0 then
            Failed ("INCORRECT VALUE FOR " & Str & "'AFT");
         end if;

      end P;

      procedure P1 is new P (Fixed, Fixed'First, Fixed'Last, Fixed'Delta);
      procedure P2 is new P (Duration, Duration'First, Duration'Last,
         Duration'Delta);

   begin
      P1 ("FIXED");
      P2 ("DURATION");
   end; -- (C).

   Result;
end Cc1223a;
