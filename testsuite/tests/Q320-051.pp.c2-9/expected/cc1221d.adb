-- CC1221D.ADA

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
--     FOR A FORMAL INTEGER TYPE, CHECK THAT THE FOLLOWING BASIC
--     OPERATIONS ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE
--     WITHIN THE GENERIC UNIT:  EXPLICIT CONVERSION TO AND FROM REAL
--     TYPES AND IMPLICIT CONVERSION FROM INTEGER LITERALS.

-- HISTORY:
--     BCB 11/12/87  CREATED ORIGINAL TEST FROM SPLIT OF CC1221A.ADA

with System; use System;
with Report; use Report;
procedure Cc1221d is

   subtype Subint is Integer range -100 .. 100;
   type Int is range -300 .. 300;
   subtype Sint1 is Int range Int (Ident_Int (-4)) .. Int (Ident_Int (4));
   type Int1 is range -6 .. 6;

begin
   Test
     ("CC1221D",
      "FOR A FORMAL INTEGER TYPE, CHECK THAT THE " &
      "FOLLOWING BASIC OPERATIONS ARE IMPLICITLY " &
      "DECLARED AND ARE THEREFORE AVAILABLE " &
      "WITHIN THE GENERIC UNIT:  EXPLICIT " &
      "CONVERSION TO AND FROM REAL TYPES AND " &
      "IMPLICIT CONVERSION FROM INTEGER LITERALS");

   declare -- (D) CHECKS FOR EXPLICIT CONVERSION TO AND FROM OTHER
      --     NUMERIC TYPES, AND IMPLICIT CONVERSION FROM
      --     INTEGER LITERALS.

      generic
         type T is range <>;
      procedure P (Str : String);

      procedure P (Str : String) is

         type Fixed is delta 0.1 range -100.0 .. 100.0;
         Fi0  : Fixed := 0.0;
         Fi2  : Fixed := 2.0;
         Fin2 : Fixed := -2.0;

         Fl0  : Float := 0.0;
         Fl2  : Float := 2.0;
         Fln2 : Float := -2.0;

         T0  : T := 0;
         T2  : T := 2;
         Tn2 : T := -2;

         function Ident (X : T) return T is
         begin
            if Equal (3, 3) then
               return X;
            else
               return T'First;
            end if;
         end Ident;

      begin
         if T0 + 1 /= 1 then
            Failed
              ("INCORRECT RESULTS FOR IMPLICIT " &
               "CONVERSION WITH TYPE " &
               Str &
               " - 1");
         end if;

         if T2 + 1 /= 3 then
            Failed
              ("INCORRECT RESULTS FOR IMPLICIT " &
               "CONVERSION WITH TYPE " &
               Str &
               " - 2");
         end if;

         if Tn2 + 1 /= -1 then
            Failed
              ("INCORRECT RESULTS FOR IMPLICIT " &
               "CONVERSION WITH TYPE " &
               Str &
               " - 3");
         end if;

         if T (Fi0) /= T0 then
            Failed
              ("INCORRECT CONVERSION FROM " &
               "FIXED VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if T (Fi2) /= Ident (T2) then
            Failed
              ("INCORRECT CONVERSION FROM " &
               "FIXED VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if T (Fin2) /= Tn2 then
            Failed
              ("INCORRECT CONVERSION FROM " &
               "FIXED VALUE -2.0 WITH TYPE " &
               Str);
         end if;

         if T (Fl0) /= Ident (T0) then
            Failed
              ("INCORRECT CONVERSION FROM " &
               "FLOAT VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if T (Fl2) /= T2 then
            Failed
              ("INCORRECT CONVERSION FROM " &
               "FLOAT VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if T (Fln2) /= Ident (Tn2) then
            Failed
              ("INCORRECT CONVERSION FROM " &
               "FLOAT VALUE -2.0 WITH TYPE " &
               Str);
         end if;

         if Fixed (T0) /= Fi0 then
            Failed
              ("INCORRECT CONVERSION TO " &
               "FIXED VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if Fixed (Ident (T2)) /= Fi2 then
            Failed
              ("INCORRECT CONVERSION TO " &
               "FIXED VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if Fixed (Tn2) /= Fin2 then
            Failed
              ("INCORRECT CONVERSION TO " &
               "FIXED VALUE -2.0 WITH TYPE " &
               Str);
         end if;

         if Float (Ident (T0)) /= Fl0 then
            Failed
              ("INCORRECT CONVERSION TO " &
               "FLOAT VALUE 0.0 WITH TYPE " &
               Str);
         end if;

         if Float (T2) /= Fl2 then
            Failed
              ("INCORRECT CONVERSION TO " &
               "FLOAT VALUE 2.0 WITH TYPE " &
               Str);
         end if;

         if Float (Ident (Tn2)) /= Fln2 then
            Failed
              ("INCORRECT CONVERSION TO " &
               "FLOAT VALUE -2.0 WITH TYPE " &
               Str);
         end if;

      end P;

      procedure P1 is new P (Subint);
      procedure P2 is new P (Sint1);
      procedure P3 is new P (Int1);

   begin
      P1 ("SUBINT");
      P2 ("SINT");
      P3 ("INT1");
   end; -- (D).

   Result;
end Cc1221d;
