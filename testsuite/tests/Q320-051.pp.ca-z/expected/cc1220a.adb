-- CC1220A.ADA

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
--     CHECK THAT A GENERIC UNIT CAN REFER TO AN IMPLICITLY
--     DECLARED PREDEFINED OPERATOR.

-- HISTORY:
--     DAT 08/20/81  CREATED ORIGINAL TEST.
--     SPS 05/03/82
--     BCB 08/04/88  MODIFIED HEADER FORMAT AND ADDED CHECKS FOR OTHER
--                   OPERATIONS OF A DISCRETE TYPE.
--     RJW 03/27/90  REVISED TEST TO CHECK FOR A GENERIC FORMAL
--                   DISCRETE TYPE.
--     CJJ 10/14/90  ADDED CHECKS FOR RELATIONAL OPERATOR (<, <=, >, >=);
--                   MADE FAILED MESSAGES IN PROCEDURE BODY MORE SPECIFIC.

with Report; use Report;
with System; use System;

procedure Cc1220a is

begin
   Test
     ("CC1220A",
      "GENERIC UNIT CAN REFER TO IMPLICITLY " & "DECLARED OPERATORS");

   declare

      generic
         type T is (<>);
         Str : String;
         P1 : T := T'First;
         P2 : T := T (T'Succ (P1));
         P3 : T := T'(T'Pred (P2));
         P4 : Integer := Ident_Int (T'Width);
         P5 : Boolean := (P1 < P2) and (P2 > P3);
         P6 : Boolean := (P1 <= P3) and (P2 >= P1);
         P7 : Boolean := (P3 = P1);
         P8 : T := T'Base'First;
         P10 : T := T'Last;
         P11 : Integer := T'Size;
         P12 : Address := P10'Address;
         P13 : Integer := T'Width;
         P14 : Integer := T'Pos (T'Last);
         P15 : T := T'Val (1);
         P16 : Integer := T'Pos (P15);
         P17 : String := T'Image (T'Base'Last);
         P18 : T := T'Value (P17);
         P19 : Boolean := (P15 in T);
         with function Ident (X : T) return T;
      package Pkg is
         Arr : array (1 .. 3) of T := (P1, P2, P3);
         B1  : Boolean             := P7 and P19;
         B2  : Boolean             := P5 and P6;
      end Pkg;

      package body Pkg is
      begin
         if P1 /= T (T'First) then
            Failed ("IMPROPER VALUE FOR 'FIRST - " & Str);
         end if;

         if T'Succ (P1) /= Ident (P2) or T'Pred (P2) /= Ident (P1) then
            Failed ("IMPROPER VALUE FOR 'SUCC, PRED - " & Str);
         end if;

         if P10 /= T (T'Last) then
            Failed ("IMPROPER VALUE FOR 'LAST - " & Str);
         end if;

         if not Equal (P11, T'Size) then
            Failed ("IMPROPER VALUE FOR 'SIZE - " & Str);
         end if;

         if not Equal (P13, T'Width) then
            Failed ("IMPROPER VALUE FOR 'WIDTH - " & Str);
         end if;

         if not Equal (P16, T'Pos (P15)) or T'Val (P16) /= T (Ident (P15)) then
            Failed ("IMPROPER VALUE FOR 'POS, 'VAL - " & Str);
         end if;

         if T'Value (P17) /= T'Base'Last or
           T'Image (P18) /= T'Image (T'Base'Last) then
            Failed ("IMPROPER VALUE FOR 'VALUE, 'IMAGE - " & Str);
         end if;
      end Pkg;

   begin
      declare
         type Char is ('A', 'B', 'C', 'D', 'E');

         function Ident (C : Char) return Char is
         begin
            return Char'Val (Ident_Int (Char'Pos (C)));
         end Ident;

         package N_Char is new Pkg (T => Char, Str => "CHAR", Ident => Ident);
      begin
         if N_Char.Arr (1) /= Ident ('A') or N_Char.Arr (2) /= Ident ('B') or
           N_Char.Arr (3) /= 'A' or N_Char.B1 /= True or N_Char.B2 /= True then
            Failed
              ("IMPROPER VALUES FOR ARRAY COMPONENTS" &
               " IN INSTANTIATION OF N_CHAR.");
         end if;
      end;

      declare
         type Enum is (Jovial, Ada, Fortran, Basic);

         function Ident (C : Enum) return Enum is
         begin
            return Enum'Val (Ident_Int (Enum'Pos (C)));
         end Ident;

         package N_Enum is new Pkg (T => Enum, Str => "ENUM", Ident => Ident);

      begin
         if N_Enum.Arr (1) /= Ident (Jovial) or
           N_Enum.Arr (2) /= Ident (Ada) or N_Enum.Arr (3) /= Jovial or
           N_Enum.B1 /= True or N_Enum.B2 /= True then
            Failed
              ("IMPROPER VALUES FOR ARRAY COMPONENTS" &
               " IN INSTANTIATION OF N_ENUM.");
         end if;
      end;

      declare

         package N_Int is new Pkg (T => Integer, Str => "INTEGER",
            Ident                    => Ident_Int);
      begin
         if N_Int.Arr (1) /= Ident_Int (Integer'First) or
           N_Int.Arr (2) /= Ident_Int (Integer'First + 1) or
           N_Int.Arr (3) /= Integer'First or N_Int.B1 /= True or
           N_Int.B2 /= True then
            Failed
              ("IMPROPER VALUES FOR ARRAY COMPONENTS" &
               " IN INSTANTIATION OF N_INT.");
         end if;
      end;
   end;
   Result;
end Cc1220a;
