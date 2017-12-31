-- CC1221A.ADA

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
--     WITHIN THE GENERIC UNIT:  ASSIGNMENT, MEMBERSHIP, QUALIFICATION,
--     AND EXPLICIT CONVERSION TO AND FROM OTHER INTEGER TYPES.

-- HISTORY:
--     RJW 09/26/86  CREATED ORIGINAL TEST.
--     BCB 11/12/87  CHANGED HEADER TO STANDARD FORMAT.  SPLIT TEST
--                   INTO PARTS A, B, C, AND D.

with System; use System;
with Report; use Report;
procedure Cc1221a is

   subtype Subint is Integer range -100 .. 100;
   type Newint is new Integer;
   type Int is range -300 .. 300;

begin
   Test
     ("CC1221A",
      "FOR A FORMAL INTEGER TYPE, CHECK THAT THE " &
      "FOLLOWING BASIC OPERATIONS ARE IMPLICITLY " &
      "DECLARED AND ARE THEREFORE AVAILABLE " &
      "WITHIN THE GENERIC UNIT:  ASSIGNMENT, " &
      "MEMBERSHIP, QUALIFICATION, AND EXPLICIT " &
      "CONVERSION TO AND FROM OTHER INTEGER TYPES");

   declare -- (A) CHECKS FOR BASIC OPERATIONS OF A DISCRETE TYPE.
      --     PART I.

      generic
         type T is range <>;
         type T1 is range <>;
         I : T;
         I1 : T1;
      procedure P (J : T; Str : String);

      procedure P (J : T; Str : String) is
         subtype St is T range T'Val (-1) .. T'Val (1);
         K, L : T;

         function F (X : T) return Boolean is
         begin
            return Ident_Bool (True);
         end F;

         function F (X : T1) return Boolean is
         begin
            return Ident_Bool (False);
         end F;

      begin
         K := I;
         L := J;
         K := L;

         if K /= J then
            Failed
              ("INCORRECT RESULTS FOR ASSIGNMENT " & "WITH TYPE - " & Str);
         end if;

         if I in St then
            null;
         else
            Failed ("INCORRECT RESULTS FOR ""IN"" WITH " & "TYPE  - " & Str);
         end if;

         if J not in St then
            null;
         else
            Failed
              ("INCORRECT RESULTS FOR ""NOT IN"" WITH " & "TYPE  - " & Str);
         end if;

         if T'(I) /= I then
            Failed
              ("INCORRECT RESULTS FOR QUALIFICATION " & "WITH TYPE - " & Str &
               " - 1");
         end if;

         if F (T'(1)) then
            null;
         else
            Failed
              ("INCORRECT RESULTS FOR QUALIFICATION " & "WITH TYPE - " & Str &
               " - 2");
         end if;

         if T (I1) /= I then
            Failed
              ("INCORRECT RESULTS FOR EXPLICIT " & "CONVERSION WITH TYPE - " &
               Str & " - 1");
         end if;

         if F (T (I1)) then
            null;
         else
            Failed
              ("INCORRECT RESULTS FOR EXPLICIT  " & "CONVERSION WITH TYPE - " &
               Str & " - 2");
         end if;

      end P;

      procedure Np1 is new P (Subint, Subint, 0, 0);
      procedure Np2 is new P (Newint, Newint, 0, 0);
      procedure Np3 is new P (Int, Int, 0, 0);
      procedure Np4 is new P (Integer, Integer, 0, 0);

   begin
      Np1 (2, "SUBINT");
      Np2 (2, "NEWINT");
      Np3 (2, "INT");
      Np4 (2, "INTEGER");
   end; -- (A).

   Result;
end Cc1221a;
