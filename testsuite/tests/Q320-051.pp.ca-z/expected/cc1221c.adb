-- CC1221C.ADA

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
--     WITHIN THE GENERIC UNIT:  ATTRIBUTES 'POS, 'VAL, 'PRED, 'SUCC,
--     'IMAGE, AND 'VALUE.

-- HISTORY:
--     BCB 11/12/87  CREATED ORIGINAL TEST FROM SPLIT OF CC1221A.ADA

with System; use System;
with Report; use Report;
procedure Cc1221c is

   subtype Subint is Integer range -100 .. 100;
   type Newint is new Integer;
   type Int is range -300 .. 300;
   subtype Sint1 is Int range Int (Ident_Int (-4)) .. Int (Ident_Int (4));
   type Int1 is range -6 .. 6;

begin
   Test
     ("CC1221C",
      "FOR A FORMAL INTEGER TYPE, CHECK THAT THE " &
      "FOLLOWING BASIC OPERATIONS ARE IMPLICITLY " &
      "DECLARED AND ARE THEREFORE AVAILABLE " &
      "WITHIN THE GENERIC UNIT:  ATTRIBUTES 'POS, " &
      "'VAL, 'PRED, 'SUCC, 'IMAGE, AND 'VALUE");

   declare -- (C1) CHECKS FOR BASIC OPERATIONS OF A DISCRETE TYPE.
      --      PART III.

      generic
         type T is range <>;
         F : Integer;
      procedure P (Str : String);

      procedure P (Str : String) is
         I : Integer;
         Y : T;

         function Ident (X : T) return T is
         begin
            if Equal (3, 3) then
               return X;
            else
               return T'Succ (T'First);
            end if;
         end Ident;

      begin
         I := F;
         for X in T loop
            if T'Val (I) /= X then
               Failed
                 ("WRONG VALUE FOR " & Str & "'VAL OF " & Integer'Image (I));
            end if;

            if T'Pos (X) /= I then
               Failed ("WRONG VALUE FOR " & Str & "'POS OF " & T'Image (X));
            end if;

            I := I + 1;
         end loop;

         for X in T loop
            if T'Succ (X) /= T'Val (T'Pos (X) + 1) then
               Failed ("WRONG VALUE FOR " & Str & "'SUCC OF " & T'Image (X));
            end if;

            if T'Pred (X) /= T'Val (T'Pos (X) - 1) then
               Failed ("WRONG VALUE FOR " & Str & "'PRED OF " & T'Image (X));
            end if;
         end loop;

         begin
            Y := T'Succ (Ident (T'Base'Last));
            Failed
              ("NO EXCEPTION RAISED FOR " &
               Str &
               "'SUCC (IDENT (" &
               Str &
               "'BASE'LAST))");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED FOR " &
                  Str &
                  "'SUCC (IDENT (" &
                  Str &
                  "'BASE'LAST))");
         end;

         begin
            Y := T'Pred (Ident (T'Base'First));
            Failed
              ("NO EXCEPTION RAISED FOR " &
               Str &
               "'PRED (IDENT (" &
               Str &
               "'BASE'FIRST))");
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED FOR " &
                  Str &
                  "'PRED (IDENT (" &
                  Str &
                  "'BASE'FIRST))");
         end;

      end P;

      procedure P1 is new P (Subint, -100);
      procedure P2 is new P (Sint1, -4);
      procedure P3 is new P (Int1, -6);

   begin
      P1 ("SUBINT");
      P2 ("SINT");
      P3 ("INT1");
   end; -- (C1).

   declare -- (C2) CHECKS FOR BASIC OPERATIONS OF A DISCRETE TYPE.
      --      PART IV.

      generic
         type T is range <>;
         Str : String;
      package Pkg is
      end Pkg;

      package body Pkg is
         procedure P (Im : String; Va : T) is
         begin
            if T'Image (Va) /= Im then
               Failed
                 ("INCORRECT RESULTS FOR " &
                  Str &
                  "'IMAGE OF " &
                  Integer'Image (Integer (Va)));
            end if;
         end P;

         procedure Q (Im : String; Va : T) is
         begin
            if T'Value (Im) /= Va then
               Failed ("INCORRECT RESULTS FOR " & Str & "'VALUE OF " & Im);
            end if;
         exception
            when Constraint_Error =>
               Failed
                 ("CONSTRAINT_ERROR RAISED FOR " & Str & "'VALUE OF " & Im);
            when others =>
               Failed
                 ("OTHER EXCEPTION RAISED FOR " & Str & "'VALUE OF " & Im);

         end Q;

      begin
         P (" 2", 2);
         P ("-1", -1);

         Q (" 2", 2);
         Q ("-1", -1);
         Q ("        2", 2);
         Q ("-1     ", -1);
      end Pkg;

      package Pkg1 is new Pkg (Subint, "SUBINT");
      package Pkg2 is new Pkg (Sint1, "SINT1");
      package Pkg3 is new Pkg (Int1, "INT1");
      package Pkg4 is new Pkg (Newint, "NEWINT");

   begin
      null;
   end; -- (C2).

   Result;
end Cc1221c;
