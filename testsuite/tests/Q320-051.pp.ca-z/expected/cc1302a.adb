-- CC1302A.ADA

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
-- CHECK THAT GENERIC DEFAULT SUBPROGRAM PARAMETERS MAY BE ATTRIBUTES OF TYPES,
-- INCLUDING GENERIC FORMAL TYPES IN SAME GENERIC PART, OR IN GENERIC PART OF
-- ENCLOSING UNIT.

-- DAT 8/27/81
-- SPS 2/9/83
-- JBG 2/15/83
-- JBG 4/29/83

with Report; use Report;

procedure Cc1302a is
begin
   Test
     ("CC1302A",
      "GENERIC DEFAULT SUBPROGRAMS MAY BE" & " FUNCTION ATTRIBUTES OF TYPES");

   declare
      generic
         type T is (<>);
         T_Last : T;
         with function Succ (X : T) return T is T'Succ;
      package Pk1 is
      end Pk1;

      subtype Ch is Character range Character'First .. '~';
      subtype Bl is Boolean range False .. False;
      subtype Int is Integer range -10 .. 10;

      package body Pk1 is
         generic
            type Tt is (<>);
            Tt_Last : Tt;
            with function Pred (X : Tt) return Tt is Tt'Pred;
            with function Im (X : T) return String is T'Image;
            with function Val (X : String) return Tt is Tt'Value;
         package Pk2 is
         end Pk2;

         package body Pk2 is
         begin

-- CHECK THAT 'LAST GIVES RIGHT ANSWER
            if T'Last /= T_Last then
               Failed ("T'LAST INCORRECT");
            end if;

            if Tt'Last /= Tt_Last then
               Failed ("TT'LAST INCORRECT");
            end if;

-- CHECK SUCC FUNCTION
            begin
               if T'Pred (Succ (T'Last)) /= T'Last then
                  Failed ("'PRED OR SUCC GIVES WRONG " & "RESULT");
               end if;
            exception
               when Constraint_Error =>
                  Failed ("SUCC HAS CONSTRAINTS OF " & "SUBTYPE");
               when others =>
                  Failed ("SOME EXCEPTION RAISED - 1");
            end;

-- CHECK 'SUCC ATTRIBUTE
            begin
               if T'Pred (T'Succ (T'Last)) /= T'Last then
                  Failed ("'PRED OR 'SUCC GIVES WRONG " & "RESULT");
               end if;
            exception
               when Constraint_Error =>
                  Failed ("'PRED OR 'SUCC HAS CONSTRAINTS " & "OF SUBTYPE");
               when others =>
                  Failed ("SOME EXCEPTION RAISED - 2");
            end;

-- CHECK VAL ATTRIBUTE
            begin
               if T'Val (T'Pos (T'Succ (T'Last))) /=
                 T'Val (T'Pos (T'Last) + 1)
               then
                  Failed
                    ("VAL OR POS ATTRIBUTE HAS " & "INCONSISTENT RESULTS");
               end if;
            exception
               when Constraint_Error =>
                  Failed
                    ("VAL OR POS ATTRIBUTE HAS " & "CONSTRAINTS OF SUBTYPE");
               when others =>
                  Failed ("SOME EXCEPTION RAISED - 4");
            end;

-- CHECK VAL FUNCTION
            begin
               if Tt'Val (Tt'Pos (Tt'Succ (Tt'Last))) /=
                 Tt'Val (Tt'Pos (Tt'Last) + 1)
               then
                  Failed ("VAL FUNCTION GIVES INCORRECT " & "RESULTS");
               end if;
            exception
               when Constraint_Error =>
                  Failed ("VAL FUNCTION HAS CONSTRAINTS " & "OF SUBTYPE");
               when others =>
                  Failed ("SOME EXCEPTION RAISED - 6");
            end;

-- CHECK IM FUNCTION
            begin
               if T'Image (T'Succ (T'Last)) /= Im (T'Succ (T'Last)) then
                  Failed ("IM FUNCTION GIVES INCORRECT " & "RESULTS");
               end if;
            exception
               when Constraint_Error =>
                  Failed ("IM FUNCTION HAS CONSTRAINTS " & "OF SUBTYPE");
               when others =>
                  Failed ("SOME EXCEPTION RAISED - 7");
            end;

-- CHECK PRED FUNCTION
            begin
               if Pred (Tt'Succ (Tt'Last)) /= Tt'Last then
                  Failed ("PRED FUNCTION GIVES INCORRECT " & "RESULTS");
               end if;
            exception
               when Constraint_Error =>
                  Failed ("PRED FUNCTION HAS CONSTRAINTS " & "OF SUBTYPE");
               when others =>
                  Failed ("SOME EXCEPTION RAISED - 8");
            end;

         end Pk2;

         package Pk3 is new Pk2 (T, T'Last);
      end Pk1;

      package Pkg1 is new Pk1 (Ch, Ch'Last);
      package Pkg2 is new Pk1 (Bl, Bl'Last);
      package Pkg3 is new Pk1 (Int, Int'Last);
   begin
      null;
   end;

   Result;
end Cc1302a;
