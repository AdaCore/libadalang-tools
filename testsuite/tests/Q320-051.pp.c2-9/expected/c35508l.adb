-- C35508L.ADA

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
-- CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE PREFIX IS A
-- FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS A BOOLEAN TYPE.

-- RJW 3/24/86

with Report; use Report;

procedure C35508l is

begin
   Test
     ("C35508L",
      "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS A " &
      "FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER " & "IS A BOOLEAN TYPE");

   declare
      type Newbool is new Boolean;

      generic
         type Bool is (<>);
      procedure P (Str : String; B : Bool; I : Integer);

      procedure P (Str : String; B : Bool; I : Integer) is
         subtype Sbool is
           Bool range Bool'Val (Ident_Int (0)) .. Bool'Val (Ident_Int (0));
      begin
         if Bool'Pos (B) /= I then
            Failed ("WRONG " & Str & "'POS FOR " & Bool'Image (B) & " - 1");
         end if;
         if Bool'Val (I) /= B then
            Failed ("WRONG " & Str & "'VAL FOR " & Integer'Image (I) & " - 1");
         end if;

         if Sbool'Pos (B) /= I then
            Failed ("WRONG " & Str & "'POS FOR " & Bool'Image (B) & " - 2");
         end if;

         if Sbool'Val (I) /= B then
            Failed ("WRONG " & Str & "'VAL FOR " & Integer'Image (I) & " - 2");
         end if;
      end P;

      generic
         type Bool is (<>);
      procedure Q (Str : String; B : Bool; I : Integer);

      procedure Q (Str : String; B : Bool; I : Integer) is
         subtype Sbool is
           Bool range Bool'Val (Ident_Int (0)) .. Bool'Val (Ident_Int (0));
      begin
         begin
            if Bool'Val (I) = B then
               Failed
                 (Str & "'VAL OF " & Integer'Image (I) & " = " &
                  Bool'Image (B));
            end if;
            Failed
              ("NO EXCEPTION RAISED FOR " & Str & "'VAL OF " &
               Integer'Image (I));
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED FOR " & Str & "'VAL " & "OF " &
                  Integer'Image (I));
         end;

         begin
            if Sbool'Val (I) = B then
               Failed
                 (Str & " SBOOL'VAL OF " & Integer'Image (I) & " = " &
                  Bool'Image (B));
            end if;
            Failed
              ("NO EXCEPTION RAISED FOR VAL OF " & Integer'Image (I) &
               "WITH SBOOL OF " & Str);
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED FOR " & Str & "'VAL " & "OF " &
                  Integer'Image (I) & "WITH SBOOL ");
         end;
      end Q;

      procedure Np1 is new P (Bool => Boolean);
      procedure Np2 is new P (Bool => Newbool);
      procedure Nq1 is new Q (Bool => Boolean);
      procedure Nq2 is new Q (Bool => Newbool);
   begin
      Np1 ("BOOLEAN", Ident_Bool (False), Ident_Int (0));
      Np1 ("BOOLEAN", Ident_Bool (True), Ident_Int (1));
      Np2 ("NEWBOOL", False, 0);
      Np2 ("NEWBOOL", True, 1);
      Nq1 ("BOOLEAN", Ident_Bool (False), Ident_Int (-1));
      Nq1 ("BOOLEAN", Ident_Bool (True), Ident_Int (2));
      Nq2 ("NEWBOOL", False, -1);
      Nq2 ("NEWBOOL", True, 2);
   end;

   Result;
end C35508l;
