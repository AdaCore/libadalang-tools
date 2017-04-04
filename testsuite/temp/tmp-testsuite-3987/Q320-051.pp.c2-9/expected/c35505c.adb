-- C35505C.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR 'SUCC' AND 'PRED',
--     IF THE RETURNED VALUES WOULD BE OUTSIDE OF THE BASE TYPE,
--     WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT
--     IS A USER-DEFINED ENUMERATION TYPE.

-- HISTORY:
--     RJW 06/05/86  CREATED ORIGINAL TEST.
--     VCL 08/19/87  REMOVED THE FUNCTION 'IDENT' IN THE GENERIC
--                   PROCEDURE 'P' AND REPLACED ALL CALLS TO 'IDENT'
--                   WITH "T'VAL(IDENT_INT(T'POS(...)))".

with Report; use Report;

procedure C35505c is

   type B is ('Z', 'X', Z, X);

   subtype C is B range 'X' .. Z;

begin
   Test
     ("C35505C",
      "CHECK THAT 'SUCC' AND 'PRED' RAISE " &
      "CONSTRAINT_ERROR APPROPRIATELY WHEN THE " &
      "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
      "ARGUMENT IS A USER-DEFINED ENUMERATION TYPE");

   declare
      generic
         type T is (<>);
         Str : String;
      procedure P;

      procedure P is

      begin
         begin
            if T'Pred (T'Val (Ident_Int (T'Pos (T'Base'First)))) = T'First then
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'PRED -  1");
            else
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'PRED -  2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED FOR " & Str & "'PRED - 1");
         end;

         begin
            if T'Succ (T'Val (Ident_Int (T'Pos (T'Base'Last)))) = T'Last then
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'SUCC -  1");
            else
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED FOR " & Str & "'SUCC -  2");
            end if;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED FOR " & Str & "'SUCC - 1");
         end;
      end P;

      procedure Pb is new P (B, "B");
      procedure Pc is new P (C, "C");
   begin
      Pb;
      Pc;
   end;
   Result;
end C35505c;
