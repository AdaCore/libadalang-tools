-- C35502K.ADA

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
-- CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE PREFIX IS AN
-- ENUMERATION TYPE OTHER THAN A BOOLEAN OR A CHARACTER TYPE.

-- RJW 5/27/86
-- GMT 7/02/87 ADDED ENUM'VAL(3) CHECK NEAR END OF 2ND BLOCK STATEMENT.

with Report; use Report;

procedure C35502k is

   type Enum is (A, Bc, Abc, A_B_C, Abcd);
   subtype Subenum is Enum range A .. Bc;

   type Newenum is new Enum;
   subtype Subnew is Newenum range A .. Bc;

begin
   Test
     ("C35502K",
      "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
      "ENUMERATION TYPE OTHER THAN A CHARACTER " &
      "OR A BOOLEAN TYPE");

   declare
      Position : Integer;
   begin
      Position := 0;

      for E in Enum loop
         if Subenum'Pos (E) /= Position then
            Failed ("INCORRECT SUBENUM'POS (" & Enum'Image (E) & ")");
         end if;

         if Subenum'Val (Position) /= E then
            Failed
              ("INCORRECT SUBENUM'VAL (" & Integer'Image (Position) & ")");
         end if;

         Position := Position + 1;
      end loop;

      Position := 0;
      for E in Newenum loop
         if Subnew'Pos (E) /= Position then
            Failed ("INCORRECT SUBNEW'POS (" & Newenum'Image (E) & ")");
         end if;

         if Subnew'Val (Position) /= E then
            Failed ("INCORRECT SUBNEW'VAL (" & Integer'Image (Position) & ")");
         end if;

         Position := Position + 1;
      end loop;
   end;

   declare
      function A_B_C return Enum is
      begin
         return Enum'Val (Ident_Int (0));
      end A_B_C;

   begin
      if Enum'Val (0) /= A_B_C then
         Failed ("WRONG ENUM'VAL (0) WHEN HIDDEN " & "BY FUNCTION - 1");
      end if;

      if Enum'Val (0) = C35502k.A_B_C then
         Failed ("WRONG ENUM'VAL (0) WHEN HIDDEN " & "BY FUNCTION - 2");
      end if;

      if Enum'Val (3) /= C35502k.A_B_C then
         Failed ("WRONG ENUM'VAL (3) WHEN HIDDEN " & "BY FUNCTION - 3");
      end if;
   end;

   begin
      if Enum'Val (Ident_Int (-1)) = A then
         Failed ("NO EXCEPTION RAISED " & "FOR ENUM'VAL (IDENT_INT (-1)) - 1");
      else
         Failed ("NO EXCEPTION RAISED " & "FOR ENUM'VAL (IDENT_INT (-1)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "FOR ENUM'VAL (IDENT_INT (-1))");
   end;

   begin
      if Newenum'Val (Ident_Int (-1)) = A then
         Failed
           ("NO EXCEPTION RAISED FOR " & "NEWENUM'VAL (IDENT_INT (-1)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " & "NEWENUM'VAL (IDENT_INT (-1)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR " & "NEWENUM'VAL (IDENT_INT (-1))");
   end;

   begin
      if Enum'Val (Ident_Int (5)) = A then
         Failed ("NO EXCEPTION RAISED " & "FOR ENUM'VAL (IDENT_INT (5)) - 1");
      else
         Failed ("NO EXCEPTION RAISED " & "FOR ENUM'VAL (IDENT_INT (5)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "FOR ENUM'VAL (IDENT_INT (5))");
   end;

   begin
      if Newenum'Val (Ident_Int (5)) = A then
         Failed
           ("NO EXCEPTION RAISED FOR " & "NEWENUM'VAL (IDENT_INT (5)) - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR " & "NEWENUM'VAL (IDENT_INT (5)) - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR " & "NEWENUM'VAL (IDENT_INT (5))");
   end;

   Result;
end C35502k;
