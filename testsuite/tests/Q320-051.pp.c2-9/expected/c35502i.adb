-- C35502I.ADA

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
--     CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULTS WHEN
--     THE PREFIX IS AN ENUMERATION TYPE, OTHER THAN A BOOLEAN OR A
--     CHARACTER TYPE, WITH A REPRESENTATION CLAUSE.

-- HISTORY:
--     RJW 05/27/86  CREATED ORIGINAL TEST.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

with Report; use Report;

procedure C35502i is

   type Enum is (A, Bc, Abc, A_B_C, Abcd);
   for Enum use (A => 2, Bc => 4, Abc => 6, A_B_C => 8, Abcd => 10);
   subtype Subenum is Enum range A .. Bc;

   type Newenum is new Enum;
   subtype Subnew is Newenum range A .. Bc;

begin
   Test
     ("C35502I",
      "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
      "ENUMERATION TYPE, OTHER THAN A CHARACTER " &
      "OR A BOOLEAN TYPE, WITH A REPRESENTATION " & "CLAUSE");

   begin
      for I in Enum'Val (1) .. Enum'Val (4) loop
         if Subenum'Pred (I) /= Enum'Val (Enum'Pos (I) - 1) then
            Failed ("INCORRECT SUBENUM'PRED(" & Enum'Image (I) & ")");
         end if;
      end loop;

      for I in Enum'Val (0) .. Enum'Val (3) loop
         if Subenum'Succ (I) /= Enum'Val (Enum'Pos (I) + 1) then
            Failed ("INCORRECT SUBENUM'SUCC(" & Enum'Image (I) & ")");
         end if;
      end loop;
   end;

   begin
      for I in Newenum'Val (1) .. Newenum'Val (4) loop
         if Subnew'Pred (I) /= Newenum'Val (Newenum'Pos (I) - 1) then
            Failed ("INCORRECT SUBNEW'PRED(" & Newenum'Image (I) & ")");
         end if;
      end loop;

      for I in Newenum'Val (0) .. Newenum'Val (3) loop
         if Subnew'Succ (I) /= Newenum'Val (Newenum'Pos (I) + 1) then
            Failed ("INCORRECT SUBNEW'SUCC(" & Newenum'Image (I) & ")");
         end if;
      end loop;
   end;

   Result;
end C35502i;
