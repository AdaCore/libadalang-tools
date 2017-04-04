-- C35502P.ADA

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
--     FOR AN ENUMERATION TYPE OTHER THAN BOOLEAN OR CHARACTER TYPE,
--     CHECK THAT THE RESULTS AND TYPE PRODUCED BY THE ATTRIBUTES
--     ARE CORRECT.

--     CHECK THAT 'FIRST AND 'LAST YIELD CORRECT RESULTS WHEN THE
--     PREFIX DENOTES A NULL SUBTYPE.

-- HISTORY:
--     RJW 05/05/86  CREATED ORIGINAL TEST.
--     CJJ 06/09/87  CHANGED "=" COMPARISONS IN GENERIC
--                   PROCEDURE Q TO "/=".

with Report; use Report;

procedure C35502p is

begin

   Test
     ("C35502P",
      "CHECK THAT THE ATTRIBUTES 'FIRST' AND " &
      "'LAST' YIELD THE CORRECT RESULTS WHEN THE " &
      "PREFIX IS A GENERIC FORMAL DISCRETE TYPE " &
      "WHOSE ACTUAL PARAMETER IS AN ENUMERATION " &
      "TYPE OTHER THAN A CHARACTER OR A BOOLEAN " &
      "TYPE");

   declare
      -- FOR THESE DECLARATIONS, 'FIRST AND 'LAST REFER TO THE SUBTYPE VALUES,
      -- BUT 'VAL AND 'POS ARE INHERITED FROM THE BASE TYPE.

      type Enum is (A, Bc, Abc, A_B_C, Abcd);
      subtype Subenum is Enum range A .. Abc;

      type Newenum is new Enum range Bc .. A_B_C;
      type Nonewenum is new Enum range Abcd .. A;
      generic
         type E is (<>);
         F, L : E;
      procedure P (Str : String);

      procedure P (Str : String) is
         subtype Noenum is
           E range E'Val (Ident_Int (2)) .. E'Val (Ident_Int (1));
      begin
         if E'First /= F then
            Failed ("INCORRECT E'FIRST FOR " & Str);
         end if;
         if Noenum'First /= E'Val (2) then
            Failed ("INCORRECT NOENUM'FIRST FOR " & Str);
         end if;

         if E'Last /= L then
            Failed ("INCORRECT E'LAST FOR " & Str);
         end if;
         if Noenum'Last /= E'Val (1) then
            Failed ("INCORRECT NOENUM'LAST FOR " & Str);
         end if;
      end P;

      generic
         type E is (<>);
      procedure Q;

      procedure Q is
         subtype Noenum is
           E range E'Val (Ident_Int (2)) .. E'Val (Ident_Int (1));
      begin
         if E'First /= E'Val (Ident_Int (4)) then
            Failed ("INCORRECT E'FIRST FOR NONEWENUM");
         end if;
         if Noenum'First /= E'Val (2) then
            Failed ("INCORRECT NOENUM'FIRST FOR NONEWENUM");
         end if;

         if E'Last /= E'Val (Ident_Int (0)) then
            Failed ("INCORRECT E'LAST FOR NONEWENUM");
         end if;
         if Noenum'Last /= E'Val (1) then
            Failed ("INCORRECT NOENUM'LAST FOR NONEWENUM");
         end if;
      end Q;

      procedure Proc1 is new P (Enum, A, Abcd);
      procedure Proc2 is new P (Subenum, A, Abc);
      procedure Proc3 is new P (Newenum, Bc, A_B_C);
      procedure Proc4 is new Q (Nonewenum);

   begin
      Proc1 ("ENUM");
      Proc2 ("SUBENUM");
      Proc3 ("NEWENUM");
      Proc4;
   end;

   Result;
end C35502p;
