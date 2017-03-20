-- C41326A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED EQUALITY AND INEQUALITY OPERATORS
-- MAY BE SELECTED FROM OUTSIDE A PACKAGE USING AN EXPANDED NAME, FOR
-- AN ACCESS TYPE.

-- TBN  7/18/86

with Report; use Report;
procedure C41326a is

   package P is
      type Cell is record
         Value : Integer;
      end record;
      type Link is access Cell;

      Obj_Link_1 : Link := new Cell'(Value => 1);
      Obj_Link_2 : Link := Obj_Link_1;
   end P;

   Var_Link_1 : P.Link := new P.Cell'(Value => 1);
   Var_Link_2 : P.Link := new P.Cell'(Value => 2);

begin
   Test
     ("C41326A",
      "CHECK THAT IMPLICITLY DECLARED EQUALITY AND " &
      "INEQUALITY OPERATORS MAY BE SELECTED FROM " &
      "OUTSIDE A PACKAGE USING AN EXPANDED NAME, " &
      "FOR AN ACCESS TYPE");

   if P."=" (Var_Link_1, P.Obj_Link_1) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."/=" (P.Obj_Link_1, P.Obj_Link_2) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   if P."=" (Var_Link_2.all, P.Obj_Link_1.all) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
   end if;

   Var_Link_2.Value := 1;
   if P."/=" (Var_Link_2.all, P.Obj_Link_1.all) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
   end if;

   Result;
end C41326a;
