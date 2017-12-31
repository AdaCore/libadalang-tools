-- C41321A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED RELATIONAL OPERATORS, LOGICAL OPERATORS,
-- AND THE "NOT" OPERATOR MAY BE SELECTED FROM OUTSIDE THE PACKAGE USING AN
-- EXPANDED NAME, FOR A DERIVED BOOLEAN TYPE.

-- TBN  7/16/86

with Report; use Report;
procedure C41321a is

   package P is
      type Derived_Boolean is new Boolean range False .. True;
      Derived_False : Derived_Boolean := False;
      Derived_True  : Derived_Boolean := True;
   end P;

   Dbool_False : P.Derived_Boolean := P.False;
   Dbool_True  : P.Derived_Boolean := P.True;

begin
   Test
     ("C41321A",
      "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
      "OPERATORS, LOGICAL OPERATORS, AND THE 'NOT' " &
      "OPERATOR MAY BE SELECTED FROM OUTSIDE THE " &
      "PACKAGE USING AN EXPANDED NAME, FOR A DERIVED " & "BOOLEAN TYPE");

   if P."=" (Dbool_False, P.Derived_True) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
   end if;

   if P."/=" (Dbool_True, P.Derived_True) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
   end if;

   if P."<" (P.Derived_True, P.Derived_False) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
   end if;

   if P.">" (Dbool_True, P.Derived_True) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
   end if;

   if P."<=" (P.Derived_True, Dbool_False) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
   end if;

   if P."<=" (P.Derived_True, Dbool_True) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
   end if;

   if P.">=" (P.Derived_True, Dbool_True) then
      null;
   else
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
   end if;

   for J in P.Derived_Boolean'(P.True) .. P.Derived_Boolean'(P.True) loop
      if P.">=" (Dbool_False, J) then
         Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
      end if;
   end loop;

   if P."AND" (Dbool_False, P.Derived_True) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
   end if;

   if P."OR" (Dbool_False, P.Derived_False) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
   end if;

   if P."XOR" (Dbool_True, P.Derived_True) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
   end if;

   if P."NOT" (P.Derived_True) then
      Failed ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
   end if;

   Result;
end C41321a;
