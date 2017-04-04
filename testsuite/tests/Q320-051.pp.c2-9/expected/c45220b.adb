-- C45220B.ADA

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
-- CHECK THAT '<' , '<=' , '>' , '>=' PRODUCE CORRECT RESULTS ON
--    BOOLEAN-TYPE OPERANDS (IN PARTICULAR, FOR OPERANDS HAVING
--    DIFFERENT SUBTYPES).

-- THIS TEST IS DERIVED FROM C45220A.ADA .

-- RM    28 OCTOBER 1980
-- JWC 7/8/85 RENAMED TO -AB

with Report;
procedure C45220b is

   use Report;

   subtype T1 is Boolean range False .. False;
   subtype T2 is Boolean range True .. True;
   subtype T3 is Boolean range False .. True;
   subtype T4 is T3 range True .. True;

   Fvar1 : T1 := False;
   Tvar1 : T2 := True;
   Fvar2 : T3 := False;
   Tvar2 : T4 := True;

   Error_Count : Integer := 0;

   procedure Bump is
   begin
      Error_Count := Error_Count + 1;
   end Bump;

begin

   Test
     ("C45220B",
      "CHECK THAT  '<' , '<=' , '>' , '>='  PRODUCE" &
      " CORRECT RESULTS ON BOOLEAN-TYPE OPERANDS");

   -- 64 CASES ( 2 * 2 ORDERED PAIRS OF OPERAND VALUES,
   --               4    OPERATORS : '<' , <=' , '>' , '>='
   --               4    VARIABLE/LITERAL FOR LEFT OPERAND,
   --                    VARIABLE/LITERAL FOR RIGHT OPERAND.

   --  'BUMP' MEANS 'BUMP THE ERROR COUNT'

   Fvar1 := Ident_Bool (False);
   Tvar1 := Ident_Bool (True);
   Fvar2 := Ident_Bool (False);
   Tvar2 := Ident_Bool (True);

   Error_Count := 0;

   if False < False then
      Bump;
   end if;
   if Fvar1 < False then
      Bump;
   end if;
   if False < Fvar2 then
      Bump;
   end if;
   if Fvar2 < Fvar1 then
      Bump;
   end if;

   if False < True then
      null;
   else
      Bump;
   end if;
   if Fvar1 < True then
      null;
   else
      Bump;
   end if;
   if False < Tvar2 then
      null;
   else
      Bump;
   end if;
   if Fvar2 < Tvar1 then
      null;
   else
      Bump;
   end if;

   if True < False then
      Bump;
   end if;
   if True < Fvar1 then
      Bump;
   end if;
   if Tvar2 < False then
      Bump;
   end if;
   if Tvar1 < Fvar2 then
      Bump;
   end if;

   if True < True then
      Bump;
   end if;
   if Tvar1 < True then
      Bump;
   end if;
   if True < Tvar2 then
      Bump;
   end if;
   if Tvar2 < Tvar1 then
      Bump;
   end if;

   if Error_Count > 0 then
      Failed ("ORDERING OF BOOLEAN VALUES - FAILURE '<'");
   end if;

   Error_Count := 0;

   if False <= False then
      null;
   else
      Bump;
   end if;
   if Fvar1 <= False then
      null;
   else
      Bump;
   end if;
   if False <= Fvar2 then
      null;
   else
      Bump;
   end if;
   if Fvar2 <= Fvar1 then
      null;
   else
      Bump;
   end if;

   if False <= True then
      null;
   else
      Bump;
   end if;
   if Fvar1 <= True then
      null;
   else
      Bump;
   end if;
   if False <= Tvar2 then
      null;
   else
      Bump;
   end if;
   if Fvar2 <= Tvar1 then
      null;
   else
      Bump;
   end if;

   if True <= False then
      Bump;
   end if;
   if True <= Fvar1 then
      Bump;
   end if;
   if Tvar2 <= False then
      Bump;
   end if;
   if Tvar1 <= Fvar2 then
      Bump;
   end if;

   if True <= True then
      null;
   else
      Bump;
   end if;
   if Tvar1 <= True then
      null;
   else
      Bump;
   end if;
   if True <= Tvar2 then
      null;
   else
      Bump;
   end if;
   if Tvar2 <= Tvar1 then
      null;
   else
      Bump;
   end if;

   if Error_Count > 0 then
      Failed ("ORDERING OF BOOLEAN VALUES - FAILURE '<='");
   end if;

   Error_Count := 0;

   if False > False then
      Bump;
   end if;
   if Fvar1 > False then
      Bump;
   end if;
   if False > Fvar2 then
      Bump;
   end if;
   if Fvar2 > Fvar1 then
      Bump;
   end if;

   if False > True then
      Bump;
   end if;
   if Fvar1 > True then
      Bump;
   end if;
   if False > Tvar2 then
      Bump;
   end if;
   if Fvar2 > Tvar1 then
      Bump;
   end if;

   if True > False then
      null;
   else
      Bump;
   end if;
   if True > Fvar1 then
      null;
   else
      Bump;
   end if;
   if Tvar2 > False then
      null;
   else
      Bump;
   end if;
   if Tvar1 > Fvar2 then
      null;
   else
      Bump;
   end if;

   if True > True then
      Bump;
   end if;
   if Tvar1 > True then
      Bump;
   end if;
   if True > Tvar2 then
      Bump;
   end if;
   if Tvar2 > Tvar1 then
      Bump;
   end if;

   if Error_Count > 0 then
      Failed ("ORDERING OF BOOLEAN VALUES - FAILURE '>'");
   end if;

   Error_Count := 0;

   if False >= False then
      null;
   else
      Bump;
   end if;
   if Fvar1 >= False then
      null;
   else
      Bump;
   end if;
   if False >= Fvar2 then
      null;
   else
      Bump;
   end if;
   if Fvar2 >= Fvar1 then
      null;
   else
      Bump;
   end if;

   if False >= True then
      Bump;
   end if;
   if Fvar1 >= True then
      Bump;
   end if;
   if False >= Tvar2 then
      Bump;
   end if;
   if Fvar2 >= Tvar1 then
      Bump;
   end if;

   if True >= False then
      null;
   else
      Bump;
   end if;
   if True >= Fvar1 then
      null;
   else
      Bump;
   end if;
   if Tvar2 >= False then
      null;
   else
      Bump;
   end if;
   if Tvar1 >= Fvar2 then
      null;
   else
      Bump;
   end if;

   if True >= True then
      null;
   else
      Bump;
   end if;
   if Tvar1 >= True then
      null;
   else
      Bump;
   end if;
   if True >= Tvar2 then
      null;
   else
      Bump;
   end if;
   if Tvar2 >= Tvar1 then
      null;
   else
      Bump;
   end if;

   if Error_Count > 0 then
      Failed ("ORDERING OF BOOLEAN VALUES - FAILURE '>='");
   end if;

   Result;

end C45220b;
