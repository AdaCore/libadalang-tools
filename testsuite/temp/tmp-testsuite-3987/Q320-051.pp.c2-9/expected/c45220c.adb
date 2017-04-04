-- C45220C.ADA

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
-- CHECK THAT  '='  AND  '/='  PRODUCE CORRECT RESULTS ON
--    OPERANDS OF A TYPE DERIVED FROM THE TYPE 'BOOLEAN'
--    (IN PARTICULAR, FOR OPERANDS HAVING DIFFERENT SUBTYPES).

-- THIS TEST IS DERIVED FROM  C45220A.ADA .

-- RM    27 OCTOBER 1980
-- JWC 7/8/85   RENAMED TO -AB

with Report;
procedure C45220c is

   use Report;

   type Nb is new Boolean;

   subtype T1 is Nb range Nb'(False) .. Nb'(False);
   subtype T2 is Nb range Nb'(True) .. Nb'(True);
   subtype T3 is Nb range Nb'(False) .. Nb'(True);
   subtype T4 is T3 range Nb'(True) .. Nb'(True);

   Fvar1 : T1 := Nb'(False);
   Tvar1 : T2 := Nb'(True);
   Fvar2 : T3 := Nb'(False);
   Tvar2 : T4 := Nb'(True);

   Error_Count : Integer := 0;   -- INITIAL VALUE ESSENTIAL

   procedure Bump is
   begin
      Error_Count := Error_Count + 1;
   end Bump;

   function Ident_New_Bool (The_Argument : Nb) return Nb is
   begin
      if Equal (2, 2) then
         return The_Argument;
      else
         return Nb'(False);
      end if;
   end Ident_New_Bool;

begin

   Test
     ("C45220C",
      "CHECK THAT  '='  AND  '/='  PRODUCE CORRECT" &
      " RESULTS ON DERIVED-BOOLEAN-TYPE OPERANDS");

   -- 32  CASES ( 2 * 2  ORDERED PAIRS OF OPERAND VALUES,
   --               2    OPERATORS : '=' , '/=' ,
   --               4    VARIABLE/LITERAL FOR LEFT OPERAND,
   --                    VARIABLE/LITERAL FOR RIGHT OPERAND.

   --  'BUMP'  MEANS  'BUMP THE ERROR COUNT'

   Fvar1 := Ident_New_Bool (Nb'(False));
   Tvar1 := Ident_New_Bool (Nb'(True));
   Fvar2 := Ident_New_Bool (Nb'(False));
   Tvar2 := Ident_New_Bool (Nb'(True));

   if Nb'(False) = Nb'(False) then
      null;
   else
      Bump;
   end if;
   if Fvar1 = Nb'(False) then
      null;
   else
      Bump;
   end if;
   if Nb'(False) = Fvar2 then
      null;
   else
      Bump;
   end if;
   if Fvar2 = Fvar1 then
      null;
   else
      Bump;
   end if;

   if Nb'(False) = Nb'(True) then
      Bump;
   end if;
   if Fvar1 = Nb'(True) then
      Bump;
   end if;
   if Nb'(False) = Tvar2 then
      Bump;
   end if;
   if Fvar2 = Tvar1 then
      Bump;
   end if;

   if Nb'(True) = Nb'(False) then
      Bump;
   end if;
   if Nb'(True) = Fvar1 then
      Bump;
   end if;
   if Tvar2 = Nb'(False) then
      Bump;
   end if;
   if Tvar1 = Fvar2 then
      Bump;
   end if;

   if Nb'(True) = Nb'(True) then
      null;
   else
      Bump;
   end if;
   if Tvar1 = Nb'(True) then
      null;
   else
      Bump;
   end if;
   if Nb'(True) = Tvar2 then
      null;
   else
      Bump;
   end if;
   if Tvar2 = Tvar1 then
      null;
   else
      Bump;
   end if;

   if Nb'(False) /= Nb'(False) then
      Bump;
   end if;
   if Fvar1 /= Nb'(False) then
      Bump;
   end if;
   if Nb'(False) /= Fvar2 then
      Bump;
   end if;
   if Fvar2 /= Fvar1 then
      Bump;
   end if;

   if Nb'(False) /= Nb'(True) then
      null;
   else
      Bump;
   end if;
   if Fvar1 /= Nb'(True) then
      null;
   else
      Bump;
   end if;
   if Nb'(False) /= Tvar2 then
      null;
   else
      Bump;
   end if;
   if Fvar2 /= Tvar1 then
      null;
   else
      Bump;
   end if;

   if Nb'(True) /= Nb'(False) then
      null;
   else
      Bump;
   end if;
   if Nb'(True) /= Fvar1 then
      null;
   else
      Bump;
   end if;
   if Tvar2 /= Nb'(False) then
      null;
   else
      Bump;
   end if;
   if Tvar1 /= Fvar2 then
      null;
   else
      Bump;
   end if;

   if Nb'(True) /= Nb'(True) then
      Bump;
   end if;
   if Tvar1 /= Nb'(True) then
      Bump;
   end if;
   if Nb'(True) /= Tvar2 then
      Bump;
   end if;
   if Tvar2 /= Tvar1 then
      Bump;
   end if;

   if Error_Count /= 0 then
      Failed ("(IN)EQUALITY OF N_BOOLEAN VALUES - FAILURE1");
   end if;

   Result;

end C45220c;
