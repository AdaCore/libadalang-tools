-- C45662A.ADA

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
-- CHECK THE TRUTH TABLE FOR 'NOT' .

-- THE COMBINATIONS OF 'NOT' WITH 'AND' , 'OR' , 'XOR' ARE TESTED
--    IN C45101(A,G).

-- RM    28 OCTOBER 1980
-- TBN 10/21/85 RENAMED FROM C45401A.ADA.

with Report;
procedure C45662a is

   use Report;

   Tvar, Fvar, Cvar : Boolean := False; -- INITIAL VALUE IRRELEVANT
   Error_Count      : Integer := 0;            -- INITIAL VALUE ESSENTIAL

   procedure Bump is
   begin
      Error_Count := Error_Count + 1;
   end Bump;

begin

   Test ("C45662A", "CHECK THE TRUTH TABLE FOR  'NOT'");

   for A in Boolean loop

      Cvar := not A;

      if not A then
         if A then
            Bump;
         end if;
      end if;

      if Cvar then
         if A then
            Bump;
         end if;
      end if;

      if not (not (not (not (Cvar)))) then
         if A then
            Bump;
         end if;
      end if;

   end loop;

   for I in 1 .. 2 loop

      Cvar := not (I > 1);

      if not (I > 1) then
         if I > 1 then
            Bump;
         end if;
      end if;

      if Cvar then
         if I > 1 then
            Bump;
         end if;
      end if;

   end loop;

   if not True then
      Bump;
   end if;
   if not False then
      null;
   else
      Bump;
   end if;

   Tvar := Ident_Bool (True);
   Fvar := Ident_Bool (False);

   if not Tvar then
      Bump;
   end if;
   if not Fvar then
      null;
   else
      Bump;
   end if;

   if Error_Count /= 0 then
      Failed ("'NOT' TRUTH TABLE");
   end if;

   Result;

end C45662a;
