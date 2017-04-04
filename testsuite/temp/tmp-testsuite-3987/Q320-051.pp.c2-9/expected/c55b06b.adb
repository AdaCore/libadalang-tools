-- C55B06B.ADA

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
-- CHECK THAT LOOPS MAY BE SPECIFIED FOR DERIVED BOOLEAN AND
-- DERIVED DERIVED BOOLEAN.

-- DAT 3/26/81
-- SPS 3/2/83

with Report; use Report;

procedure C55b06b is

   type E is (False, True);
   type B1 is new Boolean;
   type B2 is new B1;
   type B3 is new E;

   One       : Integer := Ident_Int (1);
   Count     : Integer := 0;
   Old_Count : Integer := 0;

   procedure Q is
   begin
      Count := Count + 1;
   end Q;

begin
   Test ("C55B06B", "LOOPS OVER DERIVED BOOLEAN");

   for I in Boolean loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 1");
   else
      Old_Count := Count;
   end if;

   for I in Boolean range False .. True loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 2");
   else
      Old_Count := Count;
   end if;

   for I in Boolean'(False) .. True loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 3");
   else
      Old_Count := Count;
   end if;

   for I in E loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 4");
   else
      Old_Count := Count;
   end if;

   for I in E range False .. True loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 5");
   else
      Old_Count := Count;
   end if;

   for I in False .. E'(True) loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 6");
   else
      Old_Count := Count;
   end if;

   for I in B1 loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 7");
   else
      Old_Count := Count;
   end if;

   for I in B1 range False .. True loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 8");
   else
      Old_Count := Count;
   end if;

   for I in False .. B1'(True) loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 9");
   else
      Old_Count := Count;
   end if;

   for I in B2 loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 10");
   else
      Old_Count := Count;
   end if;

   for I in B2 range False .. True loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 11");
   else
      Old_Count := Count;
   end if;

   for I in B2'(False) .. True loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 12");
   else
      Old_Count := Count;
   end if;

   for I in B3 loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 13");
   else
      Old_Count := Count;
   end if;

   for I in B3 range False .. True loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 14");
   else
      Old_Count := Count;
   end if;

   for I in False .. B3'(True) loop
      Q;
   end loop;
   if Old_Count + Ident_Int (2) /= Count then
      Failed ("LOOP 15");
   else
      Old_Count := Count;
   end if;

   Result;
end C55b06b;
