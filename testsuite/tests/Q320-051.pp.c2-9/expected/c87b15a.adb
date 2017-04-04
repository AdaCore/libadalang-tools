-- C87B15A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- THE ARRAY ATTRIBUTES OF THE FORM: A'FIRST (N), A'LAST (N), A'RANGE
-- (N) AND A'LENGTH (N) MUST HAVE A PARAMETER (N) WHICH IS OF THE TYPE
-- UNIVERSAL_INTEGER.

-- TRH  26 JULY 82

with Report; use Report;

procedure C87b15a is

   function "+" (X, Y : Integer) return Integer renames Standard."*";

   type Box is array (0 .. 1, 3 .. 6, 5 .. 11) of Boolean;
   B1 : Box;

begin
   Test
     ("C87B15A",
      "ARRAY ATTRIBUTES: FIRST (N), LAST (N), RANGE " &
      "(N) AND LENGTH (N) TAKE UNIVERSAL_INTEGER OPERANDS");

   if Box'First (1 + 0) /= 0 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 1");
   end if;

   if B1'First (1 + 1) /= 3 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 2");
   end if;

   if B1'First (2 + 1) /= 5 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 3");
   end if;

   if Box'Last (0 + 1) /= 1 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 4");
   end if;

   if B1'Last (1 + 1) /= 6 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 5");
   end if;

   if B1'Last (1 + 2) /= 11 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 6");
   end if;

   if Box'Length (0 + 1) /= 2 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 7");
   end if;

   if B1'Length (1 + 1) /= 4 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 8");
   end if;

   if B1'Length (2 + 1) /= 7 then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " & "UNIVERSAL_INTEGER - 9");
   end if;

   if 1 not in Box'Range (0 + 1) then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
         "UNIVERSAL_INTEGER - 10");
   end if;

   if 4 not in B1'Range (1 + 1) then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
         "UNIVERSAL_INTEGER - 11");
   end if;

   if 9 not in B1'Range (2 + 1) then
      Failed
        ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
         "UNIVERSAL_INTEGER - 12");
   end if;

   Result;
end C87b15a;
