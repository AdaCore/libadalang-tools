-- C41207A.ADA

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
--     CHECK THAT THE DISCRETE RANGE IN A SLICE CAN HAVE THE FORM
--     A'RANGE, WHERE A IS A CONSTRAINED ARRAY SUBTYPE OR AN ARRAY
--     OBJECT.

-- HISTORY:
--     BCB 07/13/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C41207a is

   type Arr is array (Integer range <>) of Integer;

   subtype A1 is Arr (1 .. 5);

   Arr_Var : Arr (1 .. 10) := (90, 91, 92, 93, 94, 95, 96, 97, 98, 99);

   A2 : array (1 .. 5) of Integer := (80, 81, 82, 83, 84);

begin
   Test
     ("C41207A",
      "CHECK THAT THE DISCRETE RANGE IN A SLICE CAN " &
      "HAVE THE FORM A'RANGE, WHERE A IS A " &
      "CONSTRAINED ARRAY SUBTYPE OR AN ARRAY OBJECT");

   Arr_Var (A1'Range) := (1, 2, 3, 4, 5);

   if not
     (Equal (Arr_Var (1), 1) and
      Equal (Arr_Var (2), 2) and
      Equal (Arr_Var (3), 3) and
      Equal (Arr_Var (4), 4) and
      Equal (Arr_Var (5), 5))
   then
      Failed
        ("IMPROPER RESULT FROM SLICE ASSIGNMENT USING THE " &
         "RANGE OF A CONSTRAINED ARRAY SUBTYPE");
   end if;

   Arr_Var (A2'Range) := (6, 7, 8, 9, 10);

   if
     (not Equal (Arr_Var (1), 6) or
      not Equal (Arr_Var (2), 7) or
      not Equal (Arr_Var (3), 8) or
      not Equal (Arr_Var (4), 9) or
      not Equal (Arr_Var (5), 10))
   then
      Failed
        ("IMPROPER RESULT FROM SLICE ASSIGNMENT USING THE " &
         "RANGE OF AN ARRAY OBJECT");
   end if;

   Result;
end C41207a;
