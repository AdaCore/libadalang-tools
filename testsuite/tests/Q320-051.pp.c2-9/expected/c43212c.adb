-- C43212C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL SUBAGGREGATES FOR A PARTICULAR
-- DIMENSION DO NOT HAVE THE SAME BOUNDS. ADDITIONAL CASES FOR THE THIRD
-- DIMENSION AND FOR THE NULL ARRAYS.

-- PK  02/21/84
-- EG  05/30/84

with Report; use Report;

procedure C43212c is

   subtype Int is Integer range 1 .. 3;

begin

   Test
     ("C43212C",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL " &
      "SUBAGGREGATES FOR A PARTICULAR DIMENSION DO " &
      "NOT HAVE THE SAME BOUNDS");

   declare
      type A3 is array (Int range <>, Int range <>, Int range <>) of Integer;
   begin
      if A3'
          (((Ident_Int (1) .. Ident_Int (2) => Ident_Int (1)),
            (1 .. Ident_Int (2) => Ident_Int (1))),
           ((Ident_Int (2) .. Ident_Int (3) => Ident_Int (1)),
            (Ident_Int (2) .. Ident_Int (3) => Ident_Int (1)))) =
        A3'
          (((Ident_Int (1) .. Ident_Int (2) => Ident_Int (1)),
            (1 .. Ident_Int (2) => Ident_Int (1))),
           ((Ident_Int (2) .. Ident_Int (3) => Ident_Int (1)),
            (Ident_Int (2) .. Ident_Int (3) => Ident_Int (1))))
      then
         Failed ("A3 - EXCEPTION NOT RAISED, ARRAYS EQUAL");
      end if;
      Failed ("A3 - EXCEPTION NOT RAISED, ARRAYS NOT EQUAL");

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed ("A3 - WRONG EXCEPTION RAISED");

   end;

   declare

      type B3 is array (Int range <>, Int range <>, Int range <>) of Integer;

   begin

      if B3'
          (((Ident_Int (2) .. Ident_Int (1) => Ident_Int (1)),
            (2 .. Ident_Int (1) => Ident_Int (1))),
           ((Ident_Int (3) .. Ident_Int (1) => Ident_Int (1)),
            (Ident_Int (3) .. Ident_Int (1) => Ident_Int (1)))) =
        B3'
          (((Ident_Int (2) .. Ident_Int (1) => Ident_Int (1)),
            (2 .. Ident_Int (1) => Ident_Int (1))),
           ((Ident_Int (3) .. Ident_Int (1) => Ident_Int (1)),
            (Ident_Int (3) .. Ident_Int (1) => Ident_Int (1))))
      then
         Failed ("B3 - EXCEPTION NOT RAISED, ARRAYS EQUAL");
      end if;
      Failed ("B3 - EXCEPTION NOT RAISED, ARRAYS NOT EQUAL");

   exception

      when Constraint_Error =>
         null;
      when others =>
         Failed ("B3 - WRONG EXCEPTION RAISED");

   end;

   Result;

end C43212c;
