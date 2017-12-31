-- C36174A.ADA

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
-- CHECK THAT INDEX_CONSTRAINTS MAY BE OMITTED FOR CONSTANTS.

-- DAT 2/9/81
-- JBG 12/8/83

with Report;
procedure C36174a is

   use Report;

   S0 : constant String := "";
   S1 : constant String := S0;
   S2 : constant String := (1 .. 0 => 'Z');
   S3 : constant String := ('A', 'B', 'C');
   S4 : constant String := S3 & "ABC" & S3 & S2 & "Z";
   S9 : constant String := S0 & S1 & S2 & S3 (3 .. 1);

   type A4 is
     array
       (Integer range <>, Integer range <>, Integer range <>,
        Integer range <>) of String (1 .. 0);
   C4 : constant A4 :=
     (-6 .. -4 => (4 .. 5 => (-4 .. -5 => (1_000 .. 2_000 => S9))));
   S10 : constant String := (10 .. 9 => 'Q');

   type I_12 is new Integer range 10 .. 12;
   type A_12 is array (I_12 range <>, I_12 range <>) of I_12;
   A12 : constant A_12 := (11 .. 12 => (10 .. 10 => 10));
   B12 : constant A_12 :=
     (11 => (10 | 12 => 10, 11 => 11), 10 => (10 | 12 | 11 => 12));

   N6 : constant Integer := Ident_Int (6);
   S6 : constant String  := (N6 .. N6 + 6 => 'Z');
   S7 : constant String  := S6 (N6 .. N6 + Ident_Int (-1));

begin
   Test ("C36174A", "INDEX_CONSTRAINTS MAY BE OMITTED FOR CONSTANTS");

   if S0'First /= 1 or S0'Last /= 0 or S1'First /= 1 or S1'Last /= 0 or
     S2'First /= 1 or S2'Last /= 0 or S3'First /= 1 or S3'Last /= 3 then
      Failed ("INVALID STRING CONSTANT BOUNDS 1");
   end if;

   if S4'First /= 1 or S4'Last /= 10 then
      Failed ("INVALID STRING CONSTANT BOUNDS 2");
   end if;

   if S9'First /= 3 or S9'Last /= 1 then
      Failed ("INVALID STRING CONSTANT BOUNDS 3");
   end if;

   if C4'First (1) /= -6 or C4'Last (1) /= -4 or C4'First (2) /= 4 or
     C4'Last (2) /= 5 or C4'First (3) /= -4 or C4'Last (3) /= -5 or
     C4'First (4) /= 1_000 or C4'Last (4) /= 2_000 then
      Failed ("INVALID ARRAY CONSTANT BOUNDS");
   end if;

   if S10'First /= 10 or S10'Last /= 9 then
      Failed ("INVALID STRING CONSTANT BOUNDS 10");
   end if;

   if A12'First /= 11 or A12'Last /= 12 or A12'First (2) /= 10 or
     A12'Last (2) /= 10 then
      Failed ("INVALID ARRAY CONSTANT BOUNDS 2");
   end if;

   if B12'First /= 10 or B12'Last /= 11 or B12'First (2) /= 10 or
     B12'Last (2) /= 12 then
      Failed ("INVALID ARRAY CONSTANT BOUNDS 3");
   end if;

   if S6'First /= 6 or S6'Last /= 12 or S6'Length /= 7 then
      Failed ("INVALID STRING CONSTANT  BOUNDS 12");
   end if;

   if S7'First /= 6 or S7'Last /= 5 then
      Failed ("INVALID STRING CONSTANT BOUNDS 13");
   end if;

   Result;
end C36174a;
