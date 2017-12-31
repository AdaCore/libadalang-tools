-- CC1221B.ADA

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
--     FOR A FORMAL INTEGER TYPE, CHECK THAT THE FOLLOWING BASIC
--     OPERATIONS ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE
--     WITHIN THE GENERIC UNIT:  ATTRIBUTES 'FIRST, 'LAST, 'WIDTH,
--     'ADDRESS, AND 'SIZE.

-- HISTORY:
--     BCB 11/12/87  CREATED ORIGINAL TEST FROM SPLIT OF CC1221A.ADA.

with System; use System;
with Report; use Report;
procedure Cc1221b is

   subtype Subint is Integer range -100 .. 100;
   subtype Noint is Integer range 1 .. -1;
   type Newint is new Integer;
   type Int is range -300 .. 300;
   subtype Sint1 is Int range Int (Ident_Int (-4)) .. Int (Ident_Int (4));
   subtype Sint2 is Int range 16#E#E1 .. 2#1111_1111#;
   type Int2 is range 0E8 .. 1E3;

begin
   Test
     ("CC1221B",
      "FOR A FORMAL INTEGER TYPE, CHECK THAT THE " &
      "FOLLOWING BASIC OPERATIONS ARE IMPLICITLY " &
      "DECLARED AND ARE THEREFORE AVAILABLE " &
      "WITHIN THE GENERIC UNIT:  ATTRIBUTES 'FIRST, " &
      "'LAST, 'WIDTH, 'ADDRESS, AND 'SIZE");

   declare -- (B) CHECKS FOR BASIC OPERATIONS OF A DISCRETE TYPE.
      --     PART II.

      generic
         type T is range <>;
         F, L : T;
         W : Integer;
      procedure P (Str : String);

      procedure P (Str : String) is
         I  : Integer := F'Size;
         T1 : T;
         A  : Address := T1'Address;

      begin
         if T'First /= F then
            Failed ("INCORRECT VALUE FOR " & Str & "'FIRST");
         end if;

         if T'Last /= L then
            Failed ("INCORRECT VALUE FOR " & Str & "'LAST");
         end if;

         if T'Base'First > T'First then
            Failed ("INCORRECT RESULTS WITH " & Str & "'BASE'FIRST");
         end if;

         if T'Base'Last < T'Last then
            Failed ("INCORRECT RESULTS WITH " & Str & "'BASE'LAST");
         end if;

         if T'Width /= W then
            Failed ("INCORRECT VALUE FOR " & Str & "'WIDTH");
         end if;

         if T'Base'Width < T'Width then
            Failed ("INCORRECT RESULTS WITH " & Str & "'BASE'WIDTH");
         end if;

      end P;

      generic
         type T is range <>;
      procedure Q;

      procedure Q is
      begin
         if T'First /= 1 then
            Failed ("INCORRECT VALUE FOR NOINT'FIRST");
         end if;

         if T'Last /= -1 then
            Failed ("INCORRECT VALUE FOR NOINT'LAST");
         end if;

         if T'Base'First > T'First then
            Failed ("INCORRECT RESULTS WITH " & "NOINT'BASE'FIRST");
         end if;

         if T'Base'Last < T'Last then
            Failed ("INCORRECT RESULTS WITH " & "NOINT'BASE'LAST");
         end if;

         if T'Width /= 0 then
            Failed ("INCORRECT VALUE FOR " & "NOINT'WIDTH");
         end if;

         if T'Base'Width < T'Width then
            Failed ("INCORRECT RESULTS WITH " & "NOINT'BASE'WIDTH");
         end if;

      end Q;

      procedure P1 is new P (Integer, Integer'First, Integer'Last,
         Integer'Width);
      procedure P2 is new P (Subint, -100, 100, 4);
      procedure P3 is new P (Newint, Newint'First, Newint'Last, Newint'Width);
      procedure P4 is new P (Sint1, -4, 4, 2);
      procedure P5 is new P (Sint2, 224, 255, 4);
      procedure P6 is new P (Int2, 0, 1_000, 5);

      procedure Q1 is new Q (Noint);

   begin
      P1 ("INTEGER");
      P2 ("SUBINT");
      P3 ("NEWINT");
      P4 ("SINT1");
      P5 ("SINT2");
      P6 ("INT2");

      Q1;

   end; -- (B).

   Result;
end Cc1221b;
