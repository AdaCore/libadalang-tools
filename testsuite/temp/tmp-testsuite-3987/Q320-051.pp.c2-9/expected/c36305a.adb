-- C36305A.ADA

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
-- CHECK THAT A STRING VARIABLE IS CONSIDERED AN ARRAY.

-- DAT 2/17/81
-- SPS 10/25/82
-- EDS 07/16/98    AVOID OPTIMIZATION

with Report;
procedure C36305a is

   use Report;

   S : String (Ident_Int (5) .. Ident_Int (10));
   T : String (S'Range);
   U : String (T'First .. T'Last);
   subtype I_5 is Integer range U'Range (1);
   I5 : I_5;
   C  : constant String := "ABCDEF";

begin
   Test ("C36305A", "CHECK THAT STRINGS ARE REALLY ARRAYS");

   if S'First /= 5 or
     S'Last /= 10 or
     S'Length /= 6 or
     U'First (1) /= 5 or
     U'Last (1) /= 10 or
     U'Length (1) /= 6
   then
      Failed ("INCORRECT STRING ATTRIBUTE VALUES");
   end if;

   if 4 in U'Range or
     3 in U'Range (1) or
     0 in U'Range or
     1 in U'Range or
     5 not in U'Range or
     7 not in U'Range or
     10 not in U'Range or
     not (11 not in U'Range)
   then
      Failed ("INCORRECT STRING RANGE ATTRIBUTE");
   end if;

   begin
      begin
         begin
            I5 := 4;
            Failed ("BAD I5 SUBRANGE 1 " & Integer'Image (I5)); --use I5
         exception
            when Constraint_Error =>
               null;
         end;
         I5 := Integer'(11);
         Failed ("BAD I5 SUBRANGE 2 " & Integer'Image (I5)); --use I5
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED 1");
      end;
      I5 := Integer'(5);
      I5 := I5 + I5;
      I5 := Natural'(8);
   exception
      when others =>
         Failed ("WRONG EXCEPTION RAISED 2");
   end;

   for I in S'Range loop
      S (I) := C (11 - I);
   end loop;
   T := S;
   for I in reverse U'Range loop
      U (I) := T (15 - I);
   end loop;

   for I in 1 .. C'Length loop
      if C (1 .. I) /= U (5 .. I + 4) or
        U (I + 4 .. U'Last) /= C (I .. C'Last) or
        C (I) /= U (I + 4) or
        C (I .. I) (I .. I) (I) /= U (U'Range) (I + 4)
      then
         Failed ("INCORRECT CHARACTER MISMATCH IN STRING");
         exit;
      end if;
   end loop;

   if U /= C or
     U /= "ABCDEF" or
     U (U'Range) /= C (C'Range) or
     U (5 .. 10) /= C (1 .. 6) or
     U (5 .. 6) /= C (1 .. 2)
   then
      Failed ("STRINGS AS ARRAYS BEHAVE INCORRECTLY");
   end if;

   Result;
end C36305a;
