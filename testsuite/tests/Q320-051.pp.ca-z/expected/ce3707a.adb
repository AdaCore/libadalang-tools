-- CE3707A.ADA

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
--     CHECK THAT INTEGER_IO GET CAN READ A VALUE FROM A STRING.  CHECK
--     THAT IT TREATS THE END OF THE STRING AS A FILE TERMINATOR.  CHECK
--     THAT LAST CONTAINS THE INDEX VALUE OF THE LAST CHARACTER READ
--     FROM THE STRING.

-- HISTORY:
--     SPS 10/05/82
--     VKG 01/13/83
--     JLH 09/11/87  CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3707a is

   package Iio is new Integer_Io (Integer);
   use Iio;
   X   : Integer;
   L   : Positive;
   Str : String (1 .. 6) := "123456";

begin

   Test
     ("CE3707A",
      "CHECK THAT INTEGER_IO GET OPERATES CORRECTLY " & "ON STRINGS");

-- LEFT JUSTIFIED STRING NON NULL

   Get ("2362  ", X, L);
   if X /= 2_362 then
      Failed ("VALUE FROM STRING INCORRECT - 1");
   end if;

   if L /= 4 then
      Failed ("VALUE OF LAST INCORRECT - 1");
   end if;

-- STRING LITERAL WITH BLANKS

   begin
      Get ("  ", X, L);
      Failed ("END_ERROR NOT RAISED - 2");
   exception
      when End_Error =>
         if L /= 4 then
            Failed ("AFTER END ERROR VALUE OF LAST " & "INCORRECT - 2");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 2");
   end;

-- NULL STRING

   begin
      Get ("", X, L);
      Failed (" END_ERROR NOT RAISED - 3");
   exception
      when End_Error =>
         if L /= 4 then
            Failed ("AFTER END_ERROR VALUE OF LAST " & "INCORRECT - 3");
         end if;
      when others =>
         Failed ("SOME EXCEPTION RAISED - 3");
   end;

-- NULL SLICE

   begin
      Get (Str (5 .. Ident_Int (2)), X, L);
      Failed ("END_ERROR NOT RAISED - 4");
   exception
      when End_Error =>
         if L /= 4 then
            Failed ("AFTER END_ERROR VALUE OF LAST " & "INCORRECT - 4");
         end if;
      when others =>
         Failed ("SOME EXCEPTION RAISED - 4");
   end;

-- NON-NULL SLICE

   Get (Str (2 .. 3), X, L);
   if X /= 23 then
      Failed ("INTEGER VALUE INCORRECT - 5");
   end if;
   if L /= 3 then
      Failed ("LAST INCORRECT FOR SLICE - 5");
   end if;

-- RIGHT JUSTIFIED NEGATIVE NUMBER

   Get ("   -2345", X, L);
   if X /= -2_345 then
      Failed ("INTEGER VALUE INCORRECT - 6");
   end if;
   if L /= 8 then
      Failed ("LAST INCORRECT FOR NEGATIVE NUMBER - 6");
   end if;

   Result;

end Ce3707a;
