-- C24203A.ADA

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
-- CHECK THAT BASED INTEGER LITERALS WITH BASES 2 THROUGH 16 ALL YIELD CORRECT
-- VALUES.

-- JRK 12/12/79
-- JRK 10/27/80
-- JWC 6/28/85 RENAMED FROM C24103A.ADA

with Report;
procedure C24203a is

   use Report;

   I : Integer := 200;

begin
   Test ("C24203A", "VALUES OF BASED INTEGER LITERALS");

   if 2#11# /= 3 then
      Failed ("INCORRECT VALUE FOR BASE 2 INTEGER");
   end if;

   if 3#22# /= 8 then
      Failed ("INCORRECT VALUE FOR BASE 3 INTEGER");
   end if;

   if 4#33# /= 15 then
      Failed ("INCORRECT VALUE FOR BASE 4 INTEGER");
   end if;

   if 5#44# /= 24 then
      Failed ("INCORRECT VALUE FOR BASE 5 INTEGER");
   end if;

   if 6#55# /= 35 then
      Failed ("INCORRECT VALUE FOR BASE 6 INTEGER");
   end if;

   if 7#66# /= 48 then
      Failed ("INCORRECT VALUE FOR BASE 7 INTEGER");
   end if;

   if 8#77# /= 63 then
      Failed ("INCORRECT VALUE FOR BASE 8 INTEGER");
   end if;

   if 9#88# /= 80 then
      Failed ("INCORRECT VALUE FOR BASE 9 INTEGER");
   end if;

   if 10#99# /= 99 then
      Failed ("INCORRECT VALUE FOR BASE 10 INTEGER");
   end if;

   if 11#AA# /= 120 then
      Failed ("INCORRECT VALUE FOR BASE 11 INTEGER");
   end if;

   if 12#BB# /= 143 then
      Failed ("INCORRECT VALUE FOR BASE 12 INTEGER");
   end if;

   if 13#CC# /= 168 then
      Failed ("INCORRECT VALUE FOR BASE 13 INTEGER");
   end if;

   if 14#DD# /= 195 then
      Failed ("INCORRECT VALUE FOR BASE 14 INTEGER");
   end if;

   if 15#EE# /= 224 then
      Failed ("INCORRECT VALUE FOR BASE 15 INTEGER");
   end if;

   if 16#FF# /= 255 then
      Failed ("INCORRECT VALUE FOR BASE 16 INTEGER");
   end if;

   ----------------------------------------

   if 7#66#E1 /= 336 then
      Failed ("INCORRECT VALUE FOR BASE 7 INTEGER " & "WITH EXPONENT");
   end if;

   Result;
end C24203a;
