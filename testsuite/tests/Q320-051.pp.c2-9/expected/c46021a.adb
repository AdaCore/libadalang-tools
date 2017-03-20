-- C46021A.ADA

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
--     CHECK THAT FLOATING POINT CONVERSIONS ARE PERFORMED CORRECTLY
--     WHEN THE OPERAND TYPE IS AN INTEGER TYPE, FOR 5-DIGIT PRECISION.

-- HISTORY:
--     JET 02/12/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C46021a is

   type Float5 is digits 5;
   type Int is range -32_768 .. 32_767;

   type Nfloat5 is new Float5;

   function Ident (A : Float5) return Float5 is
   begin
      if Equal (3, 3) then
         return A;
      else
         return 0.0;
      end if;
   end Ident;

   function Ident (A : Nfloat5) return Nfloat5 is
   begin
      if Equal (3, 3) then
         return A;
      else
         return 0.0;
      end if;
   end Ident;

begin
   Test
     ("C46021A",
      "CHECK THAT FLOATING POINT CONVERSIONS ARE " &
      "PERFORMED CORRECTLY WHEN THE OPERAND TYPE " &
      "IS AN INTEGER TYPE, FOR 5-DIGIT PRECISION");

   if Float5 (Ident_Int (-7)) /= -7.0 then
      Failed ("INCORRECT VALUE (1)");
   end if;

   if Float5 (Ident_Int (3)) /= 3.0 then
      Failed ("INCORRECT VALUE (2)");
   end if;

   if Float5 (Ident_Int (-999)) /= -999.0 then
      Failed ("INCORRECT VALUE (3)");
   end if;

   if Float5 (Ident_Int (101)) /= 101.0 then
      Failed ("INCORRECT VALUE (4)");
   end if;

   if Float5 (Ident_Int (-32_767)) /= -32_767.0 then
      Failed ("INCORRECT VALUE (5)");
   end if;

   if Float5 (Ident_Int (32_767)) /= 32_767.0 then
      Failed ("INCORRECT VALUE (6)");
   end if;

   if Float5 (-7) /= Ident (-7.0) then
      Failed ("INCORRECT VALUE (7)");
   end if;

   if Float5 (3) /= Ident (3.0) then
      Failed ("INCORRECT VALUE (8)");
   end if;

   if Float5 (-999) /= Ident (-999.0) then
      Failed ("INCORRECT VALUE (9)");
   end if;

   if Float5 (101) /= Ident (101.0) then
      Failed ("INCORRECT VALUE (10)");
   end if;

   if Float5 (-32_767) /= Ident (-32_767.0) then
      Failed ("INCORRECT VALUE (11)");
   end if;

   if Float5 (32_767) /= Ident (32_767.0) then
      Failed ("INCORRECT VALUE (12)");
   end if;

   if Float5 (Int'(-7)) /= Ident (-7.0) then
      Failed ("INCORRECT VALUE (13)");
   end if;

   if Float5 (Int'(3)) /= Ident (3.0) then
      Failed ("INCORRECT VALUE (14)");
   end if;

   if Float5 (Int'(-999)) /= Ident (-999.0) then
      Failed ("INCORRECT VALUE (15)");
   end if;

   if Float5 (Int'(101)) /= Ident (101.0) then
      Failed ("INCORRECT VALUE (16)");
   end if;

   if Float5 (Int'(-32_767)) /= Ident (-32_767.0) then
      Failed ("INCORRECT VALUE (17)");
   end if;

   if Float5 (Int'(32_767)) /= Ident (32_767.0) then
      Failed ("INCORRECT VALUE (18)");
   end if;

   if Nfloat5 (Ident_Int (-7)) /= -7.0 then
      Failed ("INCORRECT VALUE (19)");
   end if;

   if Nfloat5 (Ident_Int (3)) /= 3.0 then
      Failed ("INCORRECT VALUE (20)");
   end if;

   if Nfloat5 (Ident_Int (-999)) /= -999.0 then
      Failed ("INCORRECT VALUE (21)");
   end if;

   if Nfloat5 (Ident_Int (101)) /= 101.0 then
      Failed ("INCORRECT VALUE (22)");
   end if;

   if Nfloat5 (Ident_Int (-32_767)) /= -32_767.0 then
      Failed ("INCORRECT VALUE (23)");
   end if;

   if Nfloat5 (Ident_Int (32_767)) /= 32_767.0 then
      Failed ("INCORRECT VALUE (24)");
   end if;

   if Nfloat5 (-7) /= Ident (-7.0) then
      Failed ("INCORRECT VALUE (25)");
   end if;

   if Nfloat5 (3) /= Ident (3.0) then
      Failed ("INCORRECT VALUE (26)");
   end if;

   if Nfloat5 (-999) /= Ident (-999.0) then
      Failed ("INCORRECT VALUE (27)");
   end if;

   if Nfloat5 (101) /= Ident (101.0) then
      Failed ("INCORRECT VALUE (28)");
   end if;

   if Nfloat5 (-32_767) /= Ident (-32_767.0) then
      Failed ("INCORRECT VALUE (29)");
   end if;

   if Nfloat5 (32_767) /= Ident (32_767.0) then
      Failed ("INCORRECT VALUE (30)");
   end if;

   if Nfloat5 (Int'(-7)) /= Ident (-7.0) then
      Failed ("INCORRECT VALUE (31)");
   end if;

   if Nfloat5 (Int'(3)) /= Ident (3.0) then
      Failed ("INCORRECT VALUE (32)");
   end if;

   if Nfloat5 (Int'(-999)) /= Ident (-999.0) then
      Failed ("INCORRECT VALUE (33)");
   end if;

   if Nfloat5 (Int'(101)) /= Ident (101.0) then
      Failed ("INCORRECT VALUE (34)");
   end if;

   if Nfloat5 (Int'(-32_767)) /= Ident (-32_767.0) then
      Failed ("INCORRECT VALUE (35)");
   end if;

   if Nfloat5 (Int'(32_767)) /= Ident (32_767.0) then
      Failed ("INCORRECT VALUE (36)");
   end if;

   Result;

end C46021a;
