-- C46011A.ADA

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
-- CHECK THAT INTEGER CONVERSIONS ARE PERFORMED CORRECTLY WHEN THE TARGET AND
-- OPERAND TYPES ARE BOTH INTEGER TYPES.

-- R.WILLIAMS 9/8/86

with Report; use Report;
procedure C46011a is

   type Int1 is range -100 .. 100;
   I1 : Int1 := Int1'Val (Ident_Int (10));
   F1 : Int1 := Int1'Val (Ident_Int (-100));
   L1 : Int1 := Int1'Val (Ident_Int (100));

   type Int2 is range -100 .. 100;
   I2 : Int2 := Int2'Val (Ident_Int (10));
   F2 : Int2 := Int2'Val (Ident_Int (-100));
   L2 : Int2 := Int2'Val (Ident_Int (100));

   type Newinteger is new Integer;
   N1 : Newinteger := Newinteger'Val (Ident_Int (10));

   T1 : Integer := Ident_Int (10);

   U1 : constant := Integer'Pos (10);
begin
   Test
     ("C46011A",
      "CHECK THAT INTEGER CONVERSIONS ARE " &
      "PERFORMED CORRECTLY WHEN THE TARGET AND " &
      "OPERAND TYPES ARE BOTH INTEGER TYPES");

   if Int1 (U1) /= U1 then
      Failed ("INCORRECT CONVERSION OF 'INT1 (U1)'");
   end if;

   if Int1 (I1) /= I1 then
      Failed ("INCORRECT CONVERSION OF 'INT1 (I1)'");
   end if;

   if Int1 (N1) /= I1 then
      Failed ("INCORRECT CONVERSION OF 'INT1 (N1)'");
   end if;

   if Int1 (10) /= I1 then
      Failed ("INCORRECT CONVERSION OF 'INT1 (10)'");
   end if;

   if Int1 (T1) /= I1 then
      Failed ("INCORRECT CONVERSION OF 'INT1 (T1)'");
   end if;

   if Int1 (F2) /= F1 then
      Failed ("INCORRECT CONVERSION OF 'INT1 (F2)'");
   end if;

   if Int1 (L2) /= L1 then
      Failed ("INCORRECT CONVERSION OF 'INT1 (L2)'");
   end if;

   if Int2 (I1) /= I2 then
      Failed ("INCORRECT CONVERSION OF 'INT2 (I1)'");
   end if;

   if Int2 (T1) /= 10 then
      Failed ("INCORRECT CONVERSION OF 'INT2 (T1)'");
   end if;

   if Int2 (F1) /= -100 then
      Failed ("INCORRECT CONVERSION OF 'INT2 (F1)'");
   end if;

   if Int2 (L1) /= 100 then
      Failed ("INCORRECT CONVERSION OF 'INT2 (L1)'");
   end if;

   if Newinteger (I1) /= N1 then
      Failed ("INCORRECT CONVERSION OF 'NEWINTEGER (I1)'");
   end if;

   if Newinteger (N1) /= N1 then
      Failed ("INCORRECT CONVERSION OF 'NEWINTEGER (N1)'");
   end if;

   if Newinteger (T1) /= N1 then
      Failed ("INCORRECT CONVERSION OF 'NEWINTEGER (T1)'");
   end if;

   if Newinteger (Integer (N1)) /= N1 then
      Failed ("INCORRECT CONVERSION OF " & "'NEWINTEGER (INTEGER (N1))'");
   end if;

   if Newinteger (Integer (N1 + 1)) /= 11 then
      Failed ("INCORRECT CONVERSION OF " & "'NEWINTEGER (INTEGER (N1 + 1))'");
   end if;

   if Integer (10) /= T1 then
      Failed ("INCORRECT CONVERSION OF 'INTEGER (10)'");
   end if;

   if Integer (N1) /= 10 then
      Failed ("INCORRECT CONVERSION OF 'INTEGER (N1)'");
   end if;

   if Integer (I1) /= T1 then
      Failed ("INCORRECT CONVERSION OF 'INTEGER (I1)'");
   end if;

   if Integer (Int1 (Newinteger (Int1 (I1)))) /= T1 then
      Failed
        ("INCORRECT CONVERSION OF " &
         "'INTEGER (INT1 (NEWINTEGER (INT1 (I1)))'");
   end if;

   if Integer (I1 + 1) /= 11 then
      Failed ("INCORRECT CONVERSION OF 'INTEGER (I1 + 1)'");
   end if;

   Result;
end C46011a;
