-- C87B07B.ADA

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
-- FOR THE ATTRIBUTE OF THE FORM T'VAL (X), THE OPERAND X MAY
-- BE OF ANY INTEGER TYPE. THE RESULT IS OF TYPE T.

-- TRH  15 SEPT 82
-- DSJ  06 JUNE 83
-- PWN  01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;

procedure C87b07b is

   type New_Int is new Integer;
   type Whole is new Integer range 0 .. Integer'Last;
   type Flag is (Pass, Fail);

   function "+" (X, Y : New_Int) return New_Int renames "-";
   function "+" (X, Y : Whole) return Whole renames "*";

   generic
      type T is private;
      Arg : in T;
      Stat : in Flag;
   function F1 return T;

   function F1 return T is
   begin
      if Stat = Fail then
         Failed
           ("THE 'VAL' ATTRIBUTE TAKES AN OPERAND " & "OF AN INTEGER TYPE");
      end if;
      return Arg;
   end F1;

   function F is new F1 (Character, '1', Fail);
   function F is new F1 (Duration, 1.0, Fail);
   function F is new F1 (Float, 1.0, Fail);
   function F is new F1 (New_Int, 1, Pass);

begin
   Test ("C87B07B", "OVERLOADED OPERANDS TO THE 'VAL' ATTRIBUTE");

   if (Integer'Val (F) /= 1) then
      Failed
        ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
         "MUST RETURN A VALUE OF TYPE T - 1");
   end if;

   if (Integer'Val (3 + 3) + 1 /= 7) then
      Failed
        ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
         "MUST RETURN A VALUE OF TYPE T - 2");
   end if;

   if (New_Int'Val (F) /= 1) then
      Failed
        ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
         "MUST RETURN A VALUE OF TYPE T - 3");
   end if;

   if (New_Int'Val (3 + 3) + 1 /= 5) then
      Failed
        ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
         "MUST RETURN A VALUE OF TYPE T - 4");
   end if;

   if (Whole'Val (F) /= 1) then
      Failed
        ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
         "MUST RETURN A VALUE OF TYPE T - 5");
   end if;

   if (Whole'Val (3 + 3) + 1 /= 6) then
      Failed
        ("RESOLUTION INCORRECT - THE 'VAL' ATTRIBUTE " &
         "MUST RETURN A VALUE OF TYPE T - 6");
   end if;

   Result;
end C87b07b;
