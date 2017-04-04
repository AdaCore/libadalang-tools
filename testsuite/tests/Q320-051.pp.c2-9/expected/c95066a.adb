-- C95066A.ADA

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
-- CHECK THAT A STATIC EXPRESSION, CONSTANT NAME, ATTRIBUTE NAME, VARIABLE,
-- DEREFERENCED ACCESS, USER-DEFINED OPERATOR, USER- DEFINED FUNCTION,
-- OR ALLOCATOR CAN BE USED IN THE INITIALIZATION EXPRESSION OF A FORMAL
-- PARAMETER, AND THAT THE APPROPRIATE VALUE IS USED AS A DEFAULT PARAMETER
-- VALUE WHEN THE ENTRY IS CALLED.

-- GLH  6/19/85

with Report;
procedure C95066a is

   use Report;

   type Int is range 1 .. 10;

   type Arr is array (Integer range <>) of Integer;

   type Rectype (Constraint : Integer) is record
      A : Arr (0 .. Constraint);
   end record;

   C7 : constant Integer := 7;
   V7 : Integer          := 7;

   type A_Int is access Integer;
   C_A : constant A_Int := new Integer'(7);

   subtype Rectype1 is Rectype (2 + 5);
   subtype Rectype2 is Rectype (C7);
   subtype Rectype3 is Rectype (V7);

   function "&" (X, Y : Integer) return Integer is
   begin
      return 10;
   end "&";

   function Func (X : Integer) return Integer is
   begin
      return X;
   end Func;

   -- STATIC EXPRESSION.

   task T1 is
      entry E1 (Rec : Rectype1 := (3 + 4, (0, 1, 2, 3, 4, 5, 6, 7)));
   end T1;

   task body T1 is
   begin
      accept E1 (Rec : Rectype1 := (3 + 4, (0, 1, 2, 3, 4, 5, 6, 7))) do
         if (Rec /= (7, (0, 1, 2, 3, 4, 5, 6, 7))) then
            Failed ("INCORRECT DEFAULT VALUE FOR " & "E1 PARAMETER");
         end if;
      end E1;
   end T1;

   -- CONSTANT NAME.

   task T2 is
      entry E2 (Rec : Rectype2 := (C7, (0, 1, 2, 3, 4, 5, 6, 7)));
   end T2;

   task body T2 is
   begin
      accept E2 (Rec : Rectype2 := (C7, (0, 1, 2, 3, 4, 5, 6, 7))) do
         if (Rec /= (C7, (0, 1, 2, 3, 4, 5, 6, 7))) then
            Failed ("INCORRECT DEFAULT VALUE FOR " & "E2 PARAMETER");
         end if;
      end E2;
   end T2;

   -- ATTRIBUTE NAME.

   task T3 is
      entry E3 (P1 : Int := Int'Last);
   end T3;

   task body T3 is
   begin
      accept E3 (P1 : Int := Int'Last) do
         if (P1 /= Int (10)) then
            Failed ("INCORRECT DEFAULT VALUE FOR " & "E3 PARAMETER");
         end if;
      end E3;
   end T3;

   -- VARIABLE.

   task T4 is
      entry E4 (P4 : Rectype3 := (V7, (0, 1, 2, 3, 4, 5, 6, 7)));
   end T4;

   task body T4 is
   begin
      accept E4 (P4 : Rectype3 := (V7, (0, 1, 2, 3, 4, 5, 6, 7))) do
         if (P4 /= (V7, (0, 1, 2, 3, 4, 5, 6, 7))) then
            Failed ("INCORRECT DEFAULT VALUE FOR " & "E4 PARAMETER");
         end if;
      end E4;
   end T4;

   -- DEREFERENCED ACCESS.

   task T5 is
      entry E5 (P5 : Integer := C_A.all);
   end T5;

   task body T5 is
   begin
      accept E5 (P5 : Integer := C_A.all) do
         if (P5 /= C_A.all) then
            Failed ("INCORRECT DEFAULT VALUE FOR " & "E5 PARAMETER");
         end if;
      end E5;
   end T5;

   -- USER-DEFINED OPERATOR.

   task T6 is
      entry E6 (P6 : Integer := 6 & 4);
   end T6;

   task body T6 is
   begin
      accept E6 (P6 : Integer := 6 & 4) do
         if (P6 /= Ident_Int (10)) then
            Failed ("INCORRECT DEFAULT VALUE " & "FOR E6 PARAMETER");
         end if;
      end E6;
   end T6;

   -- USER-DEFINED FUNCTION.

   task T7 is
      entry E7 (P7 : Integer := Func (10));
   end T7;

   task body T7 is
   begin
      accept E7 (P7 : Integer := Func (10)) do
         if (P7 /= Ident_Int (10)) then
            Failed ("INCORRECT DEFAULT VALUE FOR " & "E7 PARAMETER");
         end if;
      end E7;
   end T7;

   -- ALLOCATOR.

   task T8 is
      entry E8 (P8 : A_Int := new Integer'(7));
   end T8;

   task body T8 is
   begin
      accept E8 (P8 : A_Int := new Integer'(7)) do
         if (P8.all /= Ident_Int (7)) then
            Failed ("INCORRECT DEFAULT VALUE " & "FOR E8 PARAMETER");
         end if;
      end E8;
   end T8;

begin
   Test
     ("C95066A",
      "CHECK USE OF STATIC EXPRESSIONS, CONSTANT " &
      "NAMES, ATTRIBUTE NAMES, VARIABLES, USER- " &
      "DEFINED OPERATORS, USER-DEFINED FUNCTIONS, " &
      "DEREFERENCED ACCESSES, AND ALLOCATORS IN " &
      "THE FORMAL PART OF A TASK SPECIFICATION");

   T1.E1;
   T2.E2;
   T3.E3;
   T4.E4;
   T5.E5;
   T6.E6;
   T7.E7;
   T8.E8;

   Result;

end C95066a;
