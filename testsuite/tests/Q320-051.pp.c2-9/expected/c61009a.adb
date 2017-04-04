-- C61009A.ADA

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
-- CHECK THAT A STATIC EXPRESSION, CONSTANT NAME, ATTRIBUTE NAME,
--   VARIABLE, DEREFERENCED ACCESS, USER-DEFINED OPERATOR, USER-
--   DEFINED FUNCTION, OR ALLOCATOR CAN BE USED IN THE INITIALIZATION
--   EXPRESSION OF A FORMAL PARAMETER, AND THAT THE APPROPRIATE
--   VALUE IS USED AS A DEFAULT PARAMETER VALUE WHEN THE SUBPROGRAM
--   IS CALLED.

-- DAS  1/21/81
-- ABW  7/20/82
-- SPS  12/10/82

with Report;
procedure C61009a is

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

   -- STATIC EXPRESSION

   procedure Proc1 (Rec : Rectype1 := (3 + 4, (0, 1, 2, 3, 4, 5, 6, 7))) is
   begin
      if (Rec /= (7, (0, 1, 2, 3, 4, 5, 6, 7))) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC1 PARAMETER");
      end if;
   end Proc1;

   -- CONSTANT NAME

   procedure Proc2 (Rec : Rectype2 := (C7, (0, 1, 2, 3, 4, 5, 6, 7))) is
   begin
      if (Rec /= (C7, (0, 1, 2, 3, 4, 5, 6, 7))) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC2 PARAMETER");
      end if;
   end Proc2;

   -- ATTRIBUTE NAME

   procedure Proc3 (P1 : Int := Int'Last) is
   begin
      if (P1 /= Int (10)) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC3 PARAMETER");
      end if;
   end Proc3;

   -- VARIABLE

   procedure Proc4 (P4 : Rectype3 := (V7, (0, 1, 2, 3, 4, 5, 6, 7))) is
   begin
      if (P4 /= (V7, (0, 1, 2, 3, 4, 5, 6, 7))) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC4 PARAMETER");
      end if;
   end Proc4;

   --DEREFERENCED ACCESS

   procedure Proc5 (P5 : Integer := C_A.all) is
   begin
      if (P5 /= C_A.all) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC5 PARAMETER");
      end if;
   end Proc5;

   --USER-DEFINED OPERATOR

   procedure Proc6 (P6 : Integer := 6 & 4) is
   begin
      if (P6 /= Ident_Int (10)) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC6 PARAMETER");
      end if;
   end Proc6;

   --USER-DEFINED FUNCTION

   procedure Proc7 (P7 : Integer := Func (10)) is
   begin
      if (P7 /= Ident_Int (10)) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC7 PARAMETER");
      end if;
   end Proc7;

   -- ALLOCATOR

   procedure Proc8 (P8 : A_Int := new Integer'(7)) is
   begin
      if (P8.all /= Ident_Int (7)) then
         Failed ("INCORRECT DEFAULT VALUE FOR PROC8 PARAMETER");
      end if;
   end Proc8;

begin
   Test
     ("C61009A",
      "CHECK USE OF STATIC EXPRESSIONS, CONSTANT " &
      "NAMES, ATTRIBUTE NAMES, VARIABLES, USER- " &
      "DEFINED OPERATORS, USER-DEFINED FUNCTIONS " &
      "DEREFERENCED ACCESSES, AND ALLOCATORS IN " &
      "THE FORMAL PART OF A SUBPROGRAM SPECIFICATION");

   Proc1;
   Proc2;
   Proc3;
   Proc4;
   Proc5;
   Proc6;
   Proc7;
   Proc8;

   Result;

end C61009a;
