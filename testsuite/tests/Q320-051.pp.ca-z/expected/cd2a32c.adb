-- CD2A32C.ADA

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
--     CHECK THAT A SIZE SPECIFICATION FOR AN INTEGER TYPE OF THE
--     SMALLEST APPROPRIATE SIGNED SIZE CAN BE GIVEN:
--        IN THE VISIBLE OR PRIVATE PART OF A PACKAGE FOR A TYPE
--          DECLARED IN THE VISIBLE PART;
--        FOR A DERIVED INTEGER TYPE;
--        FOR A DERIVED PRIVATE TYPE WHOSE FULL DECLARATION IS AS
--          AN INTEGER TYPE;
--        FOR AN INTEGER TYPE IN A GENERIC UNIT.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, CHANGED OPERATOR ON 'SIZE
--                   CHECKS, ADDED REPRESENTAION CLAUSE CHECK, AND
--                   ADDED CHECK ON INTEGER IN A GENERIC UNIT.
--     BCB 10/03/90  CHANGED FAILED MESSAGES FROM "SHOULD NOT BE GREATER
--                   THAN" TO "MUST BE EQUAL TO".
--     JRL 03/27/92  REMOVED TESTING OF NONOBJECTIVE TYPES.

with Report; use Report;
procedure Cd2a32c is

   type Basic_Int is range -63 .. 63;
   Specified_Size : constant := 7;

   type Derived_Int is new Basic_Int;
   for Derived_Int'Size use Specified_Size;

   package P is
      type Int_In_P is range -63 .. 63;
      for Int_In_P'Size use Specified_Size;
      type Private_Int is private;
      type Alt_Int_In_P is range -63 .. 63;
   private
      type Private_Int is range -63 .. 63;
      for Alt_Int_In_P'Size use Specified_Size;
   end P;

   use P;

   generic
   package Genpack is
      type Gen_Check_Int is range -63 .. 63;
      for Gen_Check_Int'Size use Specified_Size;
   end Genpack;

   package Newpack is new Genpack;

   use Newpack;
   type Derived_Private_Int is new Private_Int;
   for Derived_Private_Int'Size use Specified_Size;

   Minimum_Size : Integer := Ident_Int (Specified_Size);

begin

   Test
     ("CD2A32C",
      "CHECK THAT A SIZE SPECIFICATION " &
      "FOR AN INTEGER TYPE OF THE SMALLEST " &
      "APPROPRIATE SIGNED SIZE CAN BE GIVEN: IN THE " &
      "VISIBLE OR PRIVATE PART OF A PACKAGE FOR A " &
      "TYPE DECLARED IN THE VISIBLE PART; FOR A " &
      "DERIVED INTEGER TYPE; FOR A DERIVED PRIVATE " &
      "TYPE WHOSE FULL DECLARATION IS AS AN INTEGER " &
      "TYPE; FOR AN INTEGER TYPE IN A GENERIC UNIT");

   if Derived_Int'Size /= Minimum_Size then
      Failed
        ("DERIVED_INT'SIZE MUST BE EQUAL TO" &
         Integer'Image (Minimum_Size) &
         ".  ACTUAL SIZE IS" &
         Integer'Image (Derived_Int'Size));
   end if;

   if Int_In_P'Size /= Minimum_Size then
      Failed
        ("INT_IN_P'SIZE MUST BE EQUAL TO" &
         Integer'Image (Minimum_Size) &
         ".  ACTUAL SIZE IS" &
         Integer'Image (Int_In_P'Size));
   end if;

   if Alt_Int_In_P'Size /= Minimum_Size then
      Failed
        ("ALT_INT_IN_P'SIZE MUST BE EQUAL TO" &
         Integer'Image (Minimum_Size) &
         ".  ACTUAL SIZE IS" &
         Integer'Image (Alt_Int_In_P'Size));
   end if;

   if Derived_Private_Int'Size /= Minimum_Size then
      Failed
        ("DERIVED_PRIVATE_INT'SIZE MUST BE EQUAL TO " &
         Integer'Image (Minimum_Size) &
         ".  ACTUAL SIZE IS" &
         Integer'Image (Derived_Private_Int'Size));
   end if;

   if Gen_Check_Int'Size /= Minimum_Size then
      Failed
        ("GEN_CHECK_INT'SIZE MUST BE EQUAL TO" &
         Integer'Image (Minimum_Size) &
         ".  ACTUAL SIZE IS" &
         Integer'Image (Gen_Check_Int'Size));
   end if;

   Result;

end Cd2a32c;
