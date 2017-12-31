-- CD2A32G.ADA

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
--     CHECK THAT A SIZE SPECIFICATION FOR AN INTEGER
--     TYPE OF THE SMALLEST APPROPRIATE UNSIGNED SIZE CAN BE GIVEN:
--        IN THE VISIBLE OR PRIVATE PART OF A PACKAGE FOR A TYPE
--          DECLARED IN THE VISIBLE PART;
--        FOR A DERIVED INTEGER TYPE;
--        FOR A DERIVED PRIVATE TYPE WHOSE FULL DECLARATION IS AS
--          AN INTEGER TYPE;
--        FOR AN INTEGER TYPE GIVEN IN A GENERIC UNIT.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, CHANGED OPERATOR ON 'SIZE
--                   CHECKS, AND ADDED CHECK FOR 'SIZE IN A GENERIC
--                   UNIT.
--     JRL 03/27/92  REMOVED TESTING OF NONOBJECTIVE TYPES.

with Report; use Report;
procedure Cd2a32g is

   type Basic_Int is range 0 .. 126;
   Specified_Size : constant := 7;

   type Derived_Int is new Basic_Int;
   for Derived_Int'Size use Specified_Size;

   package P is
      type Int_In_P is range 0 .. 126;
      for Int_In_P'Size use Specified_Size;
      type Private_Int is private;
      type Alt_Int_In_P is range 0 .. 126;
   private
      type Private_Int is range 0 .. 126;
      for Alt_Int_In_P'Size use Specified_Size;
   end P;

   use P;

   type Derived_Private_Int is new Private_Int;
   for Derived_Private_Int'Size use Specified_Size;

   Minimum_Size : Integer := Ident_Int (Specified_Size);

   generic
   procedure Genproc;

   procedure Genproc is
      type Gen_Check_Int is range 0 .. 126;
      for Gen_Check_Int'Size use Specified_Size;

   begin

      if Gen_Check_Int'Size /= Minimum_Size then
         Failed
           ("GEN_CHECK_INT'SIZE SHOULD NOT BE GREATER " & "THAN" &
            Integer'Image (Minimum_Size) & ".  ACTUAL SIZE IS" &
            Integer'Image (Gen_Check_Int'Size));
      end if;
   end Genproc;

   procedure Newproc is new Genproc;

begin

   Test
     ("CD2A32G",
      "CHECK THAT SIZE SPECIFICATIONS OF THE SMALLEST " &
      "APPROPRIATE UNSIGNED SIZE CAN BE GIVEN " &
      "IN THE VISIBLE OR PRIVATE PART OF PACKAGE FOR " &
      "AN INTEGER TYPE DECLARED IN VISIBLE PART, " & "FOR DERIVED INTEGER " &
      "TYPES AND DERIVED PRIVATE TYPES WHOSE FULL " &
      "DECLARATION IS AS AN INTEGER TYPE AND FOR AN " &
      "INTEGER TYPE GIVEN IN A GENERIC UNIT");

   if Derived_Int'Size /= Minimum_Size then
      Failed
        ("DERIVED_INT'SIZE SHOULD NOT BE GREATER THAN" &
         Integer'Image (Minimum_Size) & ".  ACTUAL SIZE IS" &
         Integer'Image (Derived_Int'Size));
   end if;

   if Int_In_P'Size /= Minimum_Size then
      Failed
        ("INT_IN_P'SIZE SHOULD NOT BE GREATER THAN" &
         Integer'Image (Minimum_Size) & ".  ACTUAL SIZE IS" &
         Integer'Image (Int_In_P'Size));
   end if;

   if Alt_Int_In_P'Size /= Minimum_Size then
      Failed
        ("ALT_INT_IN_P'SIZE SHOULD NOT BE GREATER THAN" &
         Integer'Image (Minimum_Size) & ".  ACTUAL SIZE IS" &
         Integer'Image (Alt_Int_In_P'Size));
   end if;

   if Derived_Private_Int'Size /= Minimum_Size then
      Failed
        ("DERIVED_PRIVATE_INT'SIZE SHOULD NOT BE GREATER " & "THAN" &
         Integer'Image (Minimum_Size) & ".  ACTUAL SIZE IS" &
         Integer'Image (Derived_Private_Int'Size));
   end if;

   Newproc;

   Result;

end Cd2a32g;
