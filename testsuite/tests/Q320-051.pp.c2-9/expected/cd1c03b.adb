-- CD1C03B.ADA

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
--     CHECK THAT THE SIZE OF A DERIVED TYPE IS INHERITED FROM THE
--     PARENT IF THE SIZE OF THE PARENT WAS DETERMINED BY A PRAGMA
--     PACK.

-- HISTORY:
--     JET 09/16/87  CREATED ORIGINAL TEST.
--     PWB 03/27/89  MODIFIED COMPARISON OF OBJECT SIZE TO PARENT SIZE.

with Report; use Report;
procedure Cd1c03b is

   type Enum is (E1, E2, E3);

   type Normal_Type is array (1 .. 100) of Enum;

   type Parent_Type is array (1 .. 100) of Enum;
   pragma Pack (Parent_Type);

   type Derived_Type is new Parent_Type;
   X : Derived_Type := (others => Enum'First);

begin

   Test
     ("CD1C03B",
      "CHECK THAT THE SIZE OF A DERIVED TYPE IS " &
      "INHERITED FROM THE PARENT IF THE SIZE OF " &
      "THE PARENT WAS DETERMINED BY A PRAGMA PACK");

   if Parent_Type'Size = Ident_Int (Normal_Type'Size) then
      Comment
        ("PRAGMA PACK HAD NO EFFECT ON THE SIZE OF " &
         "PARENT_TYPE, WHICH IS" &
         Integer'Image (Parent_Type'Size));
   elsif Parent_Type'Size > Ident_Int (Normal_Type'Size) then
      Failed
        ("PARENT_TYPE'SIZE SHOULD NOT BE GREATER THAN" &
         Integer'Image (Normal_Type'Size) &
         ".  ACTUAL SIZE IS" &
         Integer'Image (Parent_Type'Size));
   end if;

   if Derived_Type'Size > Ident_Int (Parent_Type'Size) then
      Failed
        ("DERIVED_TYPE'SIZE SHOULD NOT BE GREATER THAN" &
         Integer'Image (Parent_Type'Size) &
         ".  ACTUAL SIZE IS" &
         Integer'Image (Derived_Type'Size));
   end if;

   if X'Size < Derived_Type'Size then
      Failed ("OBJECT SIZE TOO LARGE.  FIRST VALUE IS " & Enum'Image (X (1)));
   end if;

   Result;

end Cd1c03b;
