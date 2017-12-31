-- CD1C04A.ADA

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
--     CHECK THAT A SIZE CLAUSE CAN BE GIVEN FOR A DERIVED TYPE, A
--     DERIVED PRIVATE TYPE, AND A DERIVED LIMITED PRIVATE TYPE EVEN
--     IF THE SIZE IS INHERITED FROM THE PARENT, AND THAT THE SIZE
--     CLAUSES FOR THE DERIVED TYPES OVERRIDE THE PARENTS'.

-- HISTORY:
--     PWB 03/25/89  MODIFIED METHOD OF CHECKING OBJECT SIZE AGAINST
--                   TYPE SIZE; CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     JET 09/16/87  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cd1c04a is

   Specified_Size : constant := Integer'Size / 2;

   type Parent_Type is range 0 .. 100;

   for Parent_Type'Size use Integer'Size;

   type Derived_Type is new Parent_Type;

   for Derived_Type'Size use Specified_Size;

   package P is
      type Private_Parent is private;
      type Lim_Priv_Parent is limited private;
   private
      type Private_Parent is range 0 .. 100;
      for Private_Parent'Size use Integer'Size;
      type Lim_Priv_Parent is range 0 .. 100;
      for Lim_Priv_Parent'Size use Integer'Size;
   end P;

   use P;

   type Derived_Private_Type is new Private_Parent;

   for Derived_Private_Type'Size use Specified_Size;

   type Derived_Lim_Priv_Type is new Lim_Priv_Parent;

   for Derived_Lim_Priv_Type'Size use Specified_Size;

   Dt   : Derived_Type := 100;
   Dpt  : Derived_Private_Type;
   Dlpt : Derived_Lim_Priv_Type;

begin

   Test
     ("CD1C04A",
      "CHECK THAT A SIZE CLAUSE CAN BE GIVEN FOR " &
      "A DERIVED TYPE, A DERIVED PRIVATE TYPE, AND " &
      "A DERIVED LIMITED PRIVATE TYPE EVEN IF THE " &
      "SIZE IS INHERITED FROM THE PARENT, AND THAT " &
      "THE SIZE CLAUSES FOR THE DERIVED TYPES " & "OVERRIDE THE PARENTS'");

   if Parent_Type'Size /= Ident_Int (Integer'Size) then
      Failed
        ("PARENT_TYPE'SIZE SHOULD BE " & Integer'Image (Integer'Size) &
         ".  ACTUAL SIZE IS" & Integer'Image (Parent_Type'Size));
   end if;

   if Derived_Type'Size /= Ident_Int (Specified_Size) then
      Failed
        ("DERIVED_TYPE'SIZE SHOULD BE " & Integer'Image (Specified_Size) &
         ".  ACTUAL SIZE IS" & Integer'Image (Derived_Type'Size));
   end if;

   if Dt'Size < Ident_Int (Specified_Size) then
      Failed
        ("DT'SIZE SHOULD NOT BE LESS THAN" & Integer'Image (Specified_Size) &
         ".  ACTUAL SIZE IS" & Integer'Image (Dt'Size));
   end if;

   if Private_Parent'Size < Ident_Int (Integer'Size) then
      Failed
        ("PRIVATE_PARENT'SIZE SHOULD NOT BE LESS THAN" &
         Integer'Image (Integer'Size) & ".  ACTUAL SIZE IS" &
         Integer'Image (Private_Parent'Size));
   end if;

   if Derived_Private_Type'Size /= Ident_Int (Specified_Size) then
      Failed
        ("DERIVED_PRIVATE_TYPE'SIZE SHOULD BE " &
         Integer'Image (Specified_Size) & ".  ACTUAL SIZE IS" &
         Integer'Image (Derived_Private_Type'Size));
   end if;

   if Dpt'Size < Ident_Int (Specified_Size) then
      Failed
        ("DPT'SIZE SHOULD NOT BE LESS THAN" & Integer'Image (Specified_Size) &
         ".  ACTUAL SIZE IS" & Integer'Image (Dpt'Size));
   end if;

   if Lim_Priv_Parent'Size /= Ident_Int (Integer'Size) then
      Failed
        ("LIM_PRIV_PARENT'SIZE SHOULD BE" & Integer'Image (Integer'Size) &
         ".  ACTUAL SIZE IS" & Integer'Image (Lim_Priv_Parent'Size));
   end if;

   if Derived_Lim_Priv_Type'Size /= Ident_Int (Specified_Size) then
      Failed
        ("DERIVED_LIM_PRIV_TYPE'SIZE SHOULD BE " &
         Integer'Image (Specified_Size) & ".  ACTUAL SIZE IS" &
         Integer'Image (Derived_Lim_Priv_Type'Size));
   end if;

   if Dlpt'Size < Ident_Int (Specified_Size) then
      Failed
        ("DLPT'SIZE SHOULD NOT BE LESS THAN" & Integer'Image (Specified_Size) &
         ".  ACTUAL SIZE IS" & Integer'Image (Dlpt'Size));
   end if;

   Result;

end Cd1c04a;
