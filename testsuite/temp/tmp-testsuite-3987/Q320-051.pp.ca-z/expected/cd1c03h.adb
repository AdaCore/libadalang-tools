-- CD1C03H.ADA

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
--     CHECK THAT THE RECORD SIZE AND THE COMPONENT POSITIONS AND
--     SIZES OF A DERIVED RECORD TYPE ARE INHERITED FROM THE
--     PARENT IF THOSE ASPECTS OF THE PARENT WERE DETERMINED BY A
--     RECORD REPRESENTATION CLAUSE.

-- HISTORY:
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     JET 09/17/87  CREATED ORIGINAL TEST.

with Report; use Report;
with System; use System;

procedure Cd1c03h is

   Units_Per_Integer : constant :=
     (Integer'Size + System.Storage_Unit - 1) / System.Storage_Unit;

   type E_Type is (Red, Blue, Green);

   type Parent_Type is record
      I : Integer range 0 .. 127 := 127;
      C : Character              := 'S';
      B : Boolean                := False;
      E : E_Type                 := Blue;
   end record;

   for Parent_Type use record
      C at 0 * Units_Per_Integer range 0 ..   Character'Size - 1;
      B at 1 * Units_Per_Integer range 0 ..     Boolean'Size - 1;
      I at 2 * Units_Per_Integer range 0 .. Integer'Size / 2 - 1;
      E at 3 * Units_Per_Integer range 0 ..   Character'Size - 1;
   end record;

   type Derived_Type is new Parent_Type;

   P_Rec : Parent_Type;
   Rec   : Derived_Type;

begin

   Test
     ("CD1C03H",
      "CHECK THAT THE RECORD SIZE AND THE COMPONENT " &
      "POSITIONS AND SIZES OF A DERIVED RECORD " &
      "TYPE ARE INHERITED FROM THE PARENT IF THOSE " &
      "ASPECTS OF THE PARENT WERE DETERMINED BY " &
      "A RECORD REPRESENTATION CLAUSE");

   if Derived_Type'Size /= Ident_Int (Parent_Type'Size) then
      Failed ("DERIVED_TYPE'SIZE WAS NOT INHERITED FROM " & "PARENT_TYPE");
   end if;

   if Rec.I'Size /= P_Rec.I'Size or
     Rec.C'Size /= P_Rec.C'Size or
     Rec.B'Size /= P_Rec.B'Size or
     Rec.E'Size /= P_Rec.E'Size
   then
      Failed
        ("THE SIZES OF DERIVED_TYPE ELEMENTS WERE NOT " &
         "INHERITED FROM PARENT_TYPE");
   end if;

   Rec := (12, 'T', True, Red);

   if (Rec.I /= 12) or (Rec.C /= 'T') or (not Rec.B) or (Rec.E /= Red) then
      Failed ("THE VALUES OF DERIVED_TYPE COMPONENTS WERE " & "INCORRECT");
   end if;

   if Rec.I'Position /= P_Rec.I'Position or
     Rec.C'Position /= P_Rec.C'Position or
     Rec.B'Position /= P_Rec.B'Position or
     Rec.E'Position /= P_Rec.E'Position
   then
      Failed
        ("THE POSITIONS OF DERIVED_TYPE COMPONENTS WERE " &
         "NOT INHERITED FROM PARENT_TYPE");
   end if;

   if Rec.I'First_Bit /= P_Rec.I'First_Bit or
     Rec.C'First_Bit /= P_Rec.C'First_Bit or
     Rec.B'First_Bit /= P_Rec.B'First_Bit or
     Rec.E'First_Bit /= P_Rec.E'First_Bit
   then
      Failed
        ("THE FIRST_BITS OF DERIVED_TYPE COMPONENTS WERE " &
         "NOT INHERITED FROM PARENT_TYPE");
   end if;

   if Rec.I'Last_Bit /= P_Rec.I'Last_Bit or
     Rec.C'Last_Bit /= P_Rec.C'Last_Bit or
     Rec.B'Last_Bit /= P_Rec.B'Last_Bit or
     Rec.E'Last_Bit /= P_Rec.E'Last_Bit
   then
      Failed
        ("THE LAST_BITS OF DERIVED_TYPE COMPONENTS WERE " &
         "NOT INHERITED FROM PARENT_TYPE");
   end if;

   Result;

end Cd1c03h;
