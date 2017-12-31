-- CD1C03C.ADA

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
--     CHECK THAT THE COLLECTION SIZE OF A DERIVED TYPE IS
--     INHERITED FROM THE PARENT IF THE COLLECTION SIZE OF
--     THE PARENT WAS DETERMINED BY A COLLECTION SIZE CLAUSE.

-- HISTORY:
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     JET 09/16/87  CREATED ORIGINAL TEST.
--     RJW 02/10/88  RENAMED FROM CD1C03C.TST.  REMOVED MACRO -
--                   ACC_SIZE.

with Report; use Report;
procedure Cd1c03c is

   Specified_Size : constant := 512;

   type Parent_Type is access String;

   for Parent_Type'Storage_Size use Specified_Size;

   type Derived_Type is new Parent_Type;

begin

   Test
     ("CD1C03C",
      "CHECK THAT THE COLLECTION  SIZE OF A " &
      "DERIVED TYPE IS INHERITED FROM THE PARENT " &
      "IF THE COLLECTION SIZE OF THE PARENT WAS " &
      "DETERMINED BY A COLLECTION SIZE CLAUSE");

   if Parent_Type'Storage_Size < Ident_Int (Specified_Size) then
      Failed
        ("PARENT_TYPE'STORAGE_SIZE SHOULD NOT BE " &
         "LESS THAN SPECIFIED_SIZE.  " & "ACTUAL SIZE IS" &
         Integer'Image (Parent_Type'Size));
   end if;

   if Derived_Type'Storage_Size /= Ident_Int (Parent_Type'Storage_Size) then
      Failed
        ("DERIVED_TYPE'STORAGE_SIZE SHOULD BE " &
         "EQUAL TO PARENT_TYPE'STORAGE_SIZE.  " & "ACTUAL SIZE IS" &
         Integer'Image (Derived_Type'Storage_Size));
   end if;

   Result;

end Cd1c03c;
