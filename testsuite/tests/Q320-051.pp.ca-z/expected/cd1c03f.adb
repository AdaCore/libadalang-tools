-- CD1C03F.ADA

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
--     CHECK THAT THE VALUE OF 'SMALL FOR A DERIVED FIXED POINT TYPE
--     IS INHERITED FROM THE PARENT IF THE VALUE OF 'SMALL FOR THE
--     PARENT WAS DETERMINED BY A 'SMALL SPECIFICATION CLAUSE.

-- HISTORY:
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     JET 09/17/87  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cd1c03f is

   Specified_Small : constant := 0.25;

   type Flt is new Float;

   type Parent_Type is delta 1.0 range 0.0 .. 100.0;

   for Parent_Type'Small use Specified_Small;

   type Derived_Type is new Parent_Type;

   function Ident_Flt (F : Flt) return Flt is
   begin
      if Equal (3, 3) then
         return F;
      else
         return 0.0;
      end if;
   end Ident_Flt;

begin

   Test
     ("CD1C03F",
      "CHECK THAT THE VALUE OF 'SMALL FOR A " &
      "DERIVED FIXED POINT TYPE IS INHERITED " &
      "FROM THE PARENT IF THE VALUE OF 'SMALL " &
      "FOR THE PARENT WAS DETERMINED BY A 'SMALL " & "SPECIFICATION CLAUSE");

   if Parent_Type'Small /= Ident_Flt (Specified_Small) then
      Failed ("PARENT_TYPE'SMALL SHOULD BE EQUAL TO " & "THE SPECIFIED VALUE");
   end if;

   if Derived_Type'Small /= Ident_Flt (Specified_Small) then
      Failed
        ("DERIVED_TYPE'SMALL SHOULD BE EQUAL TO " & "THE SPECIFIED VALUE");
   end if;

   Result;

end Cd1c03f;
