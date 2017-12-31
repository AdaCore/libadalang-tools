-- C36304A.ADA

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
-- CHECK THAT BOUNDS OF CONSTANT STRING OBJECTS IF NOT GIVEN IN THE
-- DECLARATIONS ARE DETERMINED BY THE STRINGS' INITIAL VALUES.

-- DAT 2/17/81
-- JBG 8/21/83

with Report;
procedure C36304a is

   use Report;

   I3 : Integer := Ident_Int (3);

   S3  : constant String                := "ABC";
   S0  : constant String                := "";
   S1  : constant String                := "A";
   S2  : constant String                := "AB";
   S5  : constant String                := "ABCDE";
   S3a : constant String (I3 .. I3 + 2) := S3 (I3 - 2 .. I3);
   S3c : constant String                := S3a;
   S3d : constant String                := S3c & "";
   S3e : constant String                := S3d;
   X3  : constant String                := (I3 .. 5 => 'X');
   Y3  : constant String                := X3;
   Z0  : constant String                := (-3 .. -5 => 'A');

   procedure C (S : String; First, Last, Length : Integer; Id : String) is
   begin
      if S'First /= First then
         Failed
           ("'FIRST IS " & Integer'Image (S'First) & " INSTEAD OF " &
            Integer'Image (First) & " FOR " & Id);
      end if;

      if S'Last /= Last then
         Failed
           ("'LAST IS " & Integer'Image (S'Last) & " INSTEAD OF " &
            Integer'Image (Last) & " FOR " & Id);
      end if;

      if S'Length /= Length then
         Failed
           ("'LENGTH IS " & Integer'Image (S'Length) & " INSTEAD OF " &
            Integer'Image (Length) & " FOR " & Id);
      end if;
   end C;

begin
   Test ("C36304A", "CHECK UNUSUAL CONSTANT STRING BOUNDS");

   C (S0, 1, 0, 0, "S0");
   C (S1, 1, 1, 1, "S1");
   C (S2, 1, 2, 2, "S2");
   C (S5, 1, 5, 5, "S5");
   C (S3, 1, 3, 3, "S3");
   C (S3c, 3, 5, 3, "S3C");
   C (S3d, 3, 5, 3, "S3D");
   C (S3e, 3, 5, 3, "S3E");
   C (X3, 3, 5, 3, "X3");
   C (Y3, 3, 5, 3, "Y3");
   C (Z0, Ident_Int (-3), Ident_Int (-5), Ident_Int (0), "Z0");

   Result;
end C36304a;
