-- CA11B02.A
--
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
--
-- OBJECTIVE:
--      Check that a type derived in a client of a public child inherits
--      primitive operations from parent.
--
-- TEST DESCRIPTION:
--      Declare a root record type with discriminant in a package
--      specification. Declare a primitive subprogram for the type
--      (foundation code).
--
--      Add a public child to the above package.  Derive a new type
--      with constraint to the discriminant record type from the parent
--      package.  Declare a new primitive subprogram to write to the child
--      derived type.
--
--      In the main program, "with" the child.  Derive a new type using the
--      record type from the child package.  Access the inherited operations
--      from both parent and child packages.
--
-- TEST FILES:
--      This test depends on the following foundation code:
--
--         FA11B00.A
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

-- Child package of FA11B00.
   package Fa11b00.Ca11b02_0 is     -- Application_Two_Widget
-- This public child declares a derived type from its parent.  It
-- represents processing of widgets in a window system.

   -- Dimension of app2_widget is limited to 5000 pixels.

   type App2_Widget is new App1_Widget (Maximum_Size => 5_000);
   -- Derived record of parent type.

   -- Inherits procedure App1_Widget_Specific_Oper from parent.

   -- Primitive operation of type App2_Widget.

   procedure App2_Widget_Specific_Op1
     (The_Widget : in out App2_Widget;
      S          : in     Widget_Size);

   -- Primitive operation of type App2_Widget.

   procedure App2_Widget_Specific_Op2
     (The_Widget : in out App2_Widget;
      Loc        : in     Widget_Location);

end Fa11b00.Ca11b02_0;      -- Application_Two_Widget
