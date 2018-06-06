-- CC3019C0.ADA

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
-- OBJECTIVE
--   THIS IS GENERIC PACKAGE WHICH IS USED TO CHECK THE LEVEL OF
--   NESTED GENERICS SUPPORTED BY AN IMPLEMENTATION.
--
-- HISTORY:
--         EDWARD V. BERARD, 31 AUGUST 1990

generic

   type Element is limited private;

   with procedure Assign
     (Source : in out Element; Destination : in out Element);

   with function "=" (Left : in Element; Right : in Element) return Boolean;

package Cc3019c0_List_Class is

   type List is limited private;

   Overflow  : exception;
   Underflow : exception;

   procedure Add (This_Element : in out Element; To_This_List : in out List);

   procedure Delete
     (This_Element : in out Element; From_This_List : in out List);

   procedure Copy (This_List : in out List; To_This_List : in out List);

   procedure Clear (This_List : in out List);

   generic

      with procedure Process
        (This_Element : in Element; Continue : out Boolean);

   procedure Iterate (Over_This_List : in List);

   function Number_Of_Elements (In_This_List : in List) return Natural;

   function "=" (Left : in List; Right : in List) return Boolean;

private

   type List_Table is array (Positive range 1 .. 10) of Element;

   type List is record
      Length      : Natural := 0;
      Actual_List : List_Table;
   end record;

end Cc3019c0_List_Class;
