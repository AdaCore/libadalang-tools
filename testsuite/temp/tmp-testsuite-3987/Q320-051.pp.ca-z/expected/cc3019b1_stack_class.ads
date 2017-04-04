-- CC3019B1.ADA

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
--  THIS IS GENERIC PACKAGE WHICH IS USED TO CHECK THE LEVEL OF
--  NESTED GENERICS SUPPORTED BY AN IMPLEMENTATION. IT IS USED
--  BY THE MAIN PROCEDURE, I.E., CC3019B2M.ADA.
--
-- *** THIS FILE MUST BE COMPILED AFTER CC3019B0.ADA HAS BEEN
-- *** COMPILED.
--
-- HISTORY:
--         EDWARD V. BERARD, 31 AUGUST 1990

with Cc3019b0_List_Class;

generic

   type Element is limited private;

   with procedure Assign
     (Source      : in out Element;
      Destination : in out Element);

   with function "=" (Left : in Element; Right : in Element) return Boolean;

package Cc3019b1_Stack_Class is

   type Stack is limited private;

   Overflow  : exception;
   Underflow : exception;

   procedure Push
     (This_Element     : in out Element;
      On_To_This_Stack : in out Stack);

   procedure Pop
     (This_Element   : in out Element;
      Off_This_Stack : in out Stack);

   procedure Copy (This_Stack : in out Stack; To_This_Stack : in out Stack);

   procedure Clear (This_Stack : in out Stack);

   generic

      with procedure Process
        (This_Element : in     Element;
         Continue     :    out Boolean);

   procedure Iterate (Over_This_Stack : in Stack);

   function Number_Of_Elements (On_This_Stack : in Stack) return Natural;

   function "=" (Left : in Stack; Right : in Stack) return Boolean;

private

   package New_List_Class is new Cc3019b0_List_Class
     (Element => Element,
      Assign  => Assign,
      "="     => "=");

   type Stack is new New_List_Class.List;

end Cc3019b1_Stack_Class;
