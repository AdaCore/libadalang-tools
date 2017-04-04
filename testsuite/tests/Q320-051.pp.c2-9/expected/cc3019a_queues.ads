-- CC3019A.ADA

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
-- CHECK THAT INSTANTIATIONS OF NESTED GENERIC UNITS ARE PROCESSED CORRECTLY.

-- JBG 11/6/85

generic
   type Element_Type is private;
package Cc3019a_Queues is

   type Queue_Type is private;

   procedure Add (To_Q : in out Queue_Type; Value : Element_Type);

   generic
      with procedure Apply (Val : Element_Type);
   procedure Iterator (To_Q : Queue_Type);

private

   type Contents_Type is array (1 .. 3) of Element_Type;
   type Queue_Type is record
      Contents : Contents_Type;
      Size     : Natural := 0;
   end record;

end Cc3019a_Queues;
