-- CXC6001.A
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
--      Check that atomic and volatile elementary types that are not by-copy
--      types, as well as types with subcomponents that are atomic or
--      volatile are by-reference types.
--
-- TEST DESCRIPTION:
--      This test ascertains that local operations are "written
--      through" to global variables when passed.  This test also
--      ascertains that changes to the formal do not affect the actual
--      until the subprogram completes.  The types defined are defined to
--      be "tempting" to pass by copy.
--
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Systems Programming Annex.
--
-- ACCEPTANCE CRITERIA:
--      This test must execute correctly, or it must be identified as
--      illegal with an error indicating that the implementation cannot
--      support the indivisible reads and updates required by the pragmas
--      Atomic to <objects/types> or Atomic_Components to
--      <objects/types/components>.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      08 Nov 95   SAIC    Revised for 2.0.1
--      12 NOV 96   SAIC    Revised for 2.1
--
--!

package Cxc6001_0 is

   ------------ Subtest 1, Atomic Reference Semantics, Object

   type Element is record
      Atomic_Weight : Integer;
   end record;

   pragma Atomic (Element);                                  -- N/A => ERROR.
   -- Implementation is allowed to reject this if it does not support
   -- individual read/write for records.

   Plutonium : Element := (Atomic_Weight => 244);

   procedure Check_Reference_Semantics (Material : in out Element);

   ------------ Subtest 2, Atomic Reference Semantics, Component

   type Compound is array (1 .. 1) of Element;  -- By reference type since
   -- subcomponent is atomic

   Water : Compound := (1 => (Atomic_Weight => 16));

   procedure Check_Reference_Semantics (Material : in out Compound);

end Cxc6001_0;