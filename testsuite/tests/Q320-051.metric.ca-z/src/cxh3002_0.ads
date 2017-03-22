-- CXH30030.A
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
-- OBJECTIVE
--     See CHX30031.AM
--
-- TEST DESCRIPTION
--     See CHX30031.AM
--
-- TEST FILES:
--      The following files comprise this test:
--
--      => CXH30030.A
--         CXH30031.AM
--
-- APPLICABILITY CRITERIA:
--     See CHX30031.AM
--
-- SPECIAL REQUIREMENTS
--     See CHX30031.AM
--
-- CHANGE HISTORY:
--      26 OCT 95   SAIC   Initial version for 2.1
--      07 JUN 96   SAIC   Revised by reviewer request, split to multifile
--
--!

  pragma Reviewable;

-- This test requires that this configuration pragma be applied to all
-- following compilation units in the environment; specifically the ones
-- in file CXH30031.AM
-- CXH3002.A
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
-- OBJECTIVE
--     Check that pragma Inspection_Point is allowed whereever a declarative
--     item or statement is allowed.  Check that pragma Inspection_Point may
--     have zero or more arguments.  Check that the execution of pragma
--     Inspection_Point has no effect.
--
-- TEST DESCRIPTION
--     Check pragma Inspection_Point applied to:
--       A no objects,
--       B one object,
--       C multiple objects.
--     Check pragma Inspection_Point applied to:
--       D Enumeration type objects,
--       E Integer type objects (signed and unsigned),
--       F access type objects,
--       G Floating Point type objects,
--       H Fixed point type objects,
--       I array type objects,
--       J record type objects,
--       K tagged type objects,
--       L protected type objects,
--       M controlled type objects,
--       N task type objects.
--     Check pragma Inspection_Point applied in:
--       O declarations (package, procedure)
--       P statements (incl package elaboration)
--       Q subprogram (procedure, function, finalization)
--       R package
--       S specification
--       T body (PO entry, task body, loop body, accept body, select body)
--       U task
--       V protected object
--
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Safety and Security Annex.
--
--
-- CHANGE HISTORY:
--      26 OCT 95   SAIC   Initial version
--      12 NOV 96   SAIC   Revised for 2.1
--
--!

----------------------------------------------------------------- CXH3002_0

package CXH3002_0 is

  type Enum is (Item,Stuff,Things);

  type Int  is range 0..256;

  type Unt  is mod 256;

  type Flt  is digits 5;

  type Fix  is delta 0.5 range -1.0..1.0;

  type Root(Disc: Enum) is record
    I: Int;
    U: Unt;
  end record;

  type List   is array(Unt) of Root(Stuff);

  type A_List is access all List;
  type A_Proc is access procedure(R:Root);

  procedure Proc(R:Root);
  function  Func return A_Proc;

  protected type PT is
    entry Prot_Entry(Switch: Boolean);
  private
    Toggle : Boolean := False;
  end PT;

  task type TT is
    entry Task_Entry(Items: in A_List);
  end TT;

  -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
  pragma Inspection_Point;                                          -- AORS
  -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====

end CXH3002_0;
