-- CXE4006.A
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
--      Check that calls can be made to remote procedures when a
--      dispatching call is made where the controlling operand
--      designates a type declared in a remote call interface package.
--      Check that tagged types can be passed between partitions
--      when passed as a class-wide type.
--      In a remote subprogram call with a formal parameter of a
--      class-wide type, check that Program_Error is raised if the
--      actual parameter identifies a tagged type declared in a
--      normal package.
--
-- TEST DESCRIPTION:
--      This test is composed of the following compilation units:
--          CXE4006_Common  - pure package containing declarations
--              shared between partitions A & B
--          CXE4006_Part_A1 - rci package interface for partition A
--          CXE4006_Part_A2 - rci package interface for partition A
--          CXE4006_Part_B  - rci package interface for partition B
--          CXE4006_Normal  - normal package
--          CXE4006_A  - main procedure for partition A
--          CXE4006_B  - main procedure for partition B - main driver
--              for the test.
--      Tagged types are declared in all of the above packages.  The
--      first set of tests involve declaring and initializing objects
--      of each of the tagged types.  A dispatching call is made with
--      each of the objects.  One of the parameters returned from the
--      call is an indication of which routine actually handled the
--      call.  A check is made that the correct routine was called.
--      The second set of tests consist of passing an object of
--      each of the tagged types across partitions twice and then
--      a dispatching call is made.  The tag of each object is checked
--      to make sure it made it across the partition boundary ok.
--      The final test uses the structure set up for the second set
--      of tests.  In this case, however, an object of the tagged
--      type declared in the normal package is used.  This should
--      result in a program_error.
--
-- SPECIAL REQUIREMENTS:
--      Compile the compilation units in this file.
--      Create the two partitions (A and B) with the following contents:
--        Partition A contains:
--           CXE4006_A  (main procedure)
--           CXE4006_Part_A1  (RCI package body)
--           CXE4006_Part_A2  (RCI package body)
--           CXE4006_Common
--           Report
--           and all normal and pure packages with'ed by these units.
--        Partition B contains:
--           CXE4006_B  (main procedure)
--           CXE4006_Part_B  (RCI package body)
--           CXE4006_Normal
--           CXE4006_Common
--           Report
--           and all normal and pure packages with'ed by these units.
--        Note that package Report is included in both partitions and
--        prints messages from both partitions.
--      Run the program by starting partition A and then partition B.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations:
--         supporting the Distribution Annex,
--         supporting the Remote_Call_Interface pragma, and
--         providing an implementation of System.RPC.
--
-- PASS/FAIL CRITERIA:
--      This test passes if and only if both partition A and
--      partition B print a message reporting that the test passed.
--
--
-- CHANGE HISTORY:
--     30 MAY 95   SAIC    Initial version
--     29 APR 96   SAIC    Fixed operator visibility problem
--     05 AUG 98   EDS     Fix call of procedure
--     13 AUG 99   RLB     Repaired startup race condition
--!

package Cxe4006_Common is
   pragma Pure;

   -- controls progress output from the tests. The value of this flag does not
   -- affect whether or not the test passes.
   Verbose : constant Boolean := False;

   -- exception to signify that the test number or object was not a one of the
   -- expected values
   Failed_Check : exception;

   -- instances of types derived from Root_Tagged_Type. Used to identify the
   -- routine that received the dispatching call.

   type Type_Decl_Location is
     (Common_Spec,
      Part_A1_1_Spec,
      Part_A1_2_Spec,
      Part_A2_Spec,
      Part_B_Spec,
      Part_B_Body,
      Normal_Spec);

   -- root tagged type for remote access to class wide type test
   type Root_Tagged_Type is tagged record
      Common_Record_Field : Integer := 1_234;
   end record;

   procedure Single_Controlling_Operand
     (Rtt         : in out Root_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);

end Cxe4006_Common;
