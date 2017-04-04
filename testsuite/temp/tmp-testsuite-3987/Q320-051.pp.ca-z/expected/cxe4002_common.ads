-- CXE4002.A
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
--      Check that parameter passing to remote procedures is handled
--      properly when the size of the parameters can be determined at
--      compile time.   Check that the following types can be passed
--      as parameters:  integer, float, static sized arrays, and simple
--      records.  Check the parameter passing using all three modes
--      and check that function results of the various types are handled
--      properly.  Check that both direct subprogram calls and indirect
--      calls through a value of a remote access to subprogram can be
--      used for the call.
--
-- TEST DESCRIPTION:
--      This test is composed of the following compilation units:
--          CXE4002_Common  - pure package containing declarations
--              shared between partitions A & B
--          CXE4002_Part_A1 - rci package interface for partition A
--          CXE4002_Part_A2 - rci package interface for partition A
--          CXE4002_A  - main procedure for partition A
--          CXE4002_B  - main procedure for partition B - main driver
--              for the test
--
-- SPECIAL REQUIREMENTS:
--      Compile the compilation units in this file.
--      Create the two partitions (A and B) with the following contents:
--        Partition A contains:
--           CXE4002_A  (main procedure)
--           CXE4002_Part_A1  (RCI package)
--           CXE4002_Part_A2  (RCI package)
--           and all normal and pure packages with'ed by these units.
--        Partition B contains:
--           CXE4002_B  (main procedure)
--           and all normal and pure packages with'ed by these units.
--        Note that package Report is included in both partitions.
--      Run the program by starting both partitions.  The partitions may be
--      started in either order.
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
--     17 APR 95   SAIC    Initial version
--     05 JUN 95   SAIC    Made floating point values friendly to
--                         heterogeneous systems.
--     19 DEC 95   SAIC    Fixed operator visibility problem.
--     25 APR 96   SAIC    Incorporated Reviewer comments.
--     25 JAN 01   RLB     Repaired startup race condition.
--
--!

package Cxe4002_Common is
   pragma Pure;

   -- types for parameters passed between partitions

   type Little_Number is range 0 .. 7;

   type Integer_Vector is array (2 .. 11) of Integer;

   subtype Description is String (1 .. 10);
   type Record_Data is record
      Part_No : Integer;
      Cost    : Float;
      Name    : Description;
   end record;
end Cxe4002_Common;
