-- CXE4003.A
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
--      Check that the task executing a remote subprogram call blocks
--      until the subprogram in the called partition returns.
--      Check that a remote procedure call can be aborted.
--      Check that remote subprogram calls are executed at most once.
--      Check that potentially concurrent calls from multiple tasks
--      can be handled by the PCS.
--
-- TEST DESCRIPTION:
--      This test is composed of the following compilation units:
--        CXE4003_A             - partition A main procedure
--        CXE4003_B             - partition B main procedure
--        CXE4003_Part_B1       - RCI package
--        CXE4003_Generic_RCI   - generic package
--        CXE4003_Part_B2       - instantiation of generic RCI pkg
--        CXE4003_Part_B3       - instantiation of generic RCI pkg
--        CXE4003_Part_B4       - instantiation of generic RCI pkg
--        CXE4003_Part_B5       - instantiation of generic RCI pkg
--        CXE4003_Blocking_Test - normal package
--        CXE4003_Cancellation_Test - normal package
--        CXE4003_Call_Test     - normal package
--      The test is divided into 3 sections.  The packages
--      CXE4003_Blocking_Test, CXE4003_Cancellation_Test, and
--      CXE4003_Call_Test each implement a section of the test.  The
--      main procedure for partition A sequences through these three
--      sections.
--
-- SPECIAL REQUIREMENTS:
--      Compile the compilation units in this file.
--      Create the two partitions (A and B) with the following contents:
--        Partition A contains:
--           CXE4003_A  (main procedure)
--           CXE4003_Blocking_Test
--           CXE4003_Cancellation_Test
--           CXE4003_Call_Test
--           Report
--           and all normal and pure packages with'ed by these units.
--        Partition B contains:
--           CXE4003_B  (main procedure)
--           CXE4003_Part_B1  (RCI package)
--           CXE4003_Part_B2  (RCI package)
--           CXE4003_Part_B3  (RCI package)
--           CXE4003_Part_B4  (RCI package)
--           CXE4003_Part_B5  (RCI package)
--           Report
--           and all normal and pure packages with'ed by these units.
--        Note that package Report is included in both partitions.
--      Run the program by starting both partitions.  Partition B
--      acts as a server and should be started first.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations that
--         support the Distribution Annex,
--         support the Remote_Call_Interface pragma, and
--         provide an implementation of System.RPC.
--      This test contains task priorities that should be used if
--      the implementation supports the Real-Time Annex.
--      The priority related code is marked with a "--RT" at the end
--      of a source line.  These lines can be removed from the test
--      if the implementation does not support the Real-Time annex.
--
-- PASS/FAIL CRITERIA:
--      This test passes if and only if both partition A and
--      partition B print a message reporting that the test passed.
--
--
-- CHANGE HISTORY:
--     09 MAY 95   SAIC    Initial version
--     23 JUN 95   SAIC    Fixed problem noted by reviewer where
--                         preemptive abort was being incorrectly
--                         determined.
--     19 DEC 95   SAIC    Lack of preemptive abort when the Real-Time
--                         Annex is supported results in failure.
--     22 FEB 96   SAIC    Incorporated Reviewer comments.
--                         New ImpDef.
--     01 DEC 97   EDS     Add pragma Remote_Call_Interface for each of the four
--                         instances of the package CXE4003_Generic_RCI.
--!

-----------------------------------------------------------------------------

package Cxe4003_Part_B1 is
   pragma Remote_Call_Interface;

   procedure Test_Complete;

   -- a call to Block_2 will not return until Release_1 and Release_2 have been
   -- called.
   procedure Block_2;
   -- a call to Release_1 is not accepted until Block_2 has been called
   procedure Release_1;
   procedure Release_2;

   -- support for cancellation test
   procedure Start_Cancellation_Test;
   procedure May_Be_Cancelled (Delay_Time : Duration);
   procedure End_Cancellation_Test (Name : String);
end Cxe4003_Part_B1;
