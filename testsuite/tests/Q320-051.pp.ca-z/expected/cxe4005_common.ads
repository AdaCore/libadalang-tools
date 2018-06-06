-- CXE4005.A
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
--      dispatching call is made to a remote access to class wide
--      type.  (5)
--      Check that Program_Error is raised if the tag of the actual
--      parameter identifies a tagged type declared in a normal
--      package or in the body of a remote call interface package. (18)
--      Check that in a dispatching call with two controlling
--      operands, Constraint_Error is raised if the two remote
--      access-to-class-wide values originated
--      from Access attribute_references in different partitions. (19)
--
-- TEST DESCRIPTION:
--      This test is composed of the following compilation units:
--          CXE4005_Common  - pure package containing declarations
--              shared between partitions A & B
--          CXE4005_Part_A1 - rci package interface for partition A
--          CXE4005_Part_A2 - rci package interface for partition A
--          CXE4005_Part_B  - rci package interface for partition B
--          CXE4005_Remote_Types - a remote types package
--          CXE4005_Normal  - normal package
--          CXE4005_A  - main procedure for partition A
--          CXE4005_B  - main procedure for partition B - main driver
--              for the test.
--      Tagged types are declared in a variety of the above packages.
--      Objects of these tagged types are declared in each of the RCI
--      packages.  A remote access to class wide type is created for each
--      of the objects and stored in a table created by CXE4005_B.
--      These access values are used to make a variety of dispatching calls
--      where the object passed to the dispatched routine is checked
--      to insure the correct call was made.  This check is accomplished
--      by having each of the tagged type objects contain a serial number
--      that is returned as a result of the remote call.  The actual
--      serial number is compared against the expected serial number.
--
-- SPECIAL REQUIREMENTS:
--      Compile the compilation units in this file.
--      Create the two partitions (A and B) with the following contents:
--        Partition A contains:
--           CXE4005_A  (main procedure)
--           CXE4005_Part_A1  (RCI package body)
--           CXE4005_Part_A2  (RCI package body)
--           CXE4005_Remote_Types
--           CXE4005_Common
--           Report
--           and all normal and pure packages with'ed by these units.
--        Partition B contains:
--           CXE4005_B  (main procedure)
--           CXE4005_Part_B  (RCI package body)
--           CXE4005_Remote_Types
--           CXE4005_Normal
--           CXE4005_Common
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
--     17 MAY 95   SAIC    Initial version
--     29 APR 96   SAIC    Incorporated reviewer comments.
--     02 FEB 99   RLB     Corrected so that the subtest for E.4(18) makes
--                         the correct check.
--     03 FEB 99   RLB     Corrected again to avoid violation of more rules.
--     25 JAN 01   RLB     Repaired startup race condition.
--
--!

package Cxe4005_Common is
   pragma Pure;

   -- controls progress output from the tests.
   Verbose : constant Boolean := False;

   -- exception to signify that the serial number of an object was not a one of
   -- the expected values for that type
   Wrong_Object : exception;

   -- identification of where a type is declared and where an access type was
   -- evaluated that refers to an object of that type.
   type Type_Selection is
     (Common_Spec,        --  xx1
      Rt_Spec,            --  xx6
      B_Body,             --  xx7
      Normal_Spec);       --  xx8
   type Access_Evaluation is
     (A1,              --  1xx
      A2,              --  2xx
      B);              --  3xx

   -- root tagged type for remote access to class wide type test
   type Root_Tagged_Type is tagged limited private;

   procedure Single_Controlling_Operand
     (Rtt    :     access Root_Tagged_Type; Test_Number : in Integer;
      Obj_Sn : out Integer);
   procedure Dual_Controlling_Operands
     (Rtt1        :    access Root_Tagged_Type; Rtt2 : access Root_Tagged_Type;
      Test_Number : in Integer; Obj_Sn1 : out Integer; Obj_Sn2 : out Integer);

   procedure Set_Serial_Number
     (Rtt : access Root_Tagged_Type; Sn : in Integer);

   function Serial_Number (Rtt : access Root_Tagged_Type) return Integer;

   type Open_Tagged_Type is tagged record
      Field : Integer;
   end record;
   procedure Open_Op (Ott : Open_Tagged_Type);

private
   type Root_Tagged_Type is tagged limited record
      Serial_Number : Integer := 123;
   end record;
end Cxe4005_Common;
