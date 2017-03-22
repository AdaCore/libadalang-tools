-- CXE4004.A
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
--      properly when the parameters are of a dynamic size or have
--      discriminants.
--      Check that the following types can be passed as parameters:
--      dynamic sized arrays, constrained discriminated records,
--      unconstrained discriminated records, and tagged records.
--      Check the parameter passing using all three modes and check
--      that function results of the various types are handled properly.
--      Check that both direct subprogram calls and indirect calls
--      through a value of a remote access to subprogram can be used
--      for the call.
--
-- TEST DESCRIPTION:
--      This test is composed of the following compilation units:
--          CXE4004_Common  - pure package containing declarations
--              shared between partitions A & B
--          CXE4004_Shared  - normal package of support routines
--              included in both partitions A & B
--          CXE4004_Part_A1 - rci package interface for partition A
--          CXE4004_Part_A2 - rci package interface for partition A
--          CXE4004_Part_B  - rci package interface for partition B
--          CXE4004_A  - main procedure for partition A
--          CXE4004_B  - main procedure for partition B - main driver
--              for the test.
--       The types that are declared in CXE4004_Common are used
--       for parameter passing across partitions.
--
-- SPECIAL REQUIREMENTS:
--      Compile the compilation units in this file.
--      Create the two partitions (A and B) with the following contents:
--        Partition A contains:
--           CXE4004_A  (main procedure)
--           CXE4004_Part_A1  (RCI package body)
--           CXE4004_Part_A2  (RCI package body)
--           CXE4004_Shared
--           CXE4004_Common
--           Report
--           and all normal and pure packages with'ed by these units.
--        Partition B contains:
--           CXE4004_B  (main procedure)
--           CXE4004_Part_B  (RCI package body)
--           CXE4004_Shared
--           CXE4004_Common
--           Report
--           and all normal and pure packages with'ed by these units.
--        Note that package Report is included in both partitions and
--        prints messages from both partitions.
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
--     15 MAY 95   SAIC    Initial version for ACVC 2.1
--      1 MAY 96   SAIC    Fixed problems noted by reviewers.
--     25 JAN 01   RLB     Repaired startup race condition.
--
--!

package CXE4004_Common is
  pragma Pure;

  -- types for parameters passed between partitions

  type Integer_Vector is array (Integer range <>) of Integer;
  subtype Three_Ints is Integer_Vector (4..6);

  type Discriminated_Record (Disc : Boolean := True) is
    record
      Common_Component : Integer;
      case Disc is
        when True =>
           A_Char : Character;
        when False =>
           Ints_3 : Three_Ints;
           Another_Int : Integer;
      end case;
    end record;

  type Another_Discriminated_Record (Disc : Boolean;
                                     Disc_Low : Integer;
                                     Disc_High : Integer) is
    record
      Common_Component : Integer := 18;
      case Disc is
        when True =>
           A_Char : Character := 'X';
        when False =>
           Some_Ints : Integer_Vector (Disc_Low .. Disc_High);
           A_Bool : Boolean := True;
           Some_More_Ints : Integer_Vector (1.. Disc_High);
           Even_More_Ints : Integer_Vector (Disc_Low .. 20);
      end case;
    end record;

  type Root_Tagged_Record is tagged
    record
      In_Root : Integer := 22;
    end record;

  type Extended_1 is new Root_Tagged_Record with
    record
      In_Extension : Integer := 445;
    end record;

  type Extended_2 is new Root_Tagged_Record with
    record
      Ext_2_Component : Integer := 135;
    end record;

end CXE4004_Common;
