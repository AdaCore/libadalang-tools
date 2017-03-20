-- CXE5001.A
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
--      Check that the specifications of the package System.RPC
--      are available for use.
--
-- TEST DESCRIPTION:
--      This test verifies that the types and subprograms specified for the
--      interface to the Partition Communication Subsystem are present.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations:
--         supporting the Distribution Annex, and
--         providing an implementation of the language-defined System.RPC
--            rather than an alternative declaration as allowed by E.5(27.1/2).
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      31 Oct 95   SAIC    Fixed problems for ACVC 2.0.1
--      21 Dec 07   RLB     Added Applicability Criteria
--
--!


--    -- The handler for incoming RPCs
--    type RPC_Receiver is access procedure(
--       Params     : access Params_Stream_Type;
--       Result     : access Params_Stream_Type);
with System.RPC;
procedure CXE5001_1 (Param_In  : access System.RPC.Params_Stream_Type;
                     Param_Out : access System.RPC.Params_Stream_Type) is
begin
   null;
end CXE5001_1;
