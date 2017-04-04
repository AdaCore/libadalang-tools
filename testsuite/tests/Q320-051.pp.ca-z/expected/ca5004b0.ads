-- CA5004B2M.ADA

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
-- CHECK THAT PRAGMA ELABORATE IS ACCEPTED AND OBEYED EVEN IF THE UNIT
--    NAMED IN THE PRAGMA DOES NOT YET HAVE A BODY IN THE LIBRARY OR IF
--    ITS BODY IS OBSOLETE.
-- CHECK THAT MORE THAN ONE NAME IS ALLOWED IN A PRAGMA ELABORATE.
--
-- SPECIAL INSTRUCTIONS:
--     1. Compile CA5004B0.ADA
--     2. Compile CA5004B1.ADA
--     3. Compile CA5004B2M.ADA
--     4. Bind/Link main unit CA5004B2M
--     5. Execute the resulting file
--
-- TEST FILES:
--        CA5004B0.ADA
--        CA5004B1.ADA
--     => CA5004B2M.ADA

-- BHS 8/03/84
-- JRK 9/20/84
-- PWN 11/30/94 ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL. PWN 05/31/96
-- Split test into files without duplicate unit names. TMB 11/20/96 ADDED
-- PROCEDURE DECL TO CA5004B0 TO INSURE IT MAKES
--               THE OLD BODY OBSOLETE
-- TMB 12/2/96 MADE NAME OF MAIN PROCEDURE SAME AS FILE NAME RLB 03/11/99 Split
-- first test file in order to prevent good units
--              from being made obsolete.

-------------------------------------------------------------

package Ca5004b0 is          -- OLD BODY NOW OBSOLETE.

   I : Integer := 2;
   B : Boolean := True;

   function F return Boolean;
   procedure P;

end Ca5004b0;
