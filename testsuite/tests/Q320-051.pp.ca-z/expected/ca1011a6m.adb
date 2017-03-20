-- CA1011A6M.ADA

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
-- CHECK THAT IF A SUBPROGRAM BODY IS INITIALLY COMPILED, SUBSEQUENT
-- ATTEMPTS TO COMPILE A SUBPROGRAM BODY WITH A DIFFERENT PARAMETER AND
-- RESULT TYPE PROFILE ARE ACCEPTED (SEE AI-00199).

-- SEPARATE FILES ARE:
--     CA1011A0  A LIBRARY PROCEDURE (CA1011A0).
--     CA1011A1  A LIBRARY PROCEDURE (CA1011A0).
--     CA1011A2  A LIBRARY PROCEDURE (CA1011A2).
--     CA1011A3  A LIBRARY PROCEDURE (CA1011A2).
--     CA1011A4  A LIBRARY FUNCTION  (CA1011A4).
--     CA1011A5  A LIBRARY FUNCTION  (CA1011A4).
--     CA1011A6M THE MAIN PROCEDURE.

-- BHS 7/20/84
-- JBG 5/23/85

with Ca1011a0, Ca1011a2, Ca1011a4;
with Report; use Report;
procedure Ca1011a6m is

   I : Integer := 5;
   J : Float   := 4.0;

begin

   Test
     ("CA1011A",
      "ATTEMPTS TO RECOMPILE A SUBPROGRAM WITH " &
      "NONCONFORMING PARAMETER OR RESULT TYPE " &
      "PROFILES ARE ACCEPTED");

   Ca1011a0 (X => I);             -- EXPECT DEFAULT Y
   if I = 3 then
      Comment ("SECOND DECLARATION OF CA1011A0 INVOKED CORRECTLY");
   end if;

   Ca1011a2 (Y => J);             -- USE DEFAULT X.
   if J = 3.0 then
      Comment ("SECOND DECLARATION OF CA1011A2 INVOKED CORRECTLY");
   end if;

   I := Integer (Ca1011a4);
   if I = 3 then
      Comment ("SECOND DECLARATION OF CA1011A4 INVOKED CORRECTLY");
   end if;

   Result;

end Ca1011a6m;
