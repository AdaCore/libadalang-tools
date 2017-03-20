-- CA2001H3M.ADA

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
-- CHECK THAT IF A BODY_STUB IS DELETED FROM A COMPILATION UNIT,
--   THE PREVIOUSLY EXISTING SUBUNIT CAN NO LONGER BE ACCESSED.

-- SEPARATE FILES ARE;
--   CA2001H0  A LIBRARY FUNCTION (CA2001H0).
--   CA2001H1  A SUBUNIT PACKAGE BODY.
--   CA2001H2  A LIBRARY FUNCTION (CA2001H0).
--   CA2001H3M THE MAIN PROCEDURE.

-- WKB 6/25/81
-- JRK 6/26/81
-- SPS 11/2/82
-- JBG 8/25/83

with Report, Ca2001h0;
use Report;
procedure Ca2001h3m is

   I : Integer := -1;

begin
   Test
     ("CA2001H",
      "IF A BODY_STUB IS DELETED FROM A COMPILATION " &
      "UNIT, THE PREVIOUSLY EXISTING SUBUNIT CAN NO " &
      "LONGER BE ACCESSED");

   I := Ca2001h0;

   if I = 1 then
      Failed ("SUBUNIT ACCESSED");
   end if;

   if I = 0 then
      Failed ("OLD LIBRARY UNIT ACCESSED");
   end if;

   if I /= 2 then
      Failed ("NEW LIBRARY UNIT NOT ACCESSED");
   end if;

   Result;
end Ca2001h3m;
