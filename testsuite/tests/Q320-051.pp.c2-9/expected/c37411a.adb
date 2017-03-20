-- C37411A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE OPERATIONS OF ASSIGNMENT, COMPARISON, MEMBERSHIP
--     TESTS, QUALIFICATION, TYPE CONVERSION, 'BASE, 'SIZE AND 'ADDRESS,
--     ARE DEFINED FOR NULL RECORDS.

-- HISTORY:
--     DHH 03/04/88 CREATED ORIGINAL TEST.
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

with System; use System;
with Report; use Report;
procedure C37411a is
   type S is record
      null;
   end record;

   subtype Ss is S;

   U, V, W : S;
   X       : Ss;

begin

   Test
     ("C37411A",
      "CHECK THAT THE OPERATIONS OF ASSIGNMENT, " &
      "COMPARISON, MEMBERSHIP TESTS, QUALIFICATION, " &
      "TYPE CONVERSION, 'BASE, 'SIZE AND 'ADDRESS, " &
      "ARE DEFINED FOR NULL RECORDS");
   U := W;
   if U /= W then
      Failed ("EQUALITY/ASSIGNMENT DOES NOT PERFORM CORRECTLY");
   end if;

   if V not in S then
      Failed ("MEMBERSHIP DOES NOT PERFORM CORRECTLY");
   end if;

   if X /= Ss (V) then
      Failed ("TYPE CONVERSION DOES NOT PERFORM CORRECTLY");
   end if;

   if S'(U) /= S'(W) then
      Failed ("QUALIFIED EXPRESSION DOES NOT PERFORM CORRECTLY");
   end if;

   if X'Size /= V'Size then
      Failed
        ("'BASE'SIZE DOES NOT PERFORM CORRECTLY WHEN PREFIX " &
         "IS AN OBJECT");
   end if;

   if X'Address = V'Address then
      Comment ("NULL RECORDS HAVE THE SAME ADDRESS");
   else
      Comment ("NULL RECORDS DO NOT HAVE THE SAME ADDRESS");
   end if;

   Result;
end C37411a;
