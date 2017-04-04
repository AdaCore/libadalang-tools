-- C87B29A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- AGGREGATES CONTAINING A SINGLE COMPONENT ASSOCIATION MUST USE ONLY NAMED
-- NOTATION.

-- TRH  4 AUG 82

with Report; use Report;

procedure C87b29a is

   type Vector is array (1 .. 1) of Integer;

   type Rec is record
      X : Integer;
   end record;

   Err : Boolean := False;

   procedure P1 (X : Integer) is
   begin
      null;
   end P1;

   procedure P1 (X : Vector) is
   begin
      Err := True;
   end P1;

   procedure P1 (X : Rec) is
   begin
      Err := True;
   end P1;

begin
   Test
     ("C87B29A",
      "AGGREGATES CONTAINING A SINGLE COMPONENT " &
      "ASSOCIATION MUST USE NAMED NOTATION");

   P1 ((0));  -- INTEGER PARAMETER, NOT AN AGGREGATE PARAMETER

   if Err then
      Failed
        ("RESOLUTION INCORRECT - AGGREGATES WITH A SINGLE " &
         "COMPONENT ASSOCIATION MUST USE NAMED NOTATION");
   end if;

   Result;
end C87b29a;
