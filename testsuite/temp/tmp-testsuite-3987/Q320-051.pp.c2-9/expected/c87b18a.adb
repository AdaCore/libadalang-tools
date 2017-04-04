-- C87B18A.ADA

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
-- THE TYPES OF THE EXPRESSIONS IN A DISCRIMINANT CONSTRAINT IN
-- A SUBTYPE INDICATION MUST MATCH THE DISCRIMINANT'S EXPLICIT
-- TYPEMARK.

-- TRH  1 JULY 82

with Report; use Report;

procedure C87b18a is

   Err : Boolean := False;

   function F1 return Integer is
   begin
      return 1;
   end F1;

   function F1 return Float is
   begin
      Err := True;
      return 0.0;
   end F1;

   function F2 return Boolean is
   begin
      return True;
   end F2;

   function F2 return String is
   begin
      Err := True;
      return "STRING";
   end F2;

begin
   Test ("C87B18A", "OVERLOADED EXPRESSIONS IN DISCRIMINANT " & "CONSTRAINTS");

   declare
      type Rec (X : Integer := 0; Y : Boolean := True) is record
         null;
      end record;

      R1 : Rec (F1, F2);
      R2 : Rec (Y => F2, X => F1);

   begin
      if Err then
         Failed
           ("RESOLUTION INCORRECT - DISCRIMINANT " &
            "CONSTRAINT MUST MATCH DISCRIMINANT TYPE");
      end if;
   end;

   Result;
end C87b18a;
