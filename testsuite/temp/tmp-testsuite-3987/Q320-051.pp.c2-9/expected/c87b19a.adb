-- C87B19A.ADA

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
--     CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
--     SIMPLE EXPRESSIONS AND RANGE BOUNDS OF VARIANT CHOICES MUST MATCH
--     THE TYPE OF THE DISCRIMINANT'S EXPLICIT TYPEMARK.

--HISTORY:
--     DSJ 06/15/83 CREATED ORIGINAL TEST.
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

with Report; use Report;

procedure C87b19a is

   type Color is (Yellow, Red, Blue, Green, Brown);
   type School is (Yale, Harvard, Princeton, Brown, Stanford);
   type Cook is (Broil, Bake, Brown, Toast, Fry);
   type Mixed is (Green, Brown, Yale, Bake, Blue, Fry);

   Rating : Integer := 0;

   function Ok return Boolean is
   begin
      Rating := Rating + 1;
      return False;
   end Ok;

   function Err return Boolean is
   begin
      Failed ("VARIANT CHOICES MUST MATCH TYPE OF DISCRIMINANT");
      return False;
   end Err;

begin
   Test
     ("C87B19A",
      "OVERLOADED EXPRESSIONS AND RANGE BOUNDS" & " OF VARIANT CHOICES");
   declare

      type Rec (X : Mixed := Brown) is record
         case X is
            when Green .. Brown =>
               null;
            when Blue =>
               null;
            when Fry =>
               null;
            when Yale =>
               null;
            when others =>
               null;
         end case;
      end record;

      R1 : Rec (X => Fry);
      R2 : Rec (X => Blue);
      R3 : Rec (X => Bake);
      R4 : Rec (X => Yale);
      R5 : Rec (X => Brown);
      R6 : Rec (X => Green);

   begin
      if Mixed'Pos (R1.X) /= 5 then
         Failed ("VARIANT CHOICES MUST MATCH TYPE OF " & "DISCRIMINANT-R1");
      end if;
      if Mixed'Pos (R2.X) /= 4 then
         Failed ("VARIANT CHOICES MUST MATCH TYPE OF " & "DISCRIMINANT-R2");
      end if;
      if Mixed'Pos (R3.X) /= 3 then
         Failed ("VARIANT CHOICES MUST MATCH TYPE OF " & "DISCRIMINANT-R3");
      end if;
      if Mixed'Pos (R4.X) /= 2 then
         Failed ("VARIANT CHOICES MUST MATCH TYPE OF " & "DISCRIMINANT-R4");
      end if;
      if Mixed'Pos (R5.X) /= 1 then
         Failed ("VARIANT CHOICES MUST MATCH TYPE OF " & "DISCRIMINANT-R5");
      end if;
      if Mixed'Pos (R6.X) /= 0 then
         Failed ("VARIANT CHOICES MUST MATCH TYPE OF " & "DISCRIMINANT-R6");
      end if;

   end;

   Result;
end C87b19a;
