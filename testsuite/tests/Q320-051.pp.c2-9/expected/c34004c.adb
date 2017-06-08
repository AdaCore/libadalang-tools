-- C34004C.ADA

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
--     FOR DERIVED FIXED POINT TYPES:

--     CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR
--     THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--     CONSTRAINED.

--     CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--     IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 09/08/86
--     JLH 09/25/87  REFORMATTED HEADER.
--     JRL 03/13/92  MODIFIED TO DEFEAT OPTIMIZATION WHEN ATTEMPTING TO
--                   RAISE CONSTRAINT_ERROR.
--     JRL 03/30/93  REMOVED NUMERIC_ERROR FROM TEST.
--     DTN 11/30/95  REMOVED NON ADA95 ATTRIBUTES.

with System; use System;
with Report; use Report;

procedure C34004c is

   type Parent is delta 0.01 range -100.0 .. 100.0;

   type T is
     new Parent delta 0.1 range
       Ident_Int (1) * (-30.0) ..
         Ident_Int (1) * (30.0);

   subtype Subparent is Parent delta 0.1 range -30.0 .. 30.0;

   type S is new Subparent;

   X, Xa : T;
   Y, Ya : S;

   function Out_Of_Bounds (Var1, Var2 : T) return Boolean is
   begin
      if (Var1 + Var2) in T then
         return False;
      else
         return True;
      end if;
   exception
      when Constraint_Error =>
         return True;
   end Out_Of_Bounds;

   function Out_Of_Bounds (Var1, Var2 : S) return Boolean is
   begin
      if (Var1 + Var2) in S then
         return False;
      else
         return True;
      end if;
   exception
      when Constraint_Error =>
         return True;
   end Out_Of_Bounds;

begin
   Test
     ("C34004C",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "FIXED POINT TYPES");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   declare
      Tbd : constant := Boolean'Pos (T'Base'Delta <= 0.01);
      Sbd : constant := Boolean'Pos (S'Base'Delta <= 0.01);
   begin
      if Tbd = 0 or Sbd = 0 then
         Failed ("INCORRECT 'BASE'DELTA");
      end if;
   end;

   declare
      N : Integer := Ident_Int (8);
   begin
      if 98.0 + T'(1.0) + N * 0.007_812_5 /= 99.062_5 or
        98.0 + S'(1.0) + 8 * 0.007_812_5 /= 99.062_5 or
        -98.0 - T'(1.0) - N * 0.007_812_5 /= -99.062_5 or
        -98.0 - S'(1.0) - 8 * 0.007_812_5 /= -99.062_5
      then
         Failed ("INCORRECT + OR -");
      end if;
   end;

   if T'First /= -30.0 or
     T'Last /= 30.0 or
     S'First /= -30.0 or
     S'Last /= 30.0
   then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := -30.0;
      Y := -30.0;
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT - 1");
      end if;
      X := 30.0;
      Y := 30.0;
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT - 2");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X  := -30.0;
      Xa := -0.062_5;
      if not Out_Of_Bounds (X, Xa) then
         Failed ("CONSTRAINT_ERROR NOT RAISED -- X := -30.0625");
      end if;
   exception
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := -30.0625");
   end;

   begin
      X  := 30.0;
      Xa := 0.062_5;
      if not Out_Of_Bounds (X, Xa) then
         Failed ("CONSTRAINT_ERROR NOT RAISED -- X := 30.0625");
      end if;
   exception
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := 30.0625");
   end;

   begin
      Y  := -30.0;
      Ya := -0.062_5;
      if not Out_Of_Bounds (Y, Ya) then
         Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := -30.0625");
      end if;
   exception
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := -30.0625");
   end;

   begin
      Y  := 30.0;
      Ya := 0.062_5;
      if not Out_Of_Bounds (Y, Ya) then
         Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := 30.0625");
      end if;
   exception
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := 30.0625");
   end;

   Result;
end C34004c;
