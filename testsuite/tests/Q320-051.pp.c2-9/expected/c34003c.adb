-- C34003C.ADA

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
-- FOR DERIVED FLOATING POINT TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 9/4/86
-- GJD 11/15/95 REMOVED USES OF OBSOLETE ADA 83 ATTRIBUTE (SAFE_LARGE).

with Report; use Report;

procedure C34003c is

   type Parent is digits 5;

   type T is
     new Parent digits 4 range
       Parent (Ident_Int (-30)) ..
         Parent (Ident_Int (30));

   subtype Subparent is Parent digits 4 range -30.0 .. 30.0;

   type S is new Subparent;

   X : T;
   Y : S;

begin
   Test
     ("C34003C",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "FLOATING POINT TYPES");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if T'Base'Digits < 5 or S'Base'Digits < 5 then
      Failed ("INCORRECT 'BASE'DIGITS");
   end if;

   if 12_344.0 + T'(1.0) + 1.0 /= 12_346.0 or
     12_344.0 + S'(1.0) + 1.0 /= 12_346.0 or
     -12_344.0 - T'(1.0) - 1.0 /= -12_346.0 or
     -12_344.0 - S'(1.0) - 1.0 /= -12_346.0
   then
      Failed ("INCORRECT + OR -");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'Digits /= 4 or S'Digits /= 4 then
      Failed ("INCORRECT 'DIGITS");
   end if;

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
      X := -31.0;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := -31.0");
      if X = -31.0 then  -- USE X.
         Comment ("X ALTERED -- X := -31.0");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := -31.0");
   end;

   begin
      X := 31.0;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := 31.0");
      if X = 31.0 then  -- USE X.
         Comment ("X ALTERED -- X := 31.0");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := 31.0");
   end;

   begin
      Y := -31.0;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := -31.0");
      if Y = -31.0 then  -- USE Y.
         Comment ("Y ALTERED -- Y := -31.0");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := -31.0");
   end;

   begin
      Y := 31.0;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := 31.0");
      if Y = 31.0 then  -- USE Y.
         Comment ("Y ALTERED -- Y := 31.0");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := 31.0");
   end;

   Result;
end C34003c;
