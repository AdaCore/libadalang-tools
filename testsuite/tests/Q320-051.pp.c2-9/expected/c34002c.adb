-- C34002C.ADA

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
-- FOR DERIVED INTEGER TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 8/21/86

with Report; use Report;

procedure C34002c is

   type Parent is range -100 .. 100;

   type T is
     new Parent range
         Parent'Val (Ident_Int (-30)) ..
           Parent'Val (Ident_Int (30));

   subtype Subparent is Parent range -30 .. 30;

   type S is new Subparent;

   X : T;
   Y : S;

begin
   Test
     ("C34002C",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "INTEGER TYPES");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if T'Pos (T'Base'First) /= Parent'Pos (Parent'Base'First) or
     S'Pos (S'Base'First) /= Parent'Pos (Parent'Base'First) or
     T'Pos (T'Base'Last) /= Parent'Pos (Parent'Base'Last) or
     S'Pos (S'Base'Last) /= Parent'Pos (Parent'Base'Last)
   then
      Failed ("INCORRECT 'BASE'FIRST OR 'BASE'LAST");
   end if;

   if T'Pred (100) /= 99 or
     T'Succ (99) /= 100 or
     S'Pred (100) /= 99 or
     S'Succ (99) /= 100
   then
      Failed ("INCORRECT 'PRED OR 'SUCC");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= -30 or T'Last /= 30 or S'First /= -30 or S'Last /= 30 then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := -30;
      Y := -30;
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT - 1");
      end if;
      X := 30;
      Y := 30;
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT - 2");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := -31;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := -31");
      if X = -31 then  -- USE X.
         Comment ("X ALTERED -- X := -31");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := -31");
   end;

   begin
      X := 31;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := 31");
      if X = 31 then  -- USE X.
         Comment ("X ALTERED -- X := 31");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := 31");
   end;

   begin
      Y := -31;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := -31");
      if Y = -31 then -- USE Y.
         Comment ("Y ALTERED -- Y := -31");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := -31");
   end;

   begin
      Y := 31;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := 31");
      if Y = 31 then  -- USE Y.
         Comment ("Y ALTERED -- Y := 31");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := 31");
   end;

   Result;
end C34002c;
