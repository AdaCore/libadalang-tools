-- C34001F.ADA

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
-- FOR DERIVED BOOLEAN TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 8/20/86

with Report; use Report;

procedure C34001f is

   subtype Parent is Boolean;

   type T is
     new Parent range
         Parent'Val (Ident_Int (Parent'Pos (False))) ..
           Parent'Val (Ident_Int (Parent'Pos (False)));

   subtype Subparent is Parent range True .. True;

   type S is new Subparent;

   X : T;
   Y : S;

begin
   Test
     ("C34001F",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "BOOLEAN TYPES");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if T'Base'First /= False or
     T'Base'Last /= True or
     S'Base'First /= False or
     S'Base'Last /= True
   then
      Failed ("INCORRECT 'BASE'FIRST OR 'BASE'LAST");
   end if;

   if T'Pred (True) /= False or
     T'Succ (False) /= True or
     S'Pred (True) /= False or
     S'Succ (False) /= True
   then
      Failed ("INCORRECT 'PRED OR 'SUCC");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= False or
     T'Last /= False or
     S'First /= True or
     S'Last /= True
   then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := False;
      Y := True;
      if not Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := True;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := TRUE");
      if X = True then  -- USE X.
         Comment ("X ALTERED -- X := TRUE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := TRUE");
   end;

   begin
      Y := False;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := FALSE");
      if Y = False then  -- USE Y.
         Comment ("Y ALTERED -- Y := FALSE");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := FALSE");
   end;

   Result;
end C34001f;
