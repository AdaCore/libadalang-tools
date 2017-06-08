-- C34001C.ADA

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
-- FOR DERIVED ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 8/20/86

with Report; use Report;

procedure C34001c is

   type Parent is (E1, E2, E3, 'A', E4, E5, E6);

   type T is
     new Parent range
       Parent'Val (Ident_Int (Parent'Pos (E3))) ..
         Parent'Val (Ident_Int (Parent'Pos (E4)));

   subtype Subparent is Parent range E3 .. E4;

   type S is new Subparent;

   X : T;
   Y : S;

begin
   Test
     ("C34001C",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "ENUMERATION TYPES, EXCLUDING BOOLEAN TYPES");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if T'Base'First /= E1 or
     T'Base'Last /= E6 or
     S'Base'First /= E1 or
     S'Base'Last /= E6
   then
      Failed ("INCORRECT 'BASE'FIRST OR 'BASE'LAST");
   end if;

   if T'Pred (E2) /= E1 or
     T'Succ (E1) /= E2 or
     S'Pred (E2) /= E1 or
     S'Succ (E1) /= E2
   then
      Failed ("INCORRECT 'PRED OR 'SUCC");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= E3 or T'Last /= E4 or S'First /= E3 or S'Last /= E4 then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := E3;
      Y := E3;
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT - 1");
      end if;
      X := E4;
      Y := E4;
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT - 2");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := E2;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := E2");
      if X = E2 then  -- USE X.
         Comment ("X ALTERED -- X := E2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := E2");
   end;

   begin
      X := E5;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := E5");
      if X = E5 then  -- USE X.
         Comment ("X ALTERED -- X := E5");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := E5");
   end;

   begin
      Y := E2;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := E2");
      if Y = E2 then  -- USE Y.
         Comment ("Y ALTERED -- Y := E2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := E2");
   end;

   begin
      Y := E5;
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := E5");
      if Y = E5 then  -- USE Y.
         Comment ("Y ALTERED -- Y := E5");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := E5");
   end;

   Result;
end C34001c;
