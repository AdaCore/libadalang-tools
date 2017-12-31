-- C34005C.ADA

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
--     FOR DERIVED ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT TYPE IS A
--     NON-LIMITED, NON-DISCRETE TYPE:
--     CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR
--     THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--     CONSTRAINED.
--     CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--     IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 9/10/86  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C34005c is

   subtype Component is Float;

   package Pkg is

      First : constant := 0;
      Last  : constant := 100;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>) of Component;

      function Create (F, L : Index; C : Component;
         Dummy              : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Int (5) .. Ident_Int (7));

   subtype Subparent is Parent (5 .. 7);

   type S is new Subparent;

   X : T := (others => 2.0);
   Y : S := (others => 2.0);

   package body Pkg is

      function Create (F, L : Index; C : Component;
         Dummy              : Parent) return Parent
      is
         A : Parent (F .. L);
         B : Component := C;
      begin
         for I in F .. L loop
            A (I) := B;
            B     := B + 1.0;
         end loop;
         return A;
      end Create;

   end Pkg;

begin
   Test
     ("C34005C",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A NON-LIMITED, NON-DISCRETE TYPE");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   begin
      if Create (2, 3, 4.0, X) /= (4.0, 5.0) or
        Create (2, 3, 4.0, Y) /= (4.0, 5.0) then
         Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE " & "SUBTYPE");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION");
   end;

   if X & (3.0, 4.0) /= (2.0, 2.0, 2.0, 3.0, 4.0) or
     Y & (3.0, 4.0) /= (2.0, 2.0, 2.0, 3.0, 4.0) then
      Failed ("INCORRECT &");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= 5 or T'Last /= 7 or S'First /= 5 or S'Last /= 7 then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := (1.0, 2.0, 3.0);
      Y := (1.0, 2.0, 3.0);
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := (1.0, 2.0);
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := (1.0, 2.0)");
      if X = (1.0, 2.0) then  -- USE X.
         Comment ("X ALTERED -- X := (1.0, 2.0)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := (1.0, 2.0)");
   end;

   begin
      X := (1.0, 2.0, 3.0, 4.0);
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "X := (1.0, 2.0, 3.0, 4.0)");
      if X = (1.0, 2.0, 3.0, 4.0) then  -- USE X.
         Comment ("X ALTERED -- X := (1.0, 2.0, 3.0, 4.0)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "X := (1.0, 2.0, 3.0, 4.0)");
   end;

   begin
      Y := (1.0, 2.0);
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := (1.0, 2.0)");
      if Y = (1.0, 2.0) then  -- USE Y.
         Comment ("Y ALTERED -- Y := (1.0, 2.0)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := (1.0, 2.0)");
   end;

   begin
      Y := (1.0, 2.0, 3.0, 4.0);
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := (1.0, 2.0, 3.0, 4.0)");
      if Y = (1.0, 2.0, 3.0, 4.0) then  -- USE Y.
         Comment ("Y ALTERED -- Y := (1.0, 2.0, 3.0, 4.0)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "Y := (1.0, 2.0, 3.0, 4.0)");
   end;

   Result;
end C34005c;
