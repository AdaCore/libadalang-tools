-- C34005I.ADA

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
--     CHARACTER TYPE:
--     CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR
--     THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--     CONSTRAINED.
--     CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--     IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 9/15/86  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C34005i is

   type Component is new Character;

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

   X : T := (others => 'B');
   Y : S := (others => 'B');

   package body Pkg is

      function Create (F, L : Index; C : Component;
         Dummy              : Parent) return Parent
      is
         A : Parent (F .. L);
         B : Component := C;
      begin
         for I in F .. L loop
            A (I) := B;
            B     := Component'Succ (B);
         end loop;
         return A;
      end Create;

   end Pkg;

begin
   Test
     ("C34005I",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A CHARACTER TYPE");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   begin
      if Create (2, 3, 'D', X) /= "DE" or Create (2, 3, 'D', Y) /= "DE" then
         Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE " & "SUBTYPE");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION");
   end;

   if X & "CD" /= "BBBCD" or Y & "CD" /= "BBBCD" then
      Failed ("INCORRECT &");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= 5 or T'Last /= 7 or S'First /= 5 or S'Last /= 7 then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := "ABC";
      Y := "ABC";
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := "AB";
      Failed ("CONSTRAINT_ERROR NOT RAISED -- X := ""AB""");
      if X = "AB" then  -- USE X.
         Comment ("X ALTERED -- X := ""AB""");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- X := ""AB""");
   end;

   begin
      X := "ABCD";
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "X := ""ABCD""");
      if X = "ABCD" then  -- USE X.
         Comment ("X ALTERED -- X := ""ABCD""");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "X := ""ABCD""");
   end;

   begin
      Y := "AB";
      Failed ("CONSTRAINT_ERROR NOT RAISED -- Y := ""AB""");
      if Y = "AB" then  -- USE Y.
         Comment ("Y ALTERED -- Y := ""AB""");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- Y := ""AB""");
   end;

   begin
      Y := "ABCD";
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := ""ABCD""");
      if Y = "ABCD" then  -- USE Y.
         Comment ("Y ALTERED -- Y := ""ABCD""");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "Y := ""ABCD""");
   end;

   Result;
end C34005i;
