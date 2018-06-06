-- C34005O.ADA

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
--     FOR DERIVED MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT TYPE
--     IS A NON-LIMITED TYPE:
--     CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR
--     THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--     CONSTRAINED.
--     CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--     IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 9/17/86  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C34005o is

   subtype Component is Integer;

   package Pkg is

      First : constant := 0;
      Last  : constant := 10;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>, Index range <>) of Component;

      function Create
        (F1, L1 : Index; F2, L2 : Index; C : Component;
         Dummy  : Parent   -- TO RESOLVE OVERLOADING.
        )
         return Parent;

   end Pkg;

   use Pkg;

   type T is
     new Parent (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   subtype Subparent is Parent (4 .. 5, 6 .. 8);

   type S is new Subparent;

   X : T := (others => (others => 2));
   Y : S := (others => (others => 2));

   package body Pkg is

      function Create
        (F1, L1 : Index; F2, L2 : Index; C : Component; Dummy : Parent)
         return Parent
      is
         A : Parent (F1 .. L1, F2 .. L2);
         B : Component := C;
      begin
         for I in F1 .. L1 loop
            for J in F2 .. L2 loop
               A (I, J) := B;
               B        := B + 1;
            end loop;
         end loop;
         return A;
      end Create;

   end Pkg;

begin
   Test
     ("C34005O",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A NON-LIMITED TYPE");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   begin
      if Create (6, 9, 2, 3, 1, X) /= ((1, 2), (3, 4), (5, 6), (7, 8)) or
        Create (6, 9, 2, 3, 1, Y) /= ((1, 2), (3, 4), (5, 6), (7, 8)) then
         Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE " & "SUBTYPE");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION");
   end;

   if ((1, 2), (3, 4), (5, 6), (7, 8)) in T or
     ((1, 2), (3, 4), (5, 6), (7, 8)) in S then
      Failed ("INCORRECT ""IN""");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= 4 or T'Last /= 5 or S'First /= 4 or S'Last /= 5 or
     T'First (2) /= 6 or T'Last (2) /= 8 or S'First (2) /= 6 or S'Last (2) /= 8
   then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := ((1, 2, 3), (4, 5, 6));
      Y := ((1, 2, 3), (4, 5, 6));
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := (4 => (6 .. 8 => 0));
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "X := (4 => (6 .. 8 => 0))");
      if X = (4 => (6 .. 8 => 0)) then  -- USE X.
         Comment ("X ALTERED -- " & "X := (4 => (6 .. 8 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "X := (4 => (6 .. 8 => 0))");
   end;

   begin
      X := (4 .. 6 => (6 .. 8 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "X := (4 .. 6 => (6 .. 8 => 0))");
      if X = (4 .. 6 => (6 .. 8 => 0)) then  -- USE X.
         Comment ("X ALTERED -- " & "X := (4 .. 6 => (6 .. 8 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "X := (4 .. 6 => (6 .. 8 => 0))");
   end;

   begin
      X := (4 .. 5 => (6 .. 7 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "X := (4 .. 5 => (6 .. 7 => 0))");
      if X = (4 .. 5 => (6 .. 7 => 0)) then  -- USE X.
         Comment ("X ALTERED -- " & "X := (4 .. 5 => (6 .. 7 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "X := (4 .. 5 => (6 .. 7 => 0))");
   end;

   begin
      X := (4 .. 5 => (6 .. 9 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "X := (4 .. 5 => (6 .. 9 => 0))");
      if X = (4 .. 5 => (6 .. 9 => 0)) then  -- USE X.
         Comment ("X ALTERED -- " & "X := (4 .. 5 => (6 .. 9 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "X := (4 .. 5 => (6 .. 9 => 0))");
   end;

   begin
      Y := (4 => (6 .. 8 => 0));
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := (4 => (6 .. 8 => 0))");
      if Y = (4 => (6 .. 8 => 0)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "Y := (4 => (6 .. 8 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "Y := (4 => (6 .. 8 => 0))");
   end;

   begin
      Y := (4 .. 6 => (6 .. 8 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := (4 .. 6 => (6 .. 8 => 0))");
      if Y = (4 .. 6 => (6 .. 8 => 0)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "Y := (4 .. 6 => (6 .. 8 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "Y := (4 .. 6 => (6 .. 8 => 0))");
   end;

   begin
      Y := (4 .. 5 => (6 .. 7 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := (4 .. 5 => (6 .. 7 => 0))");
      if Y = (4 .. 5 => (6 .. 7 => 0)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "Y := (4 .. 5 => (6 .. 7 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "Y := (4 .. 5 => (6 .. 7 => 0))");
   end;

   begin
      Y := (4 .. 5 => (6 .. 9 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := (4 .. 5 => (6 .. 9 => 0))");
      if Y = (4 .. 5 => (6 .. 9 => 0)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "Y := (4 .. 5 => (6 .. 9 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "Y := (4 .. 5 => (6 .. 9 => 0))");
   end;

   Result;
end C34005o;
