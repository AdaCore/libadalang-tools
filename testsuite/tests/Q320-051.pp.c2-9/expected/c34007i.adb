-- C34007I.ADA

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
-- FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS A MULTI-DIMENSIONAL ARRAY
-- TYPE:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 9/25/86

with Report; use Report;

procedure C34007i is

   subtype Component is Integer;

   type Designated is array (Natural range <>, Natural range <>) of Component;

   subtype Subdesignated is Designated (4 .. 5, 6 .. 8);

   package Pkg is

      type Parent is access Designated;

      function Create
        (F1, L1 : Natural;
         F2, L2 : Natural;
         C      : Component;
         Dummy  : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

   end Pkg;

   use Pkg;

   type T is
     new Parent
       (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   subtype Subparent is Parent (4 .. 5, 6 .. 8);

   type S is new Subparent;

   X : T := new Subdesignated'(others => (others => 2));
   Y : S := new Subdesignated'(others => (others => 2));

   package body Pkg is

      function Create
        (F1, L1 : Natural;
         F2, L2 : Natural;
         C      : Component;
         Dummy  : Parent) return Parent
      is
         A : Parent    := new Designated (F1 .. L1, F2 .. L2);
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
     ("C34007I",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "MULTI-DIMENSIONAL ARRAY TYPE");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if Create (6, 9, 2, 3, 1, X).all /= ((1, 2), (3, 4), (5, 6), (7, 8)) or
     Create (6, 9, 2, 3, 1, Y).all /= ((1, 2), (3, 4), (5, 6), (7, 8))
   then
      Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE SUBTYPE");
   end if;

   if Create (6, 9, 2, 3, 1, X) in T or Create (6, 9, 2, 3, 1, Y) in S then
      Failed ("INCORRECT ""IN""");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if X'First /= 4 or
     X'Last /= 5 or
     Y'First /= 4 or
     Y'Last /= 5 or
     X'First (2) /= 6 or
     X'Last (2) /= 8 or
     Y'First (2) /= 6 or
     Y'Last (2) /= 8
   then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      X := new Subdesignated'((1, 2, 3), (4, 5, 6));
      Y := new Subdesignated'((1, 2, 3), (4, 5, 6));
      if Parent (X) = Parent (Y) or  -- USE X AND Y.
      X.all /= Y.all then
         Failed ("INCORRECT ALLOCATOR OR CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := new Designated'(5 .. 6 => (6 .. 8 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "X := NEW DESIGNATED'(5 .. 6 => (6 .. 8 => 0))");
      if X = null or else X.all = ((0, 0, 0), (0, 0, 0)) then  -- USE X.
         Comment
           ("X ALTERED -- " &
            "X := NEW DESIGNATED'(5 .. 6 => " &
            "(6 .. 8 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "X := NEW DESIGNATED'(5 .. 6 => (6 .. 8 => 0))");
   end;

   begin
      X := new Designated'(4 .. 5 => (5 .. 7 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "X := NEW DESIGNATED'(4 .. 5 => (5 .. 7 => 0))");
      if X = null or else X.all = ((0, 0, 0), (0, 0, 0)) then  -- USE X.
         Comment
           ("X ALTERED -- " &
            "X := NEW DESIGNATED'(4 .. 5 => " &
            "(5 .. 7 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "X := NEW DESIGNATED'(4 .. 5 => (5 .. 7 => 0))");
   end;

   begin
      Y := new Designated'(5 .. 6 => (6 .. 8 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "Y := NEW DESIGNATED'(5 .. 6 => (6 .. 8 => 0))");
      if Y = null or else Y.all = ((0, 0, 0), (0, 0, 0)) then  -- USE Y.
         Comment
           ("Y ALTERED -- " &
            "Y := NEW DESIGNATED'(5 .. 6 => " &
            "(6 .. 8 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "Y := NEW DESIGNATED'(5 .. 6 => (6 .. 8 => 0))");
   end;

   begin
      Y := new Designated'(4 .. 5 => (5 .. 7 => 0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "Y := NEW DESIGNATED'(4 .. 5 => (5 .. 7 => 0))");
      if Y = null or else Y.all = ((0, 0, 0), (0, 0, 0)) then  -- USE Y.
         Comment
           ("Y ALTERED -- " &
            "Y := NEW DESIGNATED'(4 .. 5 => " &
            "(5 .. 7 => 0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "Y := NEW DESIGNATED'(4 .. 5 => (5 .. 7 => 0))");
   end;

   Result;
end C34007i;
