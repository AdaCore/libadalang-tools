-- C34005U.ADA

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
--     FOR DERIVED MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT TYPE IS
--     A LIMITED TYPE:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 08/21/87  CREATED ORIGINAL TEST.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with Report; use Report;

procedure C34005u is

   package Pkg_L is

      type Lp is limited private;

      function Create (X : Integer) return Lp;

      function Value (X : Lp) return Integer;

      function Equal (X, Y : Lp) return Boolean;

      procedure Assign (X : out Lp; Y : Lp);

      C1 : constant Lp;
      C2 : constant Lp;
      C3 : constant Lp;
      C4 : constant Lp;
      C5 : constant Lp;
      C6 : constant Lp;
      C7 : constant Lp;
      C8 : constant Lp;

   private

      type Lp is new Integer;

      C1 : constant Lp := 1;
      C2 : constant Lp := 2;
      C3 : constant Lp := 3;
      C4 : constant Lp := 4;
      C5 : constant Lp := 5;
      C6 : constant Lp := 6;
      C7 : constant Lp := 7;
      C8 : constant Lp := 8;

   end Pkg_L;

   use Pkg_L;

   subtype Component is Lp;

   package Pkg_P is

      First : constant := 0;
      Last  : constant := 10;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>, Index range <>) of Component;

      function Create (F1, L1 : Index; F2, L2 : Index; C : Component;
         Dummy                : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Equal (X, Y : Parent) return Boolean;

      function Aggr (A, B, C, D, E, F, G, H : Component) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is
     new Parent (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   subtype Subparent is Parent (4 .. 5, 6 .. 8);

   type S is new Subparent;

   X : T;
   Y : S;

   package body Pkg_L is

      function Create (X : Integer) return Lp is
      begin
         return Lp (Ident_Int (X));
      end Create;

      function Value (X : Lp) return Integer is
      begin
         return Integer (X);
      end Value;

      function Equal (X, Y : Lp) return Boolean is
      begin
         return X = Y;
      end Equal;

      procedure Assign (X : out Lp; Y : Lp) is
      begin
         X := Y;
      end Assign;

   end Pkg_L;

   package body Pkg_P is

      function Create (F1, L1 : Index; F2, L2 : Index; C : Component;
         Dummy                : Parent) return Parent
      is
         B : Component;
      begin
         return A : Parent (F1 .. L1, F2 .. L2) do
            Assign (B, C);
            for I in F1 .. L1 loop
               for J in F2 .. L2 loop
                  Assign (A (I, J), B);
                  Assign (B, Create (Value (B) + 1));
               end loop;
            end loop;
         end return;
      end Create;

      function Equal (X, Y : Parent) return Boolean is
      begin
         if X'Length /= Y'Length or X'Length (2) /= Y'Length (2) then
            return False;
         else
            for I in X'Range loop
               for J in X'Range (2) loop
                  if not Equal
                      (X (I, J),
                       Y (I - X'First + Y'First,
                          J - X'First (2) + Y'First (2)))
                  then
                     return False;
                  end if;
               end loop;
            end loop;
         end if;
         return True;
      end Equal;

      function Aggr (A, B, C, D, E, F, G, H : Component) return Parent is
      begin
         return
           X : Parent (Index'First .. Index'First + 3,
              Index'First .. Index'First + 1)
         do
            Assign (X (Index'First, Index'First), A);
            Assign (X (Index'First, Index'First + 1), B);
            Assign (X (Index'First + 1, Index'First), C);
            Assign (X (Index'First + 1, Index'First + 1), D);
            Assign (X (Index'First + 2, Index'First), E);
            Assign (X (Index'First + 2, Index'First + 1), F);
            Assign (X (Index'First + 3, Index'First), G);
            Assign (X (Index'First + 3, Index'First + 1), H);
         end return;
      end Aggr;

   end Pkg_P;

   procedure Assign (X : in out T; Y : T) is
   begin
      for I in X'Range loop
         for J in X'Range (2) loop
            Assign (X (I, J), Y (I, J));
         end loop;
      end loop;
   end Assign;

   procedure Assign (X : in out S; Y : S) is
   begin
      for I in X'Range loop
         for J in X'Range (2) loop
            Assign (X (I, J), Y (I, J));
         end loop;
      end loop;
   end Assign;

begin
   Test
     ("C34005U",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "MULTI-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A LIMITED TYPE");

   for I in X'Range loop
      for J in X'Range (2) loop
         Assign (X (I, J), C2);
         Assign (Y (I, J), C2);
      end loop;
   end loop;

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.
   begin
      if not Equal
          (Create (6, 9, 2, 3, C1, X),
           Aggr (C1, C2, C3, C4, C5, C6, C7, C8)) or
        not Equal
          (Create (6, 9, 2, 3, C1, Y), Aggr (C1, C2, C3, C4, C5, C6, C7, C8))
      then
         Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE " & "SUBTYPE");
      end if;
   exception
      when Constraint_Error =>
         Failed
           ("CONSTRAINT_ERROR WHEN TRYING TO CREATE BASE " &
            "TYPE VALUES OUTSIDE THE SUBTYPE");
      when others =>
         Failed
           ("EXCEPTION WHEN TRYING TO CREATE BASE TYPE " &
            "VALUES OUTSIDE THE SUBTYPE");
   end;

   if Aggr (C1, C2, C3, C4, C5, C6, C7, C8) in T or
     Aggr (C1, C2, C3, C4, C5, C6, C7, C8) in S then
      Failed ("INCORRECT ""IN""");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= 4 or T'Last /= 5 or S'First /= 4 or S'Last /= 5 or
     T'First (2) /= 6 or T'Last (2) /= 8 or S'First (2) /= 6 or S'Last (2) /= 8
   then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      Assign (X, Create (4, 5, 6, 8, C1, X));
      Assign (Y, Create (4, 5, 6, 8, C1, Y));
      if not Equal (Parent (X), Parent (Y)) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGN CALL");
   end;

   begin
      Assign (X, Create (4, 4, 6, 8, C1, X));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, CREATE (4, 4, 6, 8, C1, X))");
      if Equal (X, Create (4, 4, 6, 8, C1, X)) then  -- USE X.
         Comment ("X ALTERED -- " & "ASSIGN (X, CREATE (4, 4, 6, 8, C1, X))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, CREATE (4, 4, 6, 8, C1, X))");
   end;

   begin
      Assign (X, Create (4, 6, 6, 8, C1, X));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, CREATE (4, 6, 6, 8, C1, X))");
      if Equal (X, Create (4, 6, 6, 8, C1, X)) then  -- USE X.
         Comment ("X ALTERED -- " & "ASSIGN (X, CREATE (4, 6, 6, 8, C1, X))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, CREATE (4, 6, 6, 8, C1, X))");
   end;

   begin
      Assign (X, Create (4, 5, 6, 7, C1, X));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, CREATE (4, 5, 6, 7, C1, X))");
      if Equal (X, Create (4, 5, 6, 7, C1, X)) then  -- USE X.
         Comment ("X ALTERED -- " & "ASSIGN (X, CREATE (4, 5, 6, 7, C1, X))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, CREATE (4, 5, 6, 7, C1, X))");
   end;

   begin
      Assign (X, Create (4, 5, 6, 9, C1, X));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, CREATE (4, 5, 6, 9, C1, X))");
      if Equal (X, Create (4, 5, 6, 9, C1, X)) then  -- USE X.
         Comment ("X ALTERED -- " & "ASSIGN (X, CREATE (4, 5, 6, 9, C1, X))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, CREATE (4, 5, 6, 9, C1, X))");
   end;

   begin
      Assign (Y, Create (4, 4, 6, 8, C1, Y));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, CREATE (4, 4, 6, 8, C1, Y))");
      if Equal (Y, Create (4, 4, 6, 8, C1, Y)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "ASSIGN (Y, CREATE (4, 4, 6, 8, C1, Y))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, CREATE (4, 4, 6, 8, C1, Y))");
   end;

   begin
      Assign (Y, Create (4, 6, 6, 8, C1, Y));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, CREATE (4, 6, 6, 8, C1, Y))");
      if Equal (Y, Create (4, 6, 6, 8, C1, Y)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "ASSIGN (Y, CREATE (4, 6, 6, 8, C1, Y))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, CREATE (4, 6, 6, 8, C1, Y))");
   end;

   begin
      Assign (Y, Create (4, 5, 6, 7, C1, Y));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, CREATE (4, 5, 6, 7, C1, Y))");
      if Equal (Y, Create (4, 5, 6, 7, C1, Y)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "ASSIGN (Y, CREATE (4, 5, 6, 7, C1, Y))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, CREATE (4, 5, 6, 7, C1, Y))");
   end;

   begin
      Assign (Y, Create (4, 5, 6, 9, C1, Y));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, CREATE (4, 5, 6, 9, C1, Y))");
      if Equal (Y, Create (4, 5, 6, 9, C1, Y)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "ASSIGN (Y, CREATE (4, 5, 6, 9, C1, Y))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, CREATE (4, 5, 6, 9, C1, Y))");
   end;

   Result;
end C34005u;
