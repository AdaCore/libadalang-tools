-- C34005R.ADA

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
--     LIMITED TYPE:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 08/19/87  CREATED ORIGINAL TEST.
--     VCL 07/01/88  ADDED EXCEPTION HANDLERS TO CATCH INCORRECT TYPE
--                   CONVERSIONS TO DERIVED SUBTYPES.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with Report; use Report;

procedure C34005r is

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

   private

      type Lp is new Integer;

      C1 : constant Lp := 1;
      C2 : constant Lp := 2;
      C3 : constant Lp := 3;
      C4 : constant Lp := 4;
      C5 : constant Lp := 5;

   end Pkg_L;

   use Pkg_L;

   subtype Component is Lp;

   package Pkg_P is

      First : constant := 0;
      Last  : constant := 100;

      subtype Index is Integer range First .. Last;

      type Parent is array (Index range <>) of Component;

      function Create
        (F, L  : Index;
         C     : Component;
         Dummy : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Equal (X, Y : Parent) return Boolean;

      function Aggr (X, Y : Component) return Parent;

      function Aggr (W, X, Y, Z : Component) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is new Parent (Ident_Int (5) .. Ident_Int (7));

   subtype Subparent is Parent (5 .. 7);

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

      function Create
        (F, L  : Index;
         C     : Component;
         Dummy : Parent) return Parent
      is
         B : Component;
      begin
         return A : Parent (F .. L) do
            Assign (B, C);
            for I in F .. L loop
               Assign (A (I), B);
               Assign (B, Create (Value (B) + 1));
            end loop;
         end return;
      end Create;

      function Equal (X, Y : Parent) return Boolean is
      begin
         if X'Length /= Y'Length then
            return False;
         else
            for I in X'Range loop
               if not Equal (X (I), Y (I - X'First + Y'First)) then
                  return False;
               end if;
            end loop;
         end if;
         return True;
      end Equal;

      function Aggr (X, Y : Component) return Parent is
      begin
         return Result : Parent (Index'First .. Index'First + 1) do
            Assign (Result (Index'First), X);
            Assign (Result (Index'First + 1), Y);
         end return;
      end Aggr;

      function Aggr (W, X, Y, Z : Component) return Parent is
      begin
         return Result : Parent (Index'First .. Index'First + 3) do
            Assign (Result (Index'First), W);
            Assign (Result (Index'First + 1), X);
            Assign (Result (Index'First + 2), Y);
            Assign (Result (Index'First + 3), Z);
         end return;
      end Aggr;

   end Pkg_P;

   procedure Assign (X : in out T; Y : T) is
   begin
      for I in X'Range loop
         Assign (X (I), Y (I));
      end loop;
   end Assign;

   procedure Assign (X : in out S; Y : S) is
   begin
      for I in X'Range loop
         Assign (X (I), Y (I));
      end loop;
   end Assign;

begin
   Test
     ("C34005R",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "ONE-DIMENSIONAL ARRAY TYPES WHOSE COMPONENT " &
      "TYPE IS A LIMITED TYPE");

   Assign (X (Ident_Int (5)), Create (2));
   Assign (X (Ident_Int (6)), Create (3));
   Assign (X (Ident_Int (7)), Create (4));

   Assign (Y (5), C2);
   Assign (Y (6), C3);
   Assign (Y (7), C4);

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   begin
      if not Equal (Create (2, 3, C4, X), Aggr (C4, C5)) then
         Failed
           ("CANNOT CREATE BASE TYPE VALUES OUTSIDE " & "OF THE SUBTYPE T");
      end if;
   exception
      when others =>
         Failed
           ("EXCEPTION RAISED WHILE CHECKING BASE TYPE " &
            "VALUES OUTSIDE OF THE SUBTYPE T");
   end;

   begin
      if not Equal (Create (2, 3, C4, Y), Aggr (C4, C5)) then
         Failed
           ("CANNOT CREATE BASE TYPE VALUES OUTSIDE " & "OF THE SUBTYPE S");
      end if;
   exception
      when others =>
         Failed
           ("EXCEPTION RAISED WHILE CHECKING BASE TYPE " &
            "VALUES OUTSIDE OF THE SUBTYPE S");
   end;

   begin
      if not Equal (X (Ident_Int (6) .. Ident_Int (7)), Aggr (C3, C4)) then
         Failed ("INCORRECT SLICE OF X (VALUE)");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED WHILE CHECKING SLICE OF X");
   end;

   begin
      if not Equal (Aggr (C3, C4), Y (Ident_Int (6) .. Ident_Int (7))) then
         Failed ("INCORRECT SLICE OF Y (VALUE)");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED WHILE CHECKING SLICE OF Y");
   end;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if T'First /= 5 or T'Last /= 7 or S'First /= 5 or S'Last /= 7 then
      Failed ("INCORRECT 'FIRST OR 'LAST");
   end if;

   begin
      Assign (X, Create (5, 7, C1, X));
      Assign (Y, Create (5, 7, C1, Y));
      if not Equal (Parent (X), Parent (Y)) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGN CALL");
   end;

   begin
      Assign (X, Aggr (C1, C2));
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "ASSIGN (X, AGGR (C1, C2))");
      if Equal (X, Aggr (C1, C2)) then  -- USE X.
         Comment ("X ALTERED -- ASSIGN (X, AGGR (C1, C2))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "ASSIGN (X, AGGR (C1, C2))");
   end;

   begin
      Assign (X, Aggr (C1, C2, C3, C4));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, AGGR (C1, C2, C3, C4))");
      if Equal (X, Aggr (C1, C2, C3, C4)) then  -- USE X.
         Comment ("X ALTERED -- " & "ASSIGN (X, AGGR (C1, C2, C3, C4))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, AGGR (C1, C2, C3, C4))");
   end;

   begin
      Assign (Y, Aggr (C1, C2));
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "ASSIGN (Y, AGGR (C1, C2))");
      if Equal (Y, Aggr (C1, C2)) then  -- USE Y.
         Comment ("Y ALTERED -- ASSIGN (Y, AGGR (C1, C2))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "ASSIGN (Y, AGGR (C1, C2))");
   end;

   begin
      Assign (Y, Aggr (C1, C2, C3, C4));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, AGGR (C1, C2, C3, C4))");
      if Equal (Y, Aggr (C1, C2, C3, C4)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "ASSIGN (Y, AGGR (C1, C2, C3, C4))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, AGGR (C1, C2, C3, C4))");
   end;

   Result;
end C34005r;
