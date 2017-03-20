-- C34006L.ADA

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
--     FOR DERIVED RECORD TYPES WITH DISCRIMINANTS AND WITH A LIMITED
--     COMPONENT TYPE:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 08/26/87  CREATED ORIGINAL TEST.
--     RLB 03/16/07  CORRECTED ILLEGAL (BY AMENDMENT 1) RETURNS.

with Report; use Report;

procedure C34006l is

   package Pkg_L is

      type Lp is limited private;

      function Create (X : Integer) return Lp;

      function Equal (X, Y : Lp) return Boolean;

      procedure Assign (X : out Lp; Y : Lp);

      C2 : constant Lp;
      C4 : constant Lp;
      C5 : constant Lp;
      C6 : constant Lp;

   private

      type Lp is new Integer;

      C2 : constant Lp := 2;
      C4 : constant Lp := 4;
      C5 : constant Lp := 5;
      C6 : constant Lp := 6;

   end Pkg_L;

   use Pkg_L;

   subtype Component is Lp;

   package Pkg_P is

      Max_Len : constant := 10;

      subtype Length is Natural range 0 .. Max_Len;

      type Parent (B : Boolean := True; L : Length := 3) is record
         I : Integer := 2;
         case B is
            when True =>
               S : String (1 .. L) := (1 .. L => 'A');
               C : Component;
            when False =>
               F : Float := 5.0;
         end case;
      end record;

      function Create
        (B : Boolean;
         L : Length;
         I : Integer;
         S : String;
         C : Component;
         F : Float;
         X : Parent  -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Equal (X, Y : Parent) return Boolean;

      function Aggr
        (B : Boolean;
         L : Length;
         I : Integer;
         S : String;
         C : Component) return Parent;

      function Aggr
        (B : Boolean;
         L : Length;
         I : Integer;
         F : Float) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   subtype Subparent is Parent (True, 3);

   type S is new Subparent;

   X : T;
   Y : S;

   package body Pkg_L is

      function Create (X : Integer) return Lp is
      begin
         return Lp (Ident_Int (X));
      end Create;

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
        (B : Boolean;
         L : Length;
         I : Integer;
         S : String;
         C : Component;
         F : Float;
         X : Parent) return Parent
      is
      begin
         return A : Parent (B, L) do
            A.I := I;
            case B is
               when True =>
                  A.S := S;
                  Assign (A.C, C);
               when False =>
                  A.F := F;
            end case;
         end return;
      end Create;

      function Equal (X, Y : Parent) return Boolean is
      begin
         if X.B /= Y.B or X.L /= Y.L or X.I /= Y.I then
            return False;
         end if;
         case X.B is
            when True =>
               return X.S = Y.S and Equal (X.C, Y.C);
            when False =>
               return X.F = Y.F;
         end case;
      end Equal;

      function Aggr
        (B : Boolean;
         L : Length;
         I : Integer;
         S : String;
         C : Component) return Parent
      is
      begin
         return Result : Parent (B, L) do
            Result.I := I;
            Result.S := S;
            Assign (Result.C, C);
         end return;
      end Aggr;

      function Aggr
        (B : Boolean;
         L : Length;
         I : Integer;
         F : Float) return Parent
      is
      begin
         return Result : Parent (B, L) do
            Result.I := I;
            Result.F := F;
         end return;
      end Aggr;

   end Pkg_P;

   procedure Assign (X : in out T; Y : T) is
   begin
      X.I := Y.I;
      X.S := Y.S;
      Assign (X.C, Y.C);
   end Assign;

   procedure Assign (X : in out S; Y : S) is
   begin
      X.I := Y.I;
      X.S := Y.S;
      Assign (X.C, Y.C);
   end Assign;

begin
   Test
     ("C34006L",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "RECORD TYPES WITH DISCRIMINANTS AND WITH A " &
      "LIMITED COMPONENT TYPE");

   Assign (X.C, Create (2));
   Assign (Y.C, C2);

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if not Equal
       (Create (False, 2, 3, "ZZ", C5, 6.0, X),
        Aggr (False, 2, 3, 6.0)) or
     not Equal
       (Create (False, 2, 3, "ZZ", C5, 6.0, Y),
        Aggr (False, 2, 3, 6.0))
   then
      Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE SUBTYPE");
   end if;

   if Create (False, 2, 3, "ZZ", C5, 6.0, X) in T or
     Create (False, 2, 3, "ZZ", C5, 6.0, Y) in S
   then
      Failed ("INCORRECT ""IN""");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if X.B /= True or X.L /= 3 or Y.B /= True or Y.L /= 3 then
      Failed ("INCORRECT SELECTION OF DISCRIMINANT VALUES");
   end if;

   if not X'Constrained or not Y'Constrained then
      Failed ("INCORRECT 'CONSTRAINED");
   end if;

   begin
      Assign (X, Aggr (True, 3, 1, "ABC", C4));
      Assign (Y, Aggr (True, 3, 1, "ABC", C4));
      if not Equal (Parent (X), Parent (Y)) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGN CALL");
   end;

   begin
      Assign (X, Aggr (False, 3, 2, 6.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, AGGR (FALSE, 3, 2, 6.0))");
      if Equal (X, Aggr (False, 3, 2, 6.0)) then  -- USE X.
         Comment ("X ALTERED -- " & "ASSIGN (X, AGGR (FALSE, 3, 2, 6.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, AGGR (FALSE, 3, 2, 6.0))");
   end;

   begin
      Assign (X, Aggr (True, 4, 2, "ZZZZ", C6));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
      if Equal (X, Aggr (True, 4, 2, "ZZZZ", C6)) then  -- USE X.
         Comment
           ("X ALTERED -- " & "ASSIGN (X, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
   end;

   begin
      Assign (Y, Aggr (False, 3, 2, 6.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, AGGR (FALSE, 3, 2, 6.0))");
      if Equal (Y, Aggr (False, 3, 2, 6.0)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "ASSIGN (Y, AGGR (FALSE, 3, 2, 6.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, AGGR (FALSE, 3, 2, 6.0))");
   end;

   begin
      Assign (Y, Aggr (True, 4, 2, "ZZZZ", C6));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
      if Equal (Y, Aggr (True, 4, 2, "ZZZZ", C6)) then  -- USE Y.
         Comment
           ("Y ALTERED -- " & "ASSIGN (Y, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, AGGR (TRUE, 4, 2, ""ZZZZ"", C6))");
   end;

   Result;
end C34006l;
