-- C34007U.ADA

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
-- FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS A PRIVATE TYPE WITH
-- DISCRIMINANTS:

--   CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR THE
--   DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--   CONSTRAINED.

--   CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--   IMPOSED ON THE DERIVED SUBTYPE.

-- JRK 9/30/86

with Report; use Report;

procedure C34007u is

   subtype Component is Integer;

   package Pkg_D is

      subtype Length is Natural range 0 .. 10;

      type Designated (B : Boolean := True; L : Length := 1) is private;

      function Create (B : Boolean; L : Length; I : Integer; S : String;
         C               : Component; F : Float) return Designated;

   private

      type Designated (B : Boolean := True; L : Length := 1) is record
         I : Integer := 2;
         case B is
            when True =>
               S : String (1 .. L) := (1 .. L => 'A');
               C : Component       := 2;
            when False =>
               F : Float := 5.0;
         end case;
      end record;

   end Pkg_D;

   use Pkg_D;

   package Pkg_P is

      type Parent is access Designated;

      function Create (B : Boolean; L : Length; I : Integer; S : String;
         C : Component; F : Float; X : Parent  -- TO RESOLVE OVERLOADING.
         ) return Parent;

   end Pkg_P;

   use Pkg_P;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   subtype Subparent is Parent (True, 3);

   type S is new Subparent;

   X : T := new Designated (True, 3);
   Y : S := new Designated (True, 3);

   package body Pkg_D is

      function Create (B : Boolean; L : Length; I : Integer; S : String;
         C               : Component; F : Float) return Designated
      is
      begin
         case B is
            when True =>
               return (True, L, I, S, C);
            when False =>
               return (False, L, I, F);
         end case;
      end Create;

   end Pkg_D;

   package body Pkg_P is

      function Create (B : Boolean; L : Length; I : Integer; S : String;
         C               : Component; F : Float; X : Parent) return Parent
      is
      begin
         return new Designated'(Create (B, L, I, S, C, F));
      end Create;

   end Pkg_P;

begin
   Test
     ("C34007U",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "PRIVATE TYPE WITH DISCRIMINANTS");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if Create (False, 2, 3, "WW", 5, 6.0, X).all /=
     Create (False, 2, 3, "ZZ", 7, 6.0) or
     Create (False, 2, 3, "WW", 5, 6.0, Y).all /=
       Create (False, 2, 3, "ZZ", 7, 6.0)
   then
      Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE SUBTYPE");
   end if;

   if Create (False, 2, 3, "ZZ", 5, 6.0, X) in T or
     Create (False, 2, 3, "ZZ", 5, 6.0, Y) in S then
      Failed ("INCORRECT ""IN""");
   end if;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if X.B /= True or X.L /= 3 or Y.B /= True or Y.L /= 3 then
      Failed ("INCORRECT SELECTION OF DISCRIMINANT VALUES");
   end if;

   begin
      X := new Designated'(Create (True, 3, 1, "ABC", 4, 1.0));
      Y := new Designated'(Create (True, 3, 1, "ABC", 4, 1.0));
      if Parent (X) = Parent (Y) or  -- USE X AND Y.
      X.all /= Y.all then
         Failed ("INCORRECT ALLOCATOR OR CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := new Designated'(Create (False, 3, 2, "ZZZ", 5, 6.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "X := NEW DESIGNATED'" &
         "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
      if X = null or else X.all = Create (False, 3, 2, "ZZZ", 5, 6.0)
      then  -- USE X.
         Comment
           ("X ALTERED -- " & "X := NEW DESIGNATED'" &
            "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "X := NEW DESIGNATED'" &
            "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
   end;

   begin
      X := new Designated'(Create (True, 4, 2, "ZZZZ", 6, 7.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "X := NEW DESIGNATED'" &
         "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
      if X = null or else X.all = Create (True, 4, 2, "ZZZZ", 6, 7.0)
      then  -- USE X.
         Comment
           ("X ALTERED -- " & "X := NEW DESIGNATED'" &
            "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "X := NEW DESIGNATED'" &
            "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
   end;

   begin
      Y := new Designated'(Create (False, 3, 2, "ZZZ", 5, 6.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := NEW DESIGNATED'" &
         "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
      if Y = null or else Y.all = Create (False, 3, 2, "ZZZ", 5, 6.0)
      then  -- USE Y.
         Comment
           ("Y ALTERED -- " & "Y := NEW DESIGNATED'" &
            "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "Y := NEW DESIGNATED'" &
            "(CREATE (FALSE, 3, 2, ""ZZZ"", 5, 6.0))");
   end;

   begin
      Y := new Designated'(Create (True, 4, 2, "ZZZZ", 6, 7.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := NEW DESIGNATED'" &
         "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
      if Y = null or else Y.all = Create (True, 4, 2, "ZZZZ", 6, 7.0)
      then  -- USE Y.
         Comment
           ("Y ALTERED -- " & "Y := NEW DESIGNATED'" &
            "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "Y := NEW DESIGNATED'" &
            "(CREATE (TRUE, 4, 2, ""ZZZZ"", 6, 7.0))");
   end;

   Result;
end C34007u;
