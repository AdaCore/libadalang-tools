-- C34009L.ADA

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
--     FOR DERIVED LIMITED PRIVATE TYPES WITH DISCRIMINANTS:

--        CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT
--        FOR THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION
--        IS CONSTRAINED.

--        CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS
--        ALSO IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 09/01/87  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C34009l is

   package Pkg is

      Max_Len : constant := 10;

      subtype Length is Natural range 0 .. Max_Len;

      type Parent (B : Boolean := True; L : Length := 3) is limited private;

      function Create
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer;
         F : Float; X : Parent  -- TO RESOLVE OVERLOADING.
         ) return Parent;

      function Con
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer)
         return Parent;

      function Con
        (B : Boolean; L : Length; I : Integer; F : Float) return Parent;

      function Equal (X, Y : Parent) return Boolean;

      procedure Assign (X : out Parent; Y : Parent);

   private

      type Parent (B : Boolean := True; L : Length := 3) is record
         I : Integer := 2;
         case B is
            when True =>
               S : String (1 .. L) := (1 .. L => 'A');
               J : Integer         := 2;
            when False =>
               F : Float := 5.0;
         end case;
      end record;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   subtype Subparent is Parent (True, 3);

   type S is new Subparent;

   X : T;
   Y : S;

   package body Pkg is

      function Create
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer;
         F : Float; X : Parent) return Parent
      is
      begin
         case B is
            when True =>
               return (True, L, I, S, J);
            when False =>
               return (False, L, I, F);
         end case;
      end Create;

      function Con
        (B : Boolean; L : Length; I : Integer; S : String; J : Integer)
         return Parent
      is
      begin
         return (True, L, I, S, J);
      end Con;

      function Con
        (B : Boolean; L : Length; I : Integer; F : Float) return Parent
      is
      begin
         return (False, L, I, F);
      end Con;

      function Equal (X, Y : Parent) return Boolean is
      begin
         return X = Y;
      end Equal;

      procedure Assign (X : out Parent; Y : Parent) is
      begin
         X := Y;
      end Assign;

   end Pkg;

begin
   Test
     ("C34009L",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "LIMITED PRIVATE TYPES WITH DISCRIMINANTS");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   if not Equal
       (Create (False, 2, 3, "ZZ", 5, 6.0, X), Con (False, 2, 3, 6.0)) or
     not Equal (Create (False, 2, 3, "ZZ", 5, 6.0, Y), Con (False, 2, 3, 6.0))
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

   if not X'Constrained or not Y'Constrained then
      Failed ("INCORRECT 'CONSTRAINED");
   end if;

   begin
      Assign (X, Con (True, 3, 1, "ABC", 4));
      Assign (Y, Con (True, 3, 1, "ABC", 4));
      if not Equal (Parent (X), Parent (Y)) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGN CALL");
   end;

   begin
      Assign (X, Con (False, 3, 2, 6.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, CON (FALSE, 3, 2, 6.0))");
      if Equal (X, Con (False, 3, 2, 6.0)) then  -- USE X.
         Comment ("X ALTERED -- " & "ASSIGN (X, CON (FALSE, 3, 2, 6.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, CON (FALSE, 3, 2, 6.0))");
   end;

   begin
      Assign (X, Con (True, 4, 2, "ZZZZ", 6));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (X, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
      if Equal (X, Con (True, 4, 2, "ZZZZ", 6)) then  -- USE X.
         Comment
           ("X ALTERED -- " & "ASSIGN (X, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (X, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
   end;

   begin
      Assign (Y, Con (False, 3, 2, 6.0));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, CON (FALSE, 3, 2, 6.0))");
      if Equal (Y, Con (False, 3, 2, 6.0)) then  -- USE Y.
         Comment ("Y ALTERED -- " & "ASSIGN (Y, CON (FALSE, 3, 2, 6.0))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, CON (FALSE, 3, 2, 6.0))");
   end;

   begin
      Assign (Y, Con (True, 4, 2, "ZZZZ", 6));
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " &
         "ASSIGN (Y, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
      if Equal (Y, Con (True, 4, 2, "ZZZZ", 6)) then  -- USE Y.
         Comment
           ("Y ALTERED -- " & "ASSIGN (Y, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " &
            "ASSIGN (Y, CON (TRUE, 4, 2, ""ZZZZ"", 6))");
   end;

   Result;
end C34009l;
