-- C34006F.ADA

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
--     FOR DERIVED RECORD TYPES WITH DISCRIMINANTS AND WITH NON-LIMITED
--     COMPONENT TYPES:
--     CHECK THAT ALL VALUES OF THE PARENT (BASE) TYPE ARE PRESENT FOR
--     THE DERIVED (BASE) TYPE WHEN THE DERIVED TYPE DEFINITION IS
--     CONSTRAINED.
--     CHECK THAT ANY CONSTRAINT IMPOSED ON THE PARENT SUBTYPE IS ALSO
--     IMPOSED ON THE DERIVED SUBTYPE.

-- HISTORY:
--     JRK 9/22/86  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C34006f is

   subtype Component is Integer;

   package Pkg is

      Max_Len : constant := 10;

      subtype Length is Natural range 0 .. Max_Len;

      type Parent (B : Boolean := True; L : Length := 1) is record
         I : Integer;
         case B is
            when True =>
               S : String (1 .. L);
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

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Bool (True), Ident_Int (3));

   subtype Subparent is Parent (True, 3);

   type S is new Subparent;

   X : T := (True, 3, 2, "AAA", 2);
   Y : S := (True, 3, 2, "AAA", 2);

   package body Pkg is

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
         case B is
            when True =>
               return (True, L, I, S, C);
            when False =>
               return (False, L, I, F);
         end case;
      end Create;

   end Pkg;

begin
   Test
     ("C34006F",
      "CHECK THAT ALL VALUES OF THE PARENT (BASE) " &
      "TYPE ARE PRESENT FOR THE DERIVED (BASE) TYPE " &
      "WHEN THE DERIVED TYPE DEFINITION IS " &
      "CONSTRAINED.  ALSO CHECK THAT ANY CONSTRAINT " &
      "IMPOSED ON THE PARENT SUBTYPE IS ALSO IMPOSED " &
      "ON THE DERIVED SUBTYPE.  CHECK FOR DERIVED " &
      "RECORD TYPES WITH DISCRIMINANTS AND WITH " &
      "NON-LIMITED COMPONENT TYPES");

   -- CHECK THAT BASE TYPE VALUES NOT IN THE SUBTYPE ARE PRESENT.

   begin
      if Create (False, 2, 3, "ZZ", 5, 6.0, X) /= (False, 2, 3, 6.0) or
        Create (False, 2, 3, "ZZ", 5, 6.0, Y) /= (False, 2, 3, 6.0)
      then
         Failed ("CAN'T CREATE BASE TYPE VALUES OUTSIDE THE " & "SUBTYPE");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 1");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 1");
   end;

   begin
      if Create (False, 2, 3, "ZZ", 5, 6.0, X) in T or
        Create (False, 2, 3, "ZZ", 5, 6.0, Y) in S
      then
         Failed ("INCORRECT ""IN""");
      end if;
   exception
      when Constraint_Error =>
         Failed ("CALL TO CREATE RAISED CONSTRAINT_ERROR - 2");
      when others =>
         Failed ("CALL TO CREATE RAISED EXCEPTION - 2");
   end;

   -- CHECK THE DERIVED SUBTYPE CONSTRAINT.

   if X.B /= True or X.L /= 3 or Y.B /= True or Y.L /= 3 then
      Failed ("INCORRECT SELECTION OF DISCRIMINANT VALUES");
   end if;

   if not X'Constrained or not Y'Constrained then
      Failed ("INCORRECT 'CONSTRAINED");
   end if;

   begin
      X := (True, 3, 1, "ABC", 4);
      Y := (True, 3, 1, "ABC", 4);
      if Parent (X) /= Parent (Y) then  -- USE X AND Y.
         Failed ("INCORRECT CONVERSION TO PARENT");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED BY OK ASSIGNMENT");
   end;

   begin
      X := (False, 3, 2, 6.0);
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "X := (FALSE, 3, 2, 6.0)");
      if X = (False, 3, 2, 6.0) then  -- USE X.
         Comment ("X ALTERED -- X := (FALSE, 3, 2, 6.0)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "X := (FALSE, 3, 2, 6.0)");
   end;

   begin
      X := (True, 4, 2, "ZZZZ", 6);
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "X := (TRUE, 4, 2, ""ZZZZ"", 6)");
      if X = (True, 4, 2, "ZZZZ", 6) then  -- USE X.
         Comment ("X ALTERED -- X := (TRUE, 4, 2, ""ZZZZ"", 6)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "X := (TRUE, 4, 2, ""ZZZZ"", 6)");
   end;

   begin
      Y := (False, 3, 2, 6.0);
      Failed ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := (FALSE, 3, 2, 6.0)");
      if Y = (False, 3, 2, 6.0) then  -- USE Y.
         Comment ("Y ALTERED -- Y := (FALSE, 3, 2, 6.0)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED -- " & "Y := (FALSE, 3, 2, 6.0)");
   end;

   begin
      Y := (True, 4, 2, "ZZZZ", 6);
      Failed
        ("CONSTRAINT_ERROR NOT RAISED -- " & "Y := (TRUE, 4, 2, ""ZZZZ"", 6)");
      if Y = (True, 4, 2, "ZZZZ", 6) then  -- USE Y.
         Comment ("Y ALTERED -- Y := (TRUE, 4, 2, ""ZZZZ"", 6)");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED -- " & "Y := (TRUE, 4, 2, ""ZZZZ"", 6)");
   end;

   Result;
end C34006f;
