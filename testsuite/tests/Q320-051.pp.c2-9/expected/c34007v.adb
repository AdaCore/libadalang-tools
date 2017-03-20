-- C34007V.ADA

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
--     CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--     (IMPLICITLY) FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS A
--     ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS PART 2 OF 2 TESTS
--     WHICH COVER THE OBJECTIVE.  THE FIRST PART IS IN TEST C34007D.

-- HISTORY:
--     BCB 04/12/90  CREATED ORIGINAL TEST FROM SPLIT OF C34007D.ADA.
--     THS 09/18/90  REMOVED DECLARATION OF B, DELETED PROCEDURE A,
--                   AND REMOVED ALL REFERENCES TO B.

with System; use System;
with Report; use Report;

procedure C34007v is

   subtype Component is Integer;

   type Designated is array (Natural range <>) of Component;

   subtype Subdesignated is Designated (Ident_Int (5) .. Ident_Int (7));

   package Pkg is

      type Parent is access Designated;

      function Create
        (F, L  : Natural;
         C     : Component;
         Dummy : Parent   -- TO RESOLVE OVERLOADING.
         ) return Parent;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Int (5) .. Ident_Int (7));

   X : T         := new Subdesignated'(others => 2);
   K : Integer   := X'Size;
   Y : T         := new Subdesignated'(1, 2, 3);
   W : Parent    := new Subdesignated'(others => 2);
   C : Component := 1;
   N : constant  := 1;

   function V return T is
   begin
      return new Subdesignated'(others => C);
   end V;

   package body Pkg is

      function Create
        (F, L  : Natural;
         C     : Component;
         Dummy : Parent) return Parent
      is
         A : Parent    := new Designated (F .. L);
         B : Component := C;
      begin
         for I in F .. L loop
            A (I) := B;
            B     := B + 1;
         end loop;
         return A;
      end Create;

   end Pkg;

   function Ident (X : T) return T is
   begin
      if X = null or else Equal (X'Length, X'Length) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return new Subdesignated;
   end Ident;

begin
   Test
     ("C34007V",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS " &
      "PART 2 OF 2 TESTS WHICH COVER THE OBJECTIVE.  " &
      "THE FIRST PART IS IN TEST C34007V");

   W := Parent (Create (2, 3, 4, X));
   if W = null or else W.all /= (4, 5) then
      Failed ("INCORRECT CONVERSION TO PARENT - 2");
   end if;

   X := Ident (Y);
   if X.all /= (1, 2, 3) or Create (2, 3, 4, X).all /= (4, 5) then
      Failed ("INCORRECT .ALL (VALUE)");
   end if;

   X.all := (10, 11, 12);
   if X /= Y or Y.all /= (10, 11, 12) then
      Failed ("INCORRECT .ALL (ASSIGNMENT)");
   end if;

   Y.all := (1, 2, 3);
   begin
      Create (2, 3, 4, X).all := (10, 11);
   exception
      when others =>
         Failed ("EXCEPTION FOR .ALL (ASSIGNMENT)");
   end;

   X := Ident (Y);
   if X (Ident_Int (5)) /= 1 or Create (2, 3, 4, X) (3) /= 5 then
      Failed ("INCORRECT INDEX (VALUE)");
   end if;

   Y.all := (1, 2, 3);
   X     := Ident (Y);
   begin
      Create (2, 3, 4, X) (2) := 10;
   exception
      when others =>
         Failed ("EXCEPTION FOR INDEX (ASSIGNMENT)");
   end;

   if X (Ident_Int (6) .. Ident_Int (7)) /= (2, 3) or
     Create (1, 4, 4, X) (1 .. 3) /= (4, 5, 6)
   then
      Failed ("INCORRECT SLICE (VALUE)");
   end if;

   Y.all := (1, 2, 3);
   X     := Ident (Y);
   begin
      Create (1, 4, 4, X) (2 .. 4) := (10, 11, 12);
   exception
      when others =>
         Failed ("EXCEPTION FOR SLICE (ASSIGNMENT)");
   end;

   if X = null or
     X = new Subdesignated or
     not (X = Y) or
     X = Create (2, 3, 4, X)
   then
      Failed ("INCORRECT =");
   end if;

   if X /= Y or not (X /= null) or not (X /= Create (2, 3, 4, X)) then
      Failed ("INCORRECT /=");
   end if;

   if not (X in T) or Create (2, 3, 4, X) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (Create (2, 3, 4, X) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   Result;
end C34007v;
