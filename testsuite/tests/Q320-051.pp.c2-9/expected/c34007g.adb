-- C34007G.ADA

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
--     MULTI-DIMENSIONAL ARRAY TYPE.

-- HISTORY:
--     JRK 09/25/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 03/07/90  PUT CHECK FOR 'STORAGE_SIZE IN EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34007g is

   subtype Component is Integer;

   type Designated is array (Natural range <>, Natural range <>) of Component;

   subtype Subdesignated is
     Designated (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   package Pkg is

      type Parent is access Designated;

      function Create
        (F1, L1 : Natural; F2, L2 : Natural; C : Component;
         Dummy  : Parent   -- TO RESOLVE OVERLOADING.
        )
         return Parent;

   end Pkg;

   use Pkg;

   type T is
     new Parent (Ident_Int (4) .. Ident_Int (5),
        Ident_Int (6) .. Ident_Int (8));

   X : T         := new Subdesignated'(others => (others => 2));
   Y : T         := new Subdesignated'((1, 2, 3), (4, 5, 6));
   W : Parent    := new Subdesignated'(others => (others => 2));
   C : Component := 1;
   N : constant  := 2;

   procedure A (X : Address) is
   begin
      null;
   end A;

   function V return T is
   begin
      return new Subdesignated'(others => (others => C));
   end V;

   package body Pkg is

      function Create
        (F1, L1 : Natural; F2, L2 : Natural; C : Component; Dummy : Parent)
         return Parent
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

   function Ident (X : T) return T is
   begin
      if X = null or else Equal (X'Length, X'Length) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return new Subdesignated;
   end Ident;

begin
   Test
     ("C34007G",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "MULTI-DIMENSIONAL ARRAY TYPE");

   if Y = null or else Y.all /= ((1, 2, 3), (4, 5, 6)) then
      Failed ("INCORRECT INITIALIZATION");
   end if;

   X := Ident (Y);
   if X /= Y then
      Failed ("INCORRECT :=");
   end if;

   if T'(X) /= Y then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if T (X) /= Y then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   if Equal (3, 3) then
      W := new Subdesignated'((1, 2, 3), (4, 5, 6));
   end if;
   X := T (W);
   if X = null or else X = Y or else X.all /= ((1, 2, 3), (4, 5, 6)) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   X := Ident (Y);
   W := Parent (X);
   if W = null or else W.all /= ((1, 2, 3), (4, 5, 6)) or else T (W) /= Y then
      Failed ("INCORRECT CONVERSION TO PARENT - 1");
   end if;

   W := Parent (Create (6, 9, 2, 3, 4, X));
   if W = null or else W.all /= ((4, 5), (6, 7), (8, 9), (10, 11)) then
      Failed ("INCORRECT CONVERSION TO PARENT - 2");
   end if;

   if Ident (null) /= null or X = null then
      Failed ("INCORRECT NULL");
   end if;

   X := Ident (new Subdesignated'((1, 2, 3), (4, 5, 6)));
   if (X = null or else X = Y or else X.all /= ((1, 2, 3), (4, 5, 6))) or
     X = new Designated'((1, 2), (3, 4), (5, 6)) then
      Failed ("INCORRECT ALLOCATOR");
   end if;

   X := Ident (Y);
   if X.all /= ((1, 2, 3), (4, 5, 6)) or
     Create (6, 9, 2, 3, 4, X).all /= ((4, 5), (6, 7), (8, 9), (10, 11)) then
      Failed ("INCORRECT .ALL (VALUE)");
   end if;

   X.all := ((10, 11, 12), (13, 14, 15));
   if X /= Y or Y.all /= ((10, 11, 12), (13, 14, 15)) then
      Failed ("INCORRECT .ALL (ASSIGNMENT)");
   end if;

   Y.all := ((1, 2, 3), (4, 5, 6));
   begin
      Create (6, 9, 2, 3, 4, X).all :=
        ((20, 21), (22, 23), (24, 25), (26, 27));
   exception
      when others =>
         Failed ("EXCEPTION FOR .ALL (ASSIGNMENT)");
   end;

   X := Ident (null);
   begin
      if X.all = ((0, 0, 0), (0, 0, 0)) then
         Failed ("NO EXCEPTION FOR NULL.ALL - 1");
      else
         Failed ("NO EXCEPTION FOR NULL.ALL - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION FOR NULL.ALL");
   end;

   X := Ident (Y);
   if X (Ident_Int (4), Ident_Int (6)) /= 1 or
     Create (6, 9, 2, 3, 4, X) (9, 3) /= 11 then
      Failed ("INCORRECT INDEX (VALUE)");
   end if;

   X (Ident_Int (5), Ident_Int (8)) := 7;
   if X /= Y or Y.all /= ((1, 2, 3), (4, 5, 7)) then
      Failed ("INCORRECT INDEX (ASSIGNMENT)");
   end if;

   Y.all := ((1, 2, 3), (4, 5, 6));
   X     := Ident (Y);
   begin
      Create (6, 9, 2, 3, 4, X) (6, 2) := 15;
   exception
      when others =>
         Failed ("EXCEPTION FOR INDEX (ASSIGNMENT)");
   end;

   if X = null or X = new Subdesignated or not (X = Y) or
     X = Create (6, 9, 2, 3, 4, X) then
      Failed ("INCORRECT =");
   end if;

   if X /= Y or not (X /= null) or not (X /= Create (7, 9, 2, 4, 1, X)) then
      Failed ("INCORRECT /=");
   end if;

   if not (X in T) or Create (2, 3, 4, 5, 1, X) in T then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T or not (Create (7, 9, 2, 4, 1, X) not in T) then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   A (X'Address);

   if X'First /= 4 then
      Failed ("INCORRECT OBJECT'FIRST");
   end if;

   if V'First /= 4 then
      Failed ("INCORRECT VALUE'FIRST");
   end if;

   if X'First (N) /= 6 then
      Failed ("INCORRECT OBJECT'FIRST (N)");
   end if;

   if V'First (N) /= 6 then
      Failed ("INCORRECT VALUE'FIRST (N)");
   end if;

   if X'Last /= 5 then
      Failed ("INCORRECT OBJECT'LAST");
   end if;

   if V'Last /= 5 then
      Failed ("INCORRECT VALUE'LAST");
   end if;

   if X'Last (N) /= 8 then
      Failed ("INCORRECT OBJECT'LAST (N)");
   end if;

   if V'Last (N) /= 8 then
      Failed ("INCORRECT VALUE'LAST (N)");
   end if;

   if X'Length /= 2 then
      Failed ("INCORRECT OBJECT'LENGTH");
   end if;

   if V'Length /= 2 then
      Failed ("INCORRECT VALUE'LENGTH");
   end if;

   if X'Length (N) /= 3 then
      Failed ("INCORRECT OBJECT'LENGTH (N)");
   end if;

   if V'Length (N) /= 3 then
      Failed ("INCORRECT VALUE'LENGTH (N)");
   end if;

   declare
      Y : Designated (X'Range, 1 .. 3);
   begin
      if Y'First /= 4 or Y'Last /= 5 then
         Failed ("INCORRECT OBJECT'RANGE");
      end if;
   end;

   declare
      Y : Designated (V'Range, 1 .. 3);
   begin
      if Y'First /= 4 or Y'Last /= 5 then
         Failed ("INCORRECT VALUE'RANGE");
      end if;
   end;

   declare
      Y : Designated (1 .. 2, X'Range (N));
   begin
      if Y'First (N) /= 6 or Y'Last (N) /= 8 then
         Failed ("INCORRECT OBJECT'RANGE (N)");
      end if;
   end;

   declare
      Y : Designated (1 .. 2, V'Range (N));
   begin
      if Y'First (N) /= 6 or Y'Last (N) /= 8 then
         Failed ("INCORRECT VALUE'RANGE (N)");
      end if;
   end;

   if T'Size < 1 then
      Failed ("INCORRECT TYPE'SIZE");
   end if;

   if X'Size < T'Size then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   begin
      if T'Storage_Size /= Parent'Storage_Size then
         Failed
           ("COLLECTION SIZE OF DERIVED TYPE IS NOT " &
            "EQUAL TO COLLECTION SIZE OF PARENT TYPE");
      end if;
   exception
      when Program_Error =>
         Comment
           ("PROGRAM_ERROR RAISED FOR " & "UNDEFINED STORAGE_SIZE (AI-00608)");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED");
   end;

   Result;
end C34007g;
