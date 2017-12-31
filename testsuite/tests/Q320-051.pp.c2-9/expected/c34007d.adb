-- C34007D.ADA

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
--     ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS PART 1 OF 2 TESTS
--     WHICH COVER THE OBJECTIVE.  THE SECOND PART IS IN TEST C34007V.

-- HISTORY:
--     JRK 09/25/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     BCB 04/12/90  SPLIT ORIGINAL TEST INTO C34007D.ADA AND
--                   C34007V.ADA.  PUT CHECK FOR 'STORAGE_SIZE IN
--                   EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34007d is

   subtype Component is Integer;

   type Designated is array (Natural range <>) of Component;

   subtype Subdesignated is Designated (Ident_Int (5) .. Ident_Int (7));

   package Pkg is

      type Parent is access Designated;

   end Pkg;

   use Pkg;

   type T is new Parent (Ident_Int (5) .. Ident_Int (7));

   X : T         := new Subdesignated'(others => 2);
   K : Integer   := X'Size;
   Y : T         := new Subdesignated'(1, 2, 3);
   W : Parent    := new Subdesignated'(others => 2);
   C : Component := 1;
   N : constant  := 1;

   procedure A (X : Address) is
   begin
      null;
   end A;

   function V return T is
   begin
      return new Subdesignated'(others => C);
   end V;

   function Ident (X : T) return T is
   begin
      if X = null or else Equal (X'Length, X'Length) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return new Subdesignated;
   end Ident;

begin
   Test
     ("C34007D",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS " &
      "PART 1 OF 2 TESTS WHICH COVER THE OBJECTIVE.  " &
      "THE SECOND PART IS IN TEST C34007V");

   if Y = null or else Y.all /= (1, 2, 3) then
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
      W := new Subdesignated'(1, 2, 3);
   end if;
   X := T (W);
   if X = null or else X = Y or else X.all /= (1, 2, 3) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   X := Ident (Y);
   W := Parent (X);
   if W = null or else W.all /= (1, 2, 3) or else T (W) /= Y then
      Failed ("INCORRECT CONVERSION TO PARENT - 1");
   end if;

   if Ident (null) /= null or X = null then
      Failed ("INCORRECT NULL");
   end if;

   X := Ident (new Subdesignated'(1, 2, 3));
   if (X = null or else X = Y or else X.all /= (1, 2, 3)) or
     X = new Designated'(1, 2) then
      Failed ("INCORRECT ALLOCATOR");
   end if;

   X := Ident (null);
   begin
      if X.all = (0, 0, 0) then
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

   X                 := Ident (Y);
   X (Ident_Int (7)) := 4;
   if X /= Y or Y.all /= (1, 2, 4) then
      Failed ("INCORRECT INDEX (ASSIGNMENT)");
   end if;

   Y.all                              := (1, 2, 3);
   X                                  := Ident (Y);
   X (Ident_Int (5) .. Ident_Int (6)) := (4, 5);
   if X /= Y or Y.all /= (4, 5, 3) then
      Failed ("INCORRECT SLICE (ASSIGNMENT)");
   end if;

   A (X'Address);

   if X'First /= 5 then
      Failed ("INCORRECT OBJECT'FIRST");
   end if;

   if V'First /= 5 then
      Failed ("INCORRECT VALUE'FIRST");
   end if;

   if X'First (N) /= 5 then
      Failed ("INCORRECT OBJECT'FIRST (N)");
   end if;

   if V'First (N) /= 5 then
      Failed ("INCORRECT VALUE'FIRST (N)");
   end if;

   if X'Last /= 7 then
      Failed ("INCORRECT OBJECT'LAST");
   end if;

   if V'Last /= 7 then
      Failed ("INCORRECT VALUE'LAST");
   end if;

   if X'Last (N) /= 7 then
      Failed ("INCORRECT OBJECT'LAST (N)");
   end if;

   if V'Last (N) /= 7 then
      Failed ("INCORRECT VALUE'LAST (N)");
   end if;

   if X'Length /= 3 then
      Failed ("INCORRECT OBJECT'LENGTH");
   end if;

   if V'Length /= 3 then
      Failed ("INCORRECT VALUE'LENGTH");
   end if;

   if X'Length (N) /= 3 then
      Failed ("INCORRECT OBJECT'LENGTH (N)");
   end if;

   if V'Length (N) /= 3 then
      Failed ("INCORRECT VALUE'LENGTH (N)");
   end if;

   declare
      Y : Designated (X'Range);
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT OBJECT'RANGE");
      end if;
   end;

   declare
      Y : Designated (V'Range);
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT VALUE'RANGE");
      end if;
   end;

   declare
      Y : Designated (X'Range (N));
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT OBJECT'RANGE (N)");
      end if;
   end;

   declare
      Y : Designated (V'Range (N));
   begin
      if Y'First /= 5 or Y'Last /= 7 then
         Failed ("INCORRECT VALUE'RANGE (N)");
      end if;
   end;

   if T'Size < 1 then
      Failed ("INCORRECT TYPE'SIZE");
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
end C34007d;
