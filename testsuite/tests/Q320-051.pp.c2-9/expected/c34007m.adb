-- C34007M.ADA

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
--     RECORD TYPE WITHOUT DISCRIMINANTS.

-- HISTORY:
--     JRK 09/29/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     BCB 03/07/90  PUT CHECK FOR 'STORAGE_SIZE IN EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34007m is

   subtype Component is Integer;

   type Designated is record
      C : Component;
      B : Boolean := True;
   end record;

   type Parent is access Designated;

   type T is new Parent;

   X : T         := new Designated'(2, False);
   K : Integer   := X'Size;
   Y : T         := new Designated'(1, True);
   W : Parent    := new Designated'(2, False);
   C : Component := 1;

   procedure A (X : Address) is
   begin
      null;
   end A;

   function Ident (X : T) return T is
   begin
      if X = null or else Equal (X.C, X.C) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return new Designated'(-1, False);
   end Ident;

begin
   Test
     ("C34007M",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "RECORD TYPE WITHOUT DISCRIMINANTS");

   if Y = null or else Y.all /= (1, True) then
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
      W := new Designated'(1, True);
   end if;
   X := T (W);
   if X = null or else X = Y or else X.all /= (1, True) then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   X := Ident (Y);
   W := Parent (X);
   if W = null or else W.all /= (1, True) or else T (W) /= Y then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

   if Ident (null) /= null or X = null then
      Failed ("INCORRECT NULL");
   end if;

   X := Ident (new Designated'(1, True));
   if X = null or else X = Y or else X.all /= (1, True) then
      Failed ("INCORRECT ALLOCATOR");
   end if;

   X := Ident (Y);
   if X.C /= 1 or X.B /= True then
      Failed ("INCORRECT SELECTION (VALUE)");
   end if;

   X.C := Ident_Int (3);
   X.B := Ident_Bool (False);
   if X /= Y or Y.all /= (3, False) then
      Failed ("INCORRECT SELECTION (ASSIGNMENT)");
   end if;

   Y.all := (1, True);
   X     := Ident (Y);
   if X.all /= (1, True) then
      Failed ("INCORRECT .ALL (VALUE)");
   end if;

   X.all := (10, False);
   if X /= Y or Y.all /= (10, False) then
      Failed ("INCORRECT .ALL (ASSIGNMENT)");
   end if;

   Y.all := (1, True);
   X     := Ident (null);
   begin
      if X.all = (0, False) then
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
   if X = null or X = new Designated or not (X = Y) then
      Failed ("INCORRECT =");
   end if;

   if X /= Y or not (X /= null) then
      Failed ("INCORRECT /=");
   end if;

   if not (X in T) then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   A (X'Address);

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
end C34007m;
