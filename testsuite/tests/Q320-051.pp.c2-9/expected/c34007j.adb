-- C34007J.ADA

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
--     (IMPLICITLY) FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE
--     IS A TASK TYPE.

-- HISTORY:
--     JRK 09/26/86  CREATED ORIGINAL TEST.
--     JLH 09/25/87  REFORMATTED HEADER.
--     BCB 09/26/88  REMOVED COMPARISION INVOLVING OBJECT SIZE.
--     BCB 03/07/90  PUT CHECK FOR 'STORAGE_SIZE IN EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with System; use System;
with Report; use Report;

procedure C34007j is

   task type Designated is
      entry E (I : in out Integer);
      entry F (1 .. 3) (I : Integer; J : out Integer);
      entry R (I : out Integer);
      entry W (I : Integer);
   end Designated;

   type Parent is access Designated;

   type T is new Parent;

   X : T;
   K : Integer := X'Size;
   Y : T;
   W : Parent;
   I : Integer := 0;
   J : Integer := 0;

   procedure A (X : Address) is
   begin
      null;
   end A;

   function V return T is
   begin
      return new Designated;
   end V;

   function Ident (X : T) return T is
   begin
      if (X = null or else X'Callable) or Ident_Bool (True) then
         return X;                          -- ALWAYS EXECUTED.
      end if;
      return new Designated;
   end Ident;

   task body Designated is
      N : Integer := 1;
   begin
      loop
         select
            accept E (I : in out Integer) do
               I := I + N;
            end E;
         or
            accept F (2) (I : Integer; J : out Integer) do
               J := I + N;
            end F;
         or
            accept R (I : out Integer) do
               I := N;
            end R;
         or
            accept W (I : Integer) do
               N := I;
            end W;
         or
            terminate;
         end select;
      end loop;
   end Designated;

begin
   Test
     ("C34007J",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
      "TASK TYPE");

   X := new Designated;
   Y := new Designated;
   W := new Designated;

   if Y = null then
      Failed ("INCORRECT INITIALIZATION - 1");
   else
      Y.W (2);
      Y.R (I);
      if I /= 2 then
         Failed ("INCORRECT INITIALIZATION - 2");
      end if;
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
      W := new Designated;
      W.W (3);
   end if;
   X := T (W);
   if X = null or X = Y then
      Failed ("INCORRECT CONVERSION FROM PARENT - 1");
   else
      I := 5;
      X.E (I);
      if I /= 8 then
         Failed ("INCORRECT CONVERSION FROM PARENT - 2");
      end if;
   end if;

   X := Ident (Y);
   W := Parent (X);
   if W = null or T (W) /= Y then
      Failed ("INCORRECT CONVERSION TO PARENT - 1");
   else
      I := 5;
      W.E (I);
      if I /= 7 then
         Failed ("INCORRECT CONVERSION TO PARENT - 2");
      end if;
   end if;

   if Ident (null) /= null or X = null then
      Failed ("INCORRECT NULL");
   end if;

   X := Ident (new Designated);
   if X = null or X = Y then
      Failed ("INCORRECT ALLOCATOR - 1");
   else
      I := 5;
      X.E (I);
      if I /= 6 then
         Failed ("INCORRECT ALLOCATOR - 2");
      end if;
   end if;

   X := Ident (Y);
   I := 5;
   X.E (I);
   if I /= 7 then
      Failed ("INCORRECT SELECTION (ENTRY)");
   end if;

   I := 5;
   X.F (Ident_Int (2)) (I, J);
   if J /= 7 then
      Failed ("INCORRECT SELECTION (FAMILY)");
   end if;

   I := 5;
   X.all.E (I);
   if I /= 7 then
      Failed ("INCORRECT .ALL");
   end if;

   X := Ident (null);
   begin
      if X.all'Callable then
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

   if not X'Callable then
      Failed ("INCORRECT OBJECT'CALLABLE");
   end if;

   if not V'Callable then
      Failed ("INCORRECT VALUE'CALLABLE");
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

   if X'Terminated then
      Failed ("INCORRECT OBJECT'TERMINATED");
   end if;

   if V'Terminated then
      Failed ("INCORRECT VALUE'TERMINATED");
   end if;

   Result;
end C34007j;
