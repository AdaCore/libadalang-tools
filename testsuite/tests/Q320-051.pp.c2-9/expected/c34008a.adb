-- C34008A.ADA

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
--     (IMPLICITLY) FOR DERIVED TASK TYPES.

-- HISTORY:
--     JRK 08/27/87  CREATED ORIGINAL TEST.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
--     DTN 11/30/95  REMOVED ATTIBUTES OF NON-OBJECTS.
--     RLB 01/25/08  REMOVED FUNCTION MADE ILLEGAL BY AMENDMENT 1.

with System; use System;
with Report; use Report;

procedure C34008a is

   package Pkg is

      task type Parent is
         entry E (I : in out Integer);
         entry F (1 .. 3) (I : Integer; J : out Integer);
         entry G;
         entry H (1 .. 3);
         entry R (I : out Integer);
         entry W (I : Integer);
      end Parent;

      function Id (X : Parent) return Integer;

   end Pkg;

   use Pkg;

   type T is new Parent;

   task type Aux;

   X      : T;
   W      : Parent;
   B      : Boolean := False;
   I      : Integer := 0;
   J      : Integer := 0;
   A1, A2 : Aux;

   procedure A (X : Address) is
   begin
      B := Ident_Bool (True);
   end A;

   package body Pkg is

      task body Parent is
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
               accept G do
                  while H (2)'Count < 2 loop
                     delay 5.0;
                  end loop;
                  accept H (2) do
                     if E'Count /= 0 or F (1)'Count /= 0 or F (2)'Count /= 0 or
                       F (3)'Count /= 0 or G'Count /= 0 or H (1)'Count /= 0 or
                       H (2)'Count /= 1 or H (3)'Count /= 0 or R'Count /= 0 or
                       W'Count /= 0 then
                        Failed ("INCORRECT 'COUNT");
                     end if;
                  end H;
                  accept H (2);
               end G;
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
      end Parent;

      function Id (X : Parent) return Integer is
         I : Integer;
      begin
         X.R (I);
         return I;
      end Id;

   end Pkg;

   task body Aux is
   begin
      X.H (2);
   end Aux;

begin
   Test
     ("C34008A",
      "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
      "ARE DECLARED (IMPLICITLY) FOR DERIVED TASK " & "TYPES");

   X.W (Ident_Int (2));
   if Id (X) /= 2 then
      Failed ("INCORRECT INITIALIZATION");
   end if;

   if Id (T'(X)) /= 2 then
      Failed ("INCORRECT QUALIFICATION");
   end if;

   if Id (T (X)) /= 2 then
      Failed ("INCORRECT SELF CONVERSION");
   end if;

   W.W (Ident_Int (3));
   if Id (T (W)) /= 3 then
      Failed ("INCORRECT CONVERSION FROM PARENT");
   end if;

   if Id (Parent (X)) /= 2 then
      Failed ("INCORRECT CONVERSION TO PARENT");
   end if;

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

   if not (X in T) then
      Failed ("INCORRECT ""IN""");
   end if;

   if X not in T then
      Failed ("INCORRECT ""NOT IN""");
   end if;

   B := False;
   A (X'Address);
   if not B then
      Failed ("INCORRECT OBJECT'ADDRESS");
   end if;

   if not X'Callable then
      Failed ("INCORRECT OBJECT'CALLABLE");
   end if;

   X.G;

   if X'Size < T'Size then
      Failed ("INCORRECT OBJECT'SIZE");
   end if;

   if T'Storage_Size < 0 then
      Failed ("INCORRECT TYPE'STORAGE_SIZE");
   end if;

   if X'Storage_Size < 0 then
      Failed ("INCORRECT OBJECT'STORAGE_SIZE");
   end if;

   if X'Terminated then
      Failed ("INCORRECT OBJECT'TERMINATED");
   end if;

   Result;
end C34008a;
