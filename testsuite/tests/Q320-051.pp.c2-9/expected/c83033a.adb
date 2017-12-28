-- C83033A.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A BLOCK NAME, A LOOP NAME,
--     OR A STATEMENT LABEL HIDES THE DECLARATION OF AN ENUMERATION
--     LITERAL OR OF A DERIVED SUBPROGRAM DECLARED BY A DERIVED TYPE
--     DEFINITION.

-- HISTORY:
--     DHH 09/21/88  CREATED ORIGINAL TEST.
--     WMC 03/25/92  REMOVED TEST REDUNDANCIES.

with Report; use Report;
procedure C83033a is

   package Base_P is
      type A is (Red, Blue, Yelo);
      function Red (T : Integer; X : A) return A;
      function Blue (T : Integer; X : A) return A;
   end Base_P;

   package body Base_P is
      function Red (T : Integer; X : A) return A is
      begin
         if Equal (T, T) then
            return X;
         else
            return Yelo;
         end if;
      end Red;

      function Blue (T : Integer; X : A) return A is
      begin
         if Equal (T, T) then
            return X;
         else
            return Yelo;
         end if;
      end Blue;

   end Base_P;
begin
   Test
     ("C83033A",
      "CHECK THAT AN IMPLICIT DECLARATION OF A BLOCK " &
      "NAME, A LOOP NAME, OR A STATEMENT LABEL HIDES " &
      "THE DECLARATION OF AN ENUMERATION LITERAL OR " &
      "OF A DERIVED SUBPROGRAM DECLARED BY A DERIVED " &
      "TYPE DEFINITION");

   B1 :
   declare
      type Stmt2 is new Base_P.A;
   begin

      declare
         C, D : Stmt2;
      begin
         C := C83033a.B1.Red (3, C83033a.B1.Red);
         D := C83033a.B1.Red;

         goto Red;              -- DEMONSTRATES USE OF STATEMENT LABEL.
         Failed ("STATEMENT LABEL - 1");

         <<Red>>
         if C /= D then
            Failed ("STATEMENT LABEL - 2");
         end if;
      end;
   end B1;

   B2 :
   declare
      type Stmt2 is new Base_P.A;
   begin

      declare
         A : Stmt2 := Blue;
         B : Stmt2 := Blue (3, Blue);
      begin

         Blue :
         for I in 1 .. 1 loop
            if A /= B then
               Failed ("LOOP NAME - 1");
            end if;
            exit Blue;                -- DEMONSTRATES USE OF LOOP LABEL.
            Failed ("LOOP NAME - 2");
         end loop Blue;
      end;
   end B2;

   B4 :
   declare
      package P is
         Global : Integer := 1;
         type Enum is (Green, Blue);
         type Priv is private;
         function Green return Priv;
      private
         type Priv is new Enum;
      end P;

      package body P is
         function Green return Priv is
         begin
            Global := Global + 1;
            return Blue;
         end Green;
      begin
         null;
      end P;
      use P;
   begin
      Green :
      declare
         Color : Priv := C83033a.B4.P.Green;
      begin
         if Green.Color /= C83033a.B4.P.Green or else Global /= 3 then
            Failed ("BLOCK NAME");
         end if;
      end Green;
   end B4;

   Result;
end C83033a;
