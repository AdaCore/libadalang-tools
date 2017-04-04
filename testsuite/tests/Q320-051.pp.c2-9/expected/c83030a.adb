-- C83030A.ADA

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
--     CHECK THAT WITHIN A GENERIC SUBPROGRAM BODY, NO SUBPROGRAM
--     DECLARED IN AN OUTER DECLARATIVE REGION IS HIDDEN (UNLESS THE
--     SUBPROGRAM IS A HOMOGRAPH OF THE GENERIC SUBPROGRAM).

-- HISTORY:
--     TBN 08/03/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C83030a is

   Global  : Integer := Ident_Int (Integer'First);
   Switch1 : Boolean := True;

   procedure P is
   begin
      Global := Ident_Int (1);
   end P;

   procedure P (X : Integer) is
   begin
      Global := Ident_Int (X);
   end P;

begin
   Test
     ("C83030A",
      "CHECK THAT WITHIN A GENERIC SUBPROGRAM BODY, " &
      "NO SUBPROGRAM DECLARED IN AN OUTER " &
      "DECLARATIVE REGION IS HIDDEN " &
      "(UNLESS THE SUBPROGRAM IS A HOMOGRAPH OF THE " &
      "GENERIC SUBPROGRAM)");

   One : declare
      generic
      procedure P;

      procedure P is
         A : Integer := Ident_Int (2);
      begin
         if Switch1 then
            Switch1 := False;
            P;
            if Global /= Ident_Int (3) then
               Failed ("INCORRECT VALUE FOR PROCEDURE CALL " & "- 1");
            end if;
         end if;
         P (A);
         if Global /= Ident_Int (2) then
            Failed ("INCORRECT VALUE FOR PROCEDURE CALL - 2");
         end if;
         Global := Ident_Int (3);
      end P;

      procedure New_P is new P;

   begin
      if Global /= Ident_Int (Integer'First) then
         Failed ("INCORRECT VALUE FOR START OF TEST ONE");
      end if;
      New_P;
      if Global /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR END OF TEST ONE");
      end if;
   end One;

   Two : declare
      Global : Integer := Ident_Int (Integer'First);
      Switch : Boolean := True;

      generic
         type T is (<>);
      procedure P (X : T);

      procedure P (X : T) is
         A : T := T'First;
      begin
         if Switch then
            Switch := False;
            P (X);
            if Global /= Ident_Int (2) then
               Failed ("INCORRECT VALUE FOR PROCEDURE CALL " & "- 20");
            end if;
            Global := Ident_Int (3);
         else
            Global := Ident_Int (2);
         end if;
      end P;

      procedure New_P is new P (Integer);

   begin
      if Global /= Ident_Int (Integer'First) then
         Failed ("INCORRECT VALUE FOR START OF TEST TWO");
      end if;
      New_P (1);
      if Global /= Ident_Int (3) then
         Failed ("INCORRECT VALUE FOR END OF TEST TWO");
      end if;
   end Two;

   Three : declare
      Switch : Boolean := True;

      function F return Integer is
      begin
         return Ident_Int (1);
      end F;

      function F return Boolean is
      begin
         return Ident_Bool (False);
      end F;

      function F (X : Integer) return Integer is
      begin
         return Ident_Int (X);
      end F;

   begin
      declare
         generic
         function F return Integer;

         function F return Integer is
            A : Integer := Integer'Last;
         begin
            if Switch then
               Switch := False;
               if F /= Ident_Int (3) then
                  Failed ("INCORRECT VALUE FROM FUNCTION " & "CALL - 30");
               end if;
            end if;
            if F (A) /= Ident_Int (Integer'Last) then
               Failed ("INCORRECT VALUE FROM FUNCTION CALL " & "- 31");
            end if;
            if F then
               Failed ("INCORRECT VALUE FROM FUNCTION CALL " & "- 32");
            end if;
            return Ident_Int (3);
         end F;

         function New_F is new F;

      begin
         if New_F /= Ident_Int (3) then
            Failed ("INCORRECT VALUE FOR END OF TEST THREE");
         end if;
      end;
   end Three;

   Four : declare
      Switch : Boolean := True;

      function F return Integer is
      begin
         return Ident_Int (1);
      end F;

      function F return Boolean is
      begin
         return Ident_Bool (False);
      end F;

   begin
      declare
         generic
            type T is (<>);
         function F return T;

         function F return T is
            A : T := T'Last;
         begin
            if Switch then
               Switch := False;
               if F /= T'Last then
                  Failed ("INCORRECT VALUE FROM FUNCTION " & "CALL - 40");
               end if;
               return T'First;
            else
               if F then
                  Failed ("INCORRECT VALUE FROM FUNCTION " & "CALL - 41");
               end if;
               return T'Last;
            end if;
         end F;

         function New_F is new F (Integer);

      begin
         if New_F /= Ident_Int (Integer'First) then
            Failed ("INCORRECT VALUE FOR END OF TEST FOUR");
         end if;
      end;
   end Four;

   Result;
end C83030a;
